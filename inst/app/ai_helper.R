# ai_helper.R
# Helper function to call AI providers (Gemini, OpenAI, Groq, OpenRouter, Anthropic)

# Define providers and their default models
ai_providers <- function() {
  list(
    gemini = list(label = "Google Gemini", default_model = "gemini-3.5-flash"),
    openai = list(label = "OpenAI", default_model = "gpt-4o-mini", base_url = "https://api.openai.com/v1/chat/completions"),
    groq = list(label = "Groq", default_model = "llama-3.3-70b-versatile", base_url = "https://api.groq.com/openai/v1/chat/completions"),
    openrouter = list(label = "OpenRouter", default_model = "meta-llama/llama-3.3-70b-instruct", base_url = "https://openrouter.ai/api/v1/chat/completions"),
    anthropic = list(label = "Anthropic Claude", default_model = "claude-3-5-haiku-20241022")
  )
}

ask_ai <- function(prompt, provider = "gemini", model = "", api_key = "", system_prompt = NULL) {
  if (is.null(api_key) || api_key == "") {
    return("Error: API Key is missing. Please set it in the Settings on the Homepage.")
  }

  if (is.null(model) || model == "") {
    model <- ai_providers()[[provider]]$default_model
  }

  # Format messages
  base_system_prompt <- paste0(
    "You are an assistant for measureR, a psychometrics and educational measurement R package. You MUST strictly limit your responses to the context of data analysis, statistics, psychometrics, and educational measurement. If the user asks about ANY other topic, you MUST politely refuse to answer and state that you are an AI Assistant exclusively for measureR. IMPORTANT: NEVER use LaTeX math symbols (like $\\chi^2$, $p < .001$, etc). Write all numbers, formulas, and statistical symbols in plain text only (e.g. 'Chi-square(300) = 18146.07, p < .001'). Do not use dollar signs for math. Do NOT use markdown formatting such as asterisks (** or *) for bolding or italics in your response.\n\n",
    "CRITICAL GUIDELINES FOR EVALUATING CFA AND SEM MODELS:\n",
    "When interpreting Confirmatory Factor Analysis (CFA) or Structural Equation Modeling (SEM) results, you MUST use the following established literature rules of thumb:\n",
    "1. Fit Indices (Hu & Bentler, 1999; Schreiber et al., 2006; Thakkar, 2020):\n",
    "   - RMSEA: <= .06 indicates close fit, .06 to .08 indicates fair/mediocre fit.\n",
    "   - SRMR: <= .08 indicates good fit.\n",
    "   - CFI and TLI: >= .95 indicates good fit, >= .90 indicates acceptable fit.\n",
    "   - CMIN/DF (Chi-square/df): < 3 indicates good fit.\n",
    "   - Note: Be mindful that exact cutoffs can be dynamic depending on model complexity and sample size.\n",
    "2. Estimation for Ordinal Data (DiStefano & Morgan, 2014):\n",
    "   - Diagonal Weighted Least Squares (DWLS) is highly recommended for ordinal data (e.g., Likert scales), especially when data is non-normal or has few categories.\n",
    "3. Construct Validity and Reliability (Hair et al., 2017):\n",
    "   - Outer factor loadings should ideally be > 0.708.\n",
    "   - Average Variance Extracted (AVE) should be > 0.50 for convergent validity.\n",
    "   - Composite Reliability (CR) should be > 0.70.\n",
    "4. Local Fit (Raykov et al., 2013):\n",
    "   - Remind the user to consider individual case residuals for assessing local fit rather than relying solely on overall global fit statistics.\n",
    "5. Model Modification and Comparison:\n",
    "   - If the results include model modifications or multiple models (e.g., Original vs Modified), you MUST compare their fit indices comprehensively.\n",
    "   - Explain the improvements in fit and interpret the modification indices logically based on theoretical justification, adhering to CFA/SEM reporting guidelines."
  )
  if (is.null(system_prompt) || !nzchar(system_prompt)) {
    system_prompt <- base_system_prompt
  } else {
    system_prompt <- paste(base_system_prompt, system_prompt, sep = "\n")
  }

  messages <- list(list(role = "user", content = prompt))

  reply <- tryCatch(
    {
      # Select provider
      if (provider %in% c("openai", "groq", "openrouter")) {
        url <- ai_providers()[[provider]]$base_url
        ai_call_openai_compat(url, api_key, model, messages, system_prompt)
      } else if (provider == "gemini") {
        ai_call_gemini(api_key, model, messages, system_prompt)
      } else if (provider == "anthropic") {
        ai_call_anthropic(api_key, model, messages, system_prompt)
      } else {
        paste("Error: Unknown provider", provider)
      }
    },
    error = function(e) {
      paste("Error calling AI API:", e$message)
    }
  )

  return(reply)
}

ai_call_openai_compat <- function(url, key, model, messages, system_prompt) {
  msgs <- lapply(messages, function(m) list(role = m$role, content = m$content))
  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    msgs <- c(list(list(role = "system", content = system_prompt)), msgs)
  }

  body <- list(model = model, messages = msgs)
  res <- httr::POST(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", key)),
    httr::content_type_json(),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::timeout(180)
  )

  if (httr::status_code(res) != 200) {
    err_msg <- httr::content(res, "text", encoding = "UTF-8")
    stop(paste("HTTP", httr::status_code(res), "-", err_msg))
  }

  out <- httr::content(res, as = "parsed", type = "application/json")
  return(out$choices[[1]]$message$content)
}

ai_call_gemini <- function(key, model, messages, system_prompt) {
  url <- sprintf(
    "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s",
    model, key
  )
  contents <- lapply(messages, function(m) {
    list(
      role = if (identical(m$role, "assistant")) "model" else "user",
      parts = list(list(text = m$content))
    )
  })
  payload <- list(contents = contents)
  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    payload$systemInstruction <- list(parts = list(list(text = system_prompt)))
  }

  res <- httr::POST(
    url = url,
    httr::content_type_json(),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    httr::timeout(180)
  )

  if (httr::status_code(res) != 200) {
    err_msg <- httr::content(res, "text", encoding = "UTF-8")
    stop(paste("HTTP", httr::status_code(res), "-", err_msg))
  }

  out <- httr::content(res, as = "parsed", type = "application/json")
  parts <- out$candidates[[1]]$content$parts
  return(paste(vapply(parts, function(p) p$text, character(1)), collapse = ""))
}

ai_call_anthropic <- function(key, model, messages, system_prompt) {
  msgs <- lapply(messages, function(m) list(role = m$role, content = m$content))
  payload <- list(model = model, max_tokens = 4096, messages = msgs)
  if (!is.null(system_prompt) && nzchar(system_prompt)) payload$system <- system_prompt

  res <- httr::POST(
    url = "https://api.anthropic.com/v1/messages",
    httr::add_headers(
      `x-api-key` = key,
      `anthropic-version` = "2023-06-01"
    ),
    httr::content_type_json(),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    httr::timeout(180)
  )

  if (httr::status_code(res) != 200) {
    err_msg <- httr::content(res, "text", encoding = "UTF-8")
    stop(paste("HTTP", httr::status_code(res), "-", err_msg))
  }

  out <- httr::content(res, as = "parsed", type = "application/json")
  return(paste(
    vapply(out$content, function(b) if (identical(b$type, "text")) b$text else "", character(1)),
    collapse = ""
  ))
}
