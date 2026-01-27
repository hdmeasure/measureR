# =========================================
# Content Validity Server
# =========================================

server_contentval <- function(input, output, session) {
  
  library(dplyr)
  library(tidyr)
  library(DT)
  library(readxl)
  
  # =====================================
  # Intro modal (once per session)
  # =====================================
  session$onFlushed(function() {
    showModal(
      modalDialog(
        title = "Content Validity Analysis – Getting Started",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Got it!"),
        HTML("
          <b>Supported analyses:</b>
          <ul>
            <li><b>Aiken’s V</b> – ordinal (Likert-type) expert ratings</li>
            <li><b>CVR (Lawshe)</b> – dichotomous judgments</li>
            <li><b>I-CVI & S-CVI</b> – item- and scale-level indices</li>
          </ul>

          <b>Required data format:</b>
          <ul>
            <li>Wide format</li>
            <li>Rows = items</li>
            <li>Columns = experts</li>
            <li>First column = item ID</li>
          </ul>

          <i>Note:</i> Content validity analysis should be conducted
          before EFA/CFA, CTT, or IRT analysis.
        ")
      )
    )
  }, once = TRUE)
  
  # =====================================
  # Helper functions
  # =====================================
  round_numeric <- function(df, digits = 3) {
    df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
  }
  
  # ---- Aiken critical values (α = .05, one-tailed)
  aiken_critical_table <- data.frame(
    n = 3:10,
    c4 = c(0.75, 0.69, 0.64, 0.60, 0.57, 0.54, 0.52, 0.50),
    c5 = c(0.80, 0.75, 0.70, 0.67, 0.64, 0.62, 0.60, 0.58)
  )
  
  get_aiken_critical <- function(n, c) {
    row <- aiken_critical_table[aiken_critical_table$n == n, ]
    if (nrow(row) == 0) return(NA)
    if (c == 4) return(row$c4)
    if (c == 5) return(row$c5)
    NA
  }
  
  # ---- CVR critical values (Lawshe, 1975)
  cvr_critical_table <- data.frame(
    n = 3:15,
    CVR_crit = c(
      0.99, 0.75, 0.99, 0.99, 0.99,
      0.75, 0.78, 0.62, 0.59, 0.56,
      0.54, 0.51, 0.49
    )
  )
  
  get_cvr_critical <- function(n) {
    row <- cvr_critical_table[cvr_critical_table$n == n, ]
    if (nrow(row) == 0) return(NA)
    row$CVR_crit
  }
  
  # =====================================
  # Example data (display only)
  # =====================================
  output$example_table <- renderDT({
    example_df <- data.frame(
      item_id  = c("Item_1", "Item_2", "Item_3"),
      Expert_1 = c(4, 3, 4),
      Expert_2 = c(4, 4, 3),
      Expert_3 = c(3, 4, 4)
    )
    datatable(example_df, options = list(dom = "Bt"), rownames = FALSE)
  })
  
  # =====================================
  # Load data
  # =====================================
  data_cv <- reactive({
    
    if (input$data_source_cv == "upload") {
      req(input$datafile_cv)
      ext <- tools::file_ext(input$datafile_cv$name)
      if (ext == "csv") read.csv(input$datafile_cv$datapath)
      else read_excel(input$datafile_cv$datapath)
      
    } else {
      req(input$n_item, input$n_expert, input$rating_scale)
      
      scale_vals <- switch(
        input$rating_scale,
        "1–4 Likert" = 1:4,
        "1–5 Likert" = 1:5,
        "Dichotomous (0/1)" = 0:1
      )
      
      df <- data.frame(item_id = paste0("Item_", 1:input$n_item))
      for (i in 1:input$n_expert) {
        df[[paste0("Expert_", i)]] <- sample(scale_vals, input$n_item, TRUE)
      }
      df
    }
  })
  
  # =====================================
  # Validate data format
  # =====================================
  data_validation <- reactive({
    df <- data_cv()
    
    if (ncol(df) < 3)
      return("At least one item column and two expert columns are required.")
    
    if (!all(sapply(df[, -1], is.numeric)))
      return("All expert rating columns must be numeric.")
    
    NULL
  })
  
  output$data_warning <- renderUI({
    msg <- data_validation()
    if (!is.null(msg)) {
      div(
        style = "background:#f8d7da;border-left:4px solid #dc3545;padding:10px;",
        tags$b("Data format warning: "), msg
      )
    }
  })
  # =====================================
  # Data Preview
  # =====================================
  output$data_preview_cv <- renderDT({
    req(data_cv())
    datatable(
      data_cv(),
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(scrollX=TRUE, dom = 'Bt',pageLength=30,
                   buttons = list(list(extend = 'excel',text = 'Export Excel',
                                       filename = paste0('Data Content Validity'))
                   )
      )
    )
  })
  
  # =====================================
  # Wide → Long
  # =====================================
  data_long <- reactive({
    req(is.null(data_validation()))
    data_cv() %>%
      pivot_longer(-1, names_to = "expert", values_to = "score")
  })
  
  # =====================================
  # Detect data type (FINAL, single source)
  # =====================================
  data_type_info <- reactive({
    vals <- unique(data_long()$score)
    
    if (all(vals %in% c(0, 1))) {
      list(
        type = "dichotomous",
        msg  = "Dichotomous data detected. CVR is appropriate. Aiken’s V is not recommended."
      )
    } else {
      list(
        type = "ordinal",
        msg  = "Ordinal (Likert-type) data detected. Aiken’s V and CVI are appropriate."
      )
    }
  })
  
  output$data_type_message <- renderUI({
    div(
      style = "background:#fff3cd;border-left:4px solid #ffc107;padding:10px;",
      tags$b("Data type check: "), data_type_info()$msg
    )
  })
  
  # =====================================
  # Aiken’s V
  # =====================================
  aiken_result <- reactive({
    
    df <- data_long()
    lo <- min(df$score)
    hi <- max(df$score)
    
    n_expert <- length(unique(df$expert))
    c_scale  <- hi - lo + 1
    v_crit   <- get_aiken_critical(n_expert, c_scale)
    
    df %>%
      group_by(item_id) %>%
      summarise(
        Aiken_V   = mean((score - lo) / (hi - lo)),
        V_critical = v_crit,
        Decision  = case_when(
          is.na(v_crit) ~ "Not evaluated",
          Aiken_V >= v_crit ~ "Valid",
          TRUE ~ "Not valid"
        ),
        Strength = case_when(
          Aiken_V >= 0.80 ~ "Strong",
          Aiken_V >= 0.60 ~ "Moderate",
          TRUE ~ "Weak"
        ),
        .groups = "drop"
      )
  })
  
  output$aiken_table <- renderDT({
    datatable(
      round_numeric(aiken_result(), 3),
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(scrollX=TRUE, dom = 'Bt',pageLength=30,
                   buttons = list(list(extend = 'excel',text = 'Export Excel',
                                       filename = paste0('AIKEN'))
                   )
      )
    ) %>%
      formatStyle(
        "Aiken_V",
        backgroundColor = styleInterval(
          c(0.6, 0.8),
          c("#f8d7da", "#fff3cd", "#d4edda")
        )
      )
  })
  
  output$aiken_interpretation <- renderUI({
    if (data_type_info()$type == "dichotomous") {
      tags$div(
        class = "badge-warning",
        tags$i(class="fa-solid fa-triangle-exclamation", style="margin-right:6px;"),
        tags$b("Warning: "), tags$br(),
        "Aiken’s V is not appropriate for dichotomous data. ",
        "Results are shown for descriptive purposes only."
      )
    } else {
      tags$div(
        class = "badge-info",
        tags$i(class="fa-solid fa-circle-info", style="margin-right:6px;"),
        tags$b("Interpretation: "),tags$br(),
        "Item validity is determined by comparing the observed Aiken’s V ",
        "with the critical value proposed by Aiken (1985), which depends on ",
        "the number of experts and rating categories. ",
        tags$br(), tags$br(),
        tags$b("Strength labels "), 
        "describe the level of expert agreement and are descriptive rather ",
        "than inferential."
      )
    }
  })
  
  
  # =====================================
  # CVR (Lawshe)
  # =====================================
  cvr_result <- reactive({
    
    df <- data_long()
    N <- length(unique(df$expert))
    ne_val <- max(df$score)
    
    cvr_crit <- get_cvr_critical(N)
    
    df %>%
      group_by(item_id) %>%
      summarise(
        CVR = (sum(score == ne_val) - N / 2) / (N / 2),
        CVR_critical = cvr_crit,
        Decision = case_when(
          is.na(cvr_crit) ~ "Not evaluated",
          CVR >= cvr_crit ~ "Valid",
          TRUE ~ "Not valid"
        ),
        .groups = "drop"
      )
  })
  
  output$cvr_table <- renderDT({
    df <- round_numeric(cvr_result(), 3)
    
    datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(scrollX=TRUE, dom = 'Bt',pageLength=30,
                   buttons = list(list(extend = 'excel',text = 'Export Excel',
                                       filename = paste0('CVR'))
                   )
      )
    ) %>%
      formatStyle(
        "CVR",
        backgroundColor = styleInterval(
          df$CVR_critical[1],
          c("#f8d7da", "#d4edda")
        )
      )
  })
  
  output$cvr_interpretation <- renderUI({
    if (data_type_info()$type == "ordinal") {
      tags$div(
        class = "badge-warning",
        tags$i(class="fa-solid fa-triangle-exclamation", style="margin-right:6px;"),
        tags$b("Warning: "),tags$br(),
        "CVR is designed for dichotomous judgments. ",
        "Using ordinal ratings may lead to misleading conclusions."
      )
    } else {
      tags$div(
        class = "badge-info",
        tags$i(class="fa-solid fa-circle-info", style="margin-right:6px;"),
        tags$b("Interpretation (CVR): "),tags$br(),
        "Item validity is determined by comparing the observed CVR value ",
        "with the critical value proposed by Lawshe (1975), which depends ",
        "on the number of experts."
      )
    }
  })
  
  
  # =====================================
  # I-CVI (Lynn)
  # =====================================
  icvi_result <- reactive({
    
    df <- data_long()
    N <- length(unique(df$expert))
    max_val <- max(df$score)
    
    icvi_cut <- ifelse(N <= 5, 1.00, 0.78)
    
    df %>%
      group_by(item_id) %>%
      summarise(
        I_CVI = mean(score == max_val),
        I_CVI_cutoff = icvi_cut,
        Decision = case_when(
          I_CVI >= icvi_cut ~ "Valid",
          TRUE ~ "Not valid"
        ),
        .groups = "drop"
      )
  })
  
  # =====================================
  # S-CVI
  # =====================================
  scvi_result <- reactive({
    icvi <- icvi_result()
    data.frame(
      S_CVI_Ave = mean(icvi$I_CVI),
      S_CVI_UA  = mean(icvi$I_CVI == 1)
    )
  })
  
  output$icvi_table <- renderDT({
    df <- round_numeric(icvi_result(), 3)
    
    datatable(
      df,
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(scrollX=TRUE, dom = 'Bt',pageLength=30,
                   buttons = list(list(extend = 'excel',text = 'Export Excel',
                                       filename = paste0('ICVI'))
                   )
      )
    ) %>%
      formatStyle(
        "I_CVI",
        backgroundColor = styleInterval(
          df$I_CVI_cutoff[1],
          c("#f8d7da", "#d4edda")
        )
      )
  })
  
  output$scvi_table <- renderDT({
    datatable(
      round_numeric(scvi_result(), 3),
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(scrollX=TRUE, dom = 'Bt',pageLength=30,
                   buttons = list(list(extend = 'excel',text = 'Export Excel',
                                       filename = paste0('SCVI'))
                   )
      )
    )
  })
  
  output$cvi_interpretation <- renderUI({
    tags$div(
      class = "badge-info",
      tags$i(class="fa-solid fa-circle-info", style="margin-right:6px;"),
      tags$b("Interpretation (CVI): "),tags$br(),
      "I-CVI represents the proportion of experts who rated an item as relevant. ",
      "For six or more experts, I-CVI ≥ 0.78 is considered acceptable, while for ",
      "five or fewer experts, universal agreement (I-CVI = 1.00) is required.",
      tags$br(), tags$br(),
      "S-CVI/Ave ≥ 0.90 indicates excellent scale-level content validity."
    )
  })
  
  
}
