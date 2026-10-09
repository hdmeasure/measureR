# ==== ctt_helpers.R ====
# Helper functions for the CTT module (interpretation rules, item evaluation, UI cards).
#
# Interpretation rules and their sources:
#  * Item discrimination (item-total correlation), Ebel & Frisbie (1991):
#      >= .40 very good | .30-.39 good | .20-.29 marginal (revise) | < .20 poor (eliminate / revise)
#      A negative value suggests a keying error or a faulty item.
#  * Item difficulty (proportion correct, p), Allen & Yen (1979) and
#    Crocker & Algina (1986): .30-.70 moderate (best for discrimination);
#      < .30 difficult; > .70 easy.
#    For polytomous items p is the rescaled mean: (mean - min) / (max - min).
#  * Cronbach's alpha, George & Mallery (2003), consistent with Nunnally & Bernstein (1994):
#      >= .90 excellent | .80-.89 good | .70-.79 acceptable | .60-.69 questionable |
#      .50-.59 poor | < .50 unacceptable.
#  * Alpha-if-item-deleted: an item is flagged when removing it would raise alpha.
#  * Distractors, Haladyna & Downing (1993): non-functioning when chosen by < 5% of
#    examinees; a distractor with a positive item-total correlation also needs review.

ctt_alpha_label <- function(alpha) {
  if (is.na(alpha)) return(list(label = "Not available", color = "#6c757d"))
  if (alpha >= 0.90) list(label = "Excellent", color = "#198754")
  else if (alpha >= 0.80) list(label = "Good", color = "#2e9e4f")
  else if (alpha >= 0.70) list(label = "Acceptable", color = "#e0a800")
  else if (alpha >= 0.60) list(label = "Questionable", color = "#fd7e14")
  else if (alpha >= 0.50) list(label = "Poor", color = "#dc3545")
  else list(label = "Unacceptable", color = "#a71d2a")
}

ctt_disc_label <- function(r) {
  out <- rep(NA_character_, length(r))
  out[!is.na(r) & r >= 0.40] <- "Very good"
  out[!is.na(r) & r >= 0.30 & r < 0.40] <- "Good"
  out[!is.na(r) & r >= 0.20 & r < 0.30] <- "Marginal"
  out[!is.na(r) & r >= 0 & r < 0.20] <- "Poor"
  out[!is.na(r) & r < 0] <- "Negative"
  out
}

ctt_diff_label <- function(p) {
  out <- rep(NA_character_, length(p))
  out[!is.na(p) & p < 0.30] <- "Difficult"
  out[!is.na(p) & p >= 0.30 & p <= 0.70] <- "Moderate"
  out[!is.na(p) & p > 0.70] <- "Easy"
  out
}

# Recommendation from discrimination (primary criterion)
ctt_action <- function(disc_label) {
  ifelse(disc_label %in% c("Very good", "Good"), "Retain",
         ifelse(disc_label == "Marginal", "Revise",
                ifelse(disc_label %in% c("Poor", "Negative"), "Eliminate / revise", NA_character_)))
}

# Build the evaluated item table.
#   report : ia$itemReport from CTT::itemAnalysis
#   scored : scored item matrix / data frame
#   alpha  : scale alpha
ctt_item_eval <- function(report, scored, alpha) {
  scored <- as.data.frame(scored)
  mn <- vapply(scored, function(x) suppressWarnings(min(x, na.rm = TRUE)), numeric(1))
  mx <- vapply(scored, function(x) suppressWarnings(max(x, na.rm = TRUE)), numeric(1))
  rng <- ifelse(mx > mn, mx - mn, NA_real_)
  p <- (report$itemMean - mn) / rng
  disc <- report$pBis
  dl <- ctt_disc_label(disc)
  data.frame(
    Item = colnames(scored),
    Mean = report$itemMean,
    Difficulty = p,
    Difficulty_Level = ctt_diff_label(p),
    Discrimination = disc,
    Discrimination_Level = dl,
    Biserial = report$bis,
    Alpha_if_Deleted = report$alphaIfDeleted,
    Raises_Alpha = !is.na(report$alphaIfDeleted) & report$alphaIfDeleted > alpha,
    Recommendation = ctt_action(dl),
    stringsAsFactors = FALSE
  )
}

# Summary stat (HTML). `tone` colours only the small caption (interpretation).
ctt_card <- function(label, value, sub = NULL, tone = NULL, icon_name = NULL) {
  div(
    class = "ctt-card",
    div(class = "ctt-card-label", label),
    div(class = "ctt-card-value", value),
    if (!is.null(sub))
      div(class = "ctt-card-sub",
          if (!is.null(tone)) span(class = "ctt-dot", style = paste0("background:", tone, ";")),
          sub)
  )
}

# Reference-based interpretation guide shown in the UI.
ctt_guide_ui <- function() {
  tags$details(
    class = "ctt-guide",
    tags$summary(icon("book-open"), " Interpretation guide and cut-offs"),
    fluidRow(
      column(4,
        tags$b("Discrimination (item-total correlation)"),
        tags$table(class = "ctt-legend",
          tags$tr(tags$td(class = "lg-good", "≥ .40"), tags$td("Very good")),
          tags$tr(tags$td(class = "lg-good", ".30 – .39"), tags$td("Good")),
          tags$tr(tags$td(class = "lg-warn", ".20 – .29"), tags$td("Marginal – revise")),
          tags$tr(tags$td(class = "lg-bad", "< .20"), tags$td("Poor – eliminate / revise")),
          tags$tr(tags$td(class = "lg-bad", "< 0"), tags$td("Negative – check the key"))
        ),
        tags$small("Ebel & Frisbie (1991)")
      ),
      column(4,
        tags$b("Difficulty (p)"),
        tags$table(class = "ctt-legend",
          tags$tr(tags$td(class = "lg-warn", "< .30"), tags$td("Difficult")),
          tags$tr(tags$td(class = "lg-good", ".30 – .70"), tags$td("Moderate")),
          tags$tr(tags$td(class = "lg-warn", "> .70"), tags$td("Easy"))
        ),
        tags$small("Allen & Yen (1979); Crocker & Algina (1986). For polytomous items, p = (mean − min) / (max − min).")
      ),
      column(4,
        tags$b("Reliability (Cronbach's α)"),
        tags$table(class = "ctt-legend",
          tags$tr(tags$td(class = "lg-good", "≥ .90"), tags$td("Excellent")),
          tags$tr(tags$td(class = "lg-good", ".80 – .89"), tags$td("Good")),
          tags$tr(tags$td(class = "lg-warn", ".70 – .79"), tags$td("Acceptable")),
          tags$tr(tags$td(class = "lg-warn", ".60 – .69"), tags$td("Questionable")),
          tags$tr(tags$td(class = "lg-bad", "< .60"), tags$td("Poor / unacceptable"))
        ),
        tags$small("George & Mallery (2003); Nunnally & Bernstein (1994)")
      )
    ),
    tags$p(class = "ctt-guide-note",
      "Recommendation is based on discrimination. A red 'Alpha if Deleted' cell marks an item whose removal would increase Cronbach's α. ",
      "For distractors, an option chosen by < 5% of examinees is non-functioning (Haladyna & Downing, 1993), ",
      "and an option with a positive item-total correlation needs review.")
  )
}

# Person-level scores with SEM-based confidence band and Kelley's true-score estimate.
#   observed band : X +/- z * SEM                      (Crocker & Algina, 1986)
#   Kelley T-hat  : alpha * (X - M) + M                (Kelley, 1923)
#   band for T-hat: T-hat +/- z * SD * sqrt(alpha * (1 - alpha))
# `r` is the CTT result list (needs alpha, SEM, scaleMean, scaleSD).
ctt_person_scores <- function(score, r, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  that <- r$alpha * (score - r$scaleMean) + r$scaleMean
  se_est <- r$scaleSD * sqrt(max(r$alpha * (1 - r$alpha), 0))
  data.frame(
    Score = score,
    SEM = r$SEM,
    Lower = score - z * r$SEM,
    Upper = score + z * r$SEM,
    True_Score = that,
    True_Lower = that - z * se_est,
    True_Upper = that + z * se_est,
    stringsAsFactors = FALSE
  )
}

# Built-in polytomous example: 200 examinees x 20 items scored 1-4 (e.g. rating-scale or
# partial-credit items), generated from a graded response model with a fixed seed so the
# example is reproducible. Item discriminations range from weak (0.5) to strong (2.0),
# so the analysis shows good and weak items. The caller's random seed is left untouched.
ctt_sim_poly <- function(n = 200, k = 20, seed = 2026) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(seed)
  theta <- rnorm(n)
  a <- round(seq(0.5, 2.0, length.out = k)[sample(k)], 2)
  b <- rnorm(k, 0, 0.5)
  steps <- c(-1.2, 0, 1.2)
  out <- matrix(NA_integer_, n, k)
  for (j in seq_len(k)) {
    # cumulative probabilities P(X >= c), c = 2..4
    cum <- sapply(steps, function(s) plogis(a[j] * (theta - (b[j] + s))))
    u <- runif(n)
    out[, j] <- 1L + (u < cum[, 1]) + (u < cum[, 2]) + (u < cum[, 3])
  }
  out <- as.data.frame(out)
  colnames(out) <- paste0("i", seq_len(k))
  out
}

# Explanation of how the CTT analysis differs by data type.
ctt_type_guide_ui <- function() {
  row <- function(...) tags$tr(lapply(list(...), tags$td))
  tagList(
    tags$table(
      class = "table table-condensed ctt-compare",
      tags$thead(tags$tr(tags$th(""), tags$th("Dichotomous (0/1)"),
                         tags$th("Polytomous (e.g. 1–4)"), tags$th("Response with Key (A–D)"))),
      tags$tbody(
        row(tags$b("Input"), "Scored 0/1", "Ordered scores (rating scale, partial credit)",
            "Raw options plus an answer-key row"),
        row(tags$b("Scoring"), "Used as is", "Used as is; categories must be ordinal and in the same direction",
            "Each response is compared with the key: 1 if equal to the key, otherwise 0; then analysed as dichotomous"),
        row(tags$b("Difficulty"), "p = proportion correct (item mean)",
            "Rescaled mean p = (mean − min) / (max − min); read as 'ease' or endorsement level, not proportion correct",
            "p = proportion choosing the key"),
        row(tags$b("Discrimination"), "Item-total correlation (point-biserial); biserial also reported",
            "Item-total Pearson correlation; the biserial coefficient is not reported because it assumes a 0/1 item",
            "As dichotomous, plus option-total correlations in the distractor table"),
        row(tags$b("Distractor analysis"), "Not available", "Not available (no right/wrong options)",
            "Available: option frequencies, option-total correlation, upper/lower group choice"),
        row(tags$b("Total score"), "Number correct", "Sum of ratings", "Number correct"),
        row(tags$b("Reliability"), "Cronbach's α (equal to KR-20 for 0/1 items)", "Cronbach's α",
            "Cronbach's α on the scored data"),
        row(tags$b("Typical cut-offs"), "Ebel & Frisbie (1991); p .30–.70",
            "Item-total r ≥ .30 is commonly used (Nunnally & Bernstein, 1994); p cut-offs are only a rough guide",
            "As dichotomous; options chosen by < 5% are non-functioning (Haladyna & Downing, 1993)")
      )
    ),
    tags$p(class = "ctt-note",
      "Important: do not recode multiple-choice letters (A, B, C, D) as 1–4 and analyse them as polytomous. ",
      "The letters have no order, so α and item-total correlations are meaningless. ",
      "Use Response with Key instead.")
  )
}

# ---------------------------------------------------------------------------
# Number formatting (decimal separator and digits are user settings)
# ---------------------------------------------------------------------------
ctt_fmt <- function(x, digits = 3, dec = ".") {
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- formatC(x[ok], format = "f", digits = digits, decimal.mark = dec)
  out[!ok] <- "-"
  out
}

# Round numeric columns of a data frame (for display / export)
ctt_round_df <- function(df, digits = 3) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, digits)
  df
}

# Read csv/xlsx; semicolon-delimited csv files (decimal comma) are detected automatically.
ctt_read_table <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    first <- readLines(path, n = 1, warn = FALSE)
    if (length(first) && lengths(regmatches(first, gregexpr(";", first))) >
        lengths(regmatches(first, gregexpr(",", first)))) {
      utils::read.csv2(path, stringsAsFactors = FALSE)
    } else {
      utils::read.csv(path, stringsAsFactors = FALSE)
    }
  } else if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path))
  } else {
    stop("Invalid file format: use csv, xlsx or xls")
  }
}

# Odd-even split-half reliability with Spearman-Brown correction
ctt_split_half <- function(scored) {
  scored <- as.data.frame(scored)
  if (ncol(scored) < 2) return(list(r = NA_real_, sb = NA_real_))
  odd <- rowSums(scored[, seq(1, ncol(scored), by = 2), drop = FALSE], na.rm = TRUE)
  even <- rowSums(scored[, seq(2, ncol(scored), by = 2), drop = FALSE], na.rm = TRUE)
  r <- stats::cor(odd, even, use = "pairwise.complete.obs")
  list(r = r, sb = 2 * r / (1 + r))
}

# Descriptive statistics of the total score
ctt_descriptives <- function(r) {
  x <- r$score
  data.frame(
    Statistic = c("N", "Mean", "Median", "Std. Deviation", "Min", "Max",
                  "Skewness", "Kurtosis", "SEM", "Cronbach's alpha"),
    Value = c(
      length(stats::na.omit(x)), mean(x, na.rm = TRUE), stats::median(x, na.rm = TRUE),
      stats::sd(x, na.rm = TRUE), min(x, na.rm = TRUE), max(x, na.rm = TRUE),
      suppressWarnings(psych::skew(x, na.rm = TRUE)),
      suppressWarnings(psych::kurtosi(x, na.rm = TRUE)),
      r$SEM, r$alpha
    ),
    stringsAsFactors = FALSE
  )
}

# Distractor issues for one item table (from CTT::distractorAnalysis)
ctt_distractor_issues <- function(df) {
  dis <- df[df$correct != "*", , drop = FALSE]
  list(
    low = rownames(dis)[dis$rspP < 0.05],
    positive = rownames(dis)[!is.na(dis$pBis) & dis$pBis > 0]
  )
}
