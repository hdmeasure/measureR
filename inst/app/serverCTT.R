server_ctt <- function(input, output, session, ai_context, console_context) {
    library(CTT)
    library(dplyr)
    library(DT)
    library(readxl)
    library(ggplot2)
    
    # =====================================================
    # DISPLAY SETTINGS (decimal separator, digits)
    # =====================================================
    dec <- reactive({ if (is.null(input$ctt_dec_sep)) "." else input$ctt_dec_sep })
    nd  <- reactive({ if (is.null(input$ctt_digits)) 3L else as.integer(input$ctt_digits) })
    fmt <- function(x, digits = nd()) ctt_fmt(x, digits, dec())
    # Base/ggplot axis labels follow the chosen decimal mark
    ax  <- function(x) format(x, decimal.mark = dec())
    dt_round <- function(dt, cols, digits = nd())
      DT::formatRound(dt, cols, digits = digits, dec.mark = dec(), mark = "")
    
    data_label <- reactive({
      switch(input$data_source_ctt,
             upload_scored = "Uploaded scored data",
             upload_respkey = "Uploaded response data with key",
             diko = "Built-in dichotomous data (CTT package)",
             poli = "Built-in polytomous data (simulated, 1-4)",
             respkey = "Built-in response data with key (CTT package)")
    })
    
    # =====================================================
    # LOAD RAW DATA
    # =====================================================
    raw_data <- reactive({
      
      if (input$data_source_ctt == "diko") {
        data("CTTdata", package="CTT")
        data("CTTkey", package="CTT")
        df <- as.data.frame(CTT::score(items=CTTdata, key=CTTkey, ID=NA, output.scored=TRUE)$scored) %>% 
          tibble::rownames_to_column("row_ID")
        rownames(df) <- NULL
        return(df)
      }
      
      if (input$data_source_ctt == "poli") {
        df <- ctt_sim_poly() %>% 
          tibble::rownames_to_column("row_ID")
        rownames(df) <- NULL
        return(df)
        }
      
      if (input$data_source_ctt == "respkey") {
        data("CTTdata", package="CTT")
        data("CTTkey", package="CTT")
        df <- CTTdata
        rowNames <- rownames(df)
        df <- rbind(CTTkey, df)
        df$row_ID = c("KEY",rowNames)
        rownames(df) <- NULL
        return(df)
      }
      
      if (input$data_source_ctt %in% c("upload_scored","upload_respkey")) {
        req(input$datafile_ctt)
        df <- ctt_read_table(input$datafile_ctt$datapath, input$datafile_ctt$name)
        df <- df %>% tibble::rownames_to_column("row_ID")
        rownames(df) <- NULL
        return(df)
        
      }
    })
    

    # =====================================================
    # ITEM SELECTION
    # =====================================================
    output$item_select_ui_ctt <- renderUI({
      req(raw_data())
      selectInput(
        "items_ctt",
        "Select items:",
        choices = setdiff(names(raw_data()), "row_ID"),
        selected = setdiff(names(raw_data()), "row_ID"),
        multiple = TRUE
      )
    })

    is_response <- reactive(input$data_source_ctt %in% c("respkey","upload_respkey"))

    # =====================================================
    # DATA TYPE BADGE & SUMMARY CARDS
    # =====================================================
    output$data_type_badge <- renderUI({
      txt <- switch(
        input$data_source_ctt,
        "diko" = , "upload_scored" =
          "Scored data: 0/1 items give difficulty p and point-biserial / biserial discrimination; ordered scores give a rescaled mean and the item-total correlation.",
        "poli" =
          "Simulated polytomous example: 200 examinees \u00d7 20 items scored 1\u20134 (graded response model). Analysed as ordered scores.",
        "The first row is the key. Responses are scored 1/0 against the key and analysed as dichotomous, with distractor analysis."
      )
      div(class = "ctt-flag ctt-flag-ok", txt, " ",
          tags$span(class = "ctt-note", "See 'Data format & types' below."))
    })
    
    output$data_cards_ctt <- renderUI({
      req(raw_data(), input$items_ctt)
      df <- raw_data()[, input$items_ctt, drop = FALSE]
      if (is_response()) df <- df[-1, , drop = FALSE]
      n_miss <- sum(is.na(df) | df == "")
      pct <- 100 * n_miss / max(1, nrow(df) * ncol(df))
      div(
        class = "ctt-cards",
        ctt_card("Examinees", format(nrow(df), big.mark = ""), data_label()),
        ctt_card("Items selected", ncol(df), paste("of", ncol(raw_data()) - 1, "columns")),
        ctt_card("Missing responses", paste0(fmt(pct, 1), "%"), paste(n_miss, "empty cells"),
                 if (pct > 5) "#fd7e14" else "#198754")
      )
    })
    
    # =====================================================
    # PREVIEW
    # =====================================================
    output$data_preview_ctt <- renderDT({
      req(raw_data(), input$items_ctt)
      datatable(
        raw_data()[, c("row_ID", input$items_ctt), drop=FALSE],
        extensions = 'Buttons',
        options=list(scrollX=TRUE, dom = 'Brtp',pageLength=15,
                     buttons = list(list(extend = 'excel',text = 'Export Excel',
                                         filename = paste0('Data'))
                       )
                     )
      )
    },server = FALSE)

    # =====================================================
    # STRUCTURE SUMMARY
    # =====================================================
    output$data_summary_ctt <- renderDT({
      req(raw_data(), input$items_ctt)

      df <- raw_data()[, input$items_ctt, drop=FALSE]
      if (is_response()) df <- df[-1,]

      levels_all <- sort(unique(unlist(df)))

      count_rows <- lapply(levels_all, function(v){
        c("count", v, sapply(df, function(x) sum(x==v, na.rm=TRUE)))
      })

      out <- as.data.frame(do.call(rbind, count_rows))
      colnames(out) <- c("Statistic","Value", input$items_ctt)

      datatable(out,
                options=list(scrollX=TRUE, dom = 'Brtp',pageLength=15,
                             buttons = list(list(extend = 'excel',text = 'Export Excel',
                                                 filename = paste0('Data'))
                             )
                ),
                rownames=FALSE)
    })

    # =====================================================
    # RUN CTT
    # =====================================================
    observeEvent(input$run_ctt, {
      req(raw_data(), input$items_ctt)
      updateTabsetPanel(session, "main_tab_ctt", selected = "iteman_alysis_tab")
    })

    ctt_result <- eventReactive(input$run_ctt, {

      showModal(modalDialog(title = NULL, "Please wait, (Running ITEM ANALYSIS)...", footer = NULL, easyClose = FALSE))
      on.exit(removeModal(), add = TRUE)

      resp <- is_response()
      key_vec <- NULL
      data <- NULL

      if (!resp) {
        scored <- raw_data()[, input$items_ctt, drop=FALSE]
        score  <- rowSums(scored, na.rm = TRUE)
      } else {
        df <- raw_data()
        key <- df %>% dplyr::filter(row_ID=="KEY") %>% dplyr::select(dplyr::all_of(input$items_ctt))
        data  <- df %>% dplyr::filter(row_ID!="KEY") %>% dplyr::select(dplyr::all_of(input$items_ctt))
        sc  <- CTT::score(data, key, output.scored=TRUE)
        scored <- sc$scored
        colnames(scored) <- colnames(data)
        score  <- sc$score
        key_vec <- unlist(key[1, ])
        names(key_vec) <- colnames(key)
      }
      scored <- as.data.frame(scored)
      ia  <- CTT::itemAnalysis(scored, NA.Delete = TRUE)
      sem <- ia$scaleSD * sqrt(1 - ia$alpha)

      res <- list(
        scored = scored,
        score = score,
        item = ia$itemReport,
        itemReport = ia$itemReport,
        item_eval = ctt_item_eval(ia$itemReport, scored, ia$alpha),
        alpha = ia$alpha,
        scaleMean = ia$scaleMean,
        scaleSD = ia$scaleSD,
        nItem = ia$nItem,
        nPerson = ia$nPerson,
        SEM = sem,
        sem = sem,
        key = key_vec,
        distractor = if (resp) CTT::distractorAnalysis(items = data, key = key) else NULL,
        type = if (all(unlist(scored) %in% c(0, 1), na.rm = TRUE)) "dichotomous" else "polytomous",
        data_label = data_label()
      )
      res$split <- ctt_split_half(scored)
      res$desc <- ctt_descriptives(res)
      res$person <- ctt_person_scores(score, res, 0.95)
      res
    })

    # =====================================================
    # ITEM ANALYSIS: SUMMARY CARDS & TABLE
    # =====================================================
    output$ctt_cards <- renderUI({
      req(ctt_result())
      r <- ctt_result()
      ev <- r$item_eval
      al <- ctt_alpha_label(r$alpha)
      n_act <- sum(ev$Recommendation != "Retain", na.rm = TRUE)
      div(
        class = "ctt-cards",
        ctt_card("Cronbach's \u03b1", fmt(r$alpha), al$label, al$color),
        ctt_card("SEM", fmt(r$SEM), "score units"),
        ctt_card("Mean score", fmt(r$scaleMean, 2), paste0("SD = ", fmt(r$scaleSD, 2))),
        ctt_card("Examinees \u00d7 items", paste(r$nPerson, "\u00d7", r$nItem), r$type),
        ctt_card("Items to review", n_act, "revise / eliminate",
                 if (n_act == 0) "#198754" else "#dc3545")
      )
    })
    
    output$item_flags_ui <- renderUI({
      req(ctt_result())
      ev <- ctt_result()$item_eval
      n_keep <- sum(ev$Recommendation == "Retain", na.rm = TRUE)
      n_rev  <- sum(ev$Recommendation == "Revise", na.rm = TRUE)
      n_elim <- sum(ev$Recommendation == "Eliminate / revise", na.rm = TRUE)
      n_alpha <- sum(ev$Raises_Alpha, na.rm = TRUE)
      neg <- ev$Item[!is.na(ev$Discrimination) & ev$Discrimination < 0]
      tagList(
        div(class = "ctt-note", style = "margin-bottom:6px;",
            sprintf("%d retain \u00b7 %d revise \u00b7 %d eliminate / revise \u00b7 \u03b1 would rise if removed: %d",
                    n_keep, n_rev, n_elim, n_alpha)),
        if (length(neg) > 0)
          div(class = "ctt-flag ctt-flag-bad",
              "Negative discrimination: ", paste(neg, collapse = ", "),
              ". Check the answer key or reverse-coded items.")
      )
    })
    
    output$item_table <- renderDT({
      req(ctt_result())
      r <- ctt_result()
      ev <- r$item_eval
      df <- data.frame(
        Item = ev$Item,
        Mean = ev$Mean,
        p = ev$Difficulty,
        Difficulty = ev$Difficulty_Level,
        `Item-Total r` = ev$Discrimination,
        Discrimination = ev$Discrimination_Level,
        Biserial = ev$Biserial,
        `Alpha if Deleted` = ev$Alpha_if_Deleted,
        `Raises Alpha` = ifelse(ev$Raises_Alpha, "Yes", "-"),
        Recommendation = ev$Recommendation,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      if (r$type != "dichotomous") df$Biserial <- NULL
      if (r$type == "dichotomous") df$Mean <- NULL      # identical to p for 0/1 items
      num_cols <- intersect(c("Mean", "p", "Item-Total r", "Biserial", "Alpha if Deleted"), names(df))
      hide <- which(names(df) == "Raises Alpha") - 1L    # kept for styling only
      
      datatable(
        df,
        rownames = FALSE,
        extensions = "Buttons",
        selection = list(mode = "single", selected = 1, target = "row"),
        options = list(scrollX = TRUE, dom = "Bt", pageLength = 500,
                       columnDefs = list(list(visible = FALSE, targets = hide)),
                       buttons = list(list(extend = "excel", text = "Export Excel",
                                           filename = "Item Analysis Results")))
      ) %>%
        dt_round(num_cols) %>%
        formatStyle("Difficulty",
                    backgroundColor = styleEqual(c("Difficult", "Moderate", "Easy"),
                                                 c("#fff3cd", "#d4edda", "#fff3cd"))) %>%
        formatStyle("Discrimination",
                    backgroundColor = styleEqual(c("Very good", "Good", "Marginal", "Poor", "Negative"),
                                                 c("#d4edda", "#d4edda", "#fff3cd", "#f8d7da", "#f8d7da"))) %>%
        formatStyle("Alpha if Deleted", valueColumns = "Raises Alpha",
                    backgroundColor = styleEqual("Yes", "#f8d7da")) %>%
        formatStyle("Recommendation",
                    fontWeight = "bold",
                    backgroundColor = styleEqual(c("Retain", "Revise", "Eliminate / revise"),
                                                 c("#d4edda", "#fff3cd", "#f8d7da")))
    }, server = FALSE)
    
    # =====================================================
    # ITEM DETAIL (selector, summary, ICC, distractors)
    # =====================================================
    output$item_selected_ui <- renderUI({
      req(ctt_result())
      items <- ctt_result()$item_eval$Item
      selectInput("item_distractor", "Item:", choices = items,
                  selected = items[1], multiple = FALSE, width = "100%")
    })

    observeEvent(input$item_table_rows_selected, {
      req(ctt_result())
      items <- ctt_result()$item_eval$Item
      idx <- input$item_table_rows_selected
      if (length(idx) == 1 && idx <= length(items))
        updateSelectInput(session, "item_distractor", selected = items[idx])
    })

    output$item_summary_ui <- renderUI({
      req(ctt_result(), input$item_distractor)
      ev <- ctt_result()$item_eval
      row <- ev[ev$Item == input$item_distractor, , drop = FALSE]
      req(nrow(row) == 1)
      tone <- switch(row$Recommendation,
                     "Retain" = "#198754", "Revise" = "#e0a800", "#dc3545")
      txt <- paste0(
        "Difficulty p = ", fmt(row$Difficulty, 2), " (", tolower(row$Difficulty_Level), "). ",
        "Item-total r = ", fmt(row$Discrimination, 2), " (", tolower(row$Discrimination_Level), " discrimination). ",
        if (isTRUE(row$Raises_Alpha)) "Removing this item would raise α. " else ""
      )
      tagList(
        span(class = "ctt-badge", style = paste0("background:", tone, ";"), row$Recommendation),
        p(class = "ctt-note", style = "margin-top:8px;", txt)
      )
    })

    output$icc_ctt <- renderPlot({
      req(ctt_result(), input$item_distractor)
      item_select <- input$item_distractor
      req(item_select %in% colnames(ctt_result()$scored))
      item_vec <- ctt_result()$scored[, item_select]

      old <- options(OutDec = dec()); on.exit(options(old), add = TRUE)
      CTT::cttICC(ctt_result()$score, item_vec, colTheme="cavaliers",
             cex=1.5, ylab = paste0('Item Mean (Difficulty)'),
             plotTitle = paste0('Item Characteristic Curve [',item_select, ']'))
    })

    output$show_distractor <- reactive(is_response())
    outputOptions(output,"show_distractor", suspendWhenHidden=FALSE)

    distractor_df <- reactive({
      req(ctt_result()$distractor, input$item_distractor)
      da <- ctt_result()$distractor
      req(input$item_distractor %in% names(da))
      df <- da[[input$item_distractor]]
      cbind(Option = rownames(df), df, stringsAsFactors = FALSE)
    })

    output$distractor_table <- DT::renderDT({
      df <- distractor_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      num_cols <- setdiff(num_cols, "n")

      DT::datatable(
        df,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          dom = "Bt",
          buttons = list(list(extend = 'excel',text = 'Export Excel',
                              filename = paste0('Distractor Analysis: [',input$item_distractor,']')))
        )
      ) %>%
        dt_round(num_cols) %>%
        DT::formatStyle("Option", "correct",
                        backgroundColor = DT::styleEqual("*", "#d4edda"),
                        fontWeight = DT::styleEqual("*", "bold"))
    })

    output$distractor_unavailable <- renderUI({
      if (!is_response())
        div(class = "ctt-flag ctt-flag-ok",
            "Distractor analysis needs response data with an answer key (options such as A\u2013D). ",
            "Choose 'Response with key' in the Data tab to use it.")
    })
    
    output$distractor_title <- renderUI({
      req(input$item_distractor)
      key <- ctt_result()$key[input$item_distractor]
      tags$h4(style = "margin-top:0;",
              paste0("Item ", input$item_distractor),
              span(class = "ctt-note", style = "margin-left:8px;", paste("key =", key)))
    })
    
    output$distractor_overview <- renderDT({
      req(ctt_result()$distractor)
      r <- ctt_result()
      rows <- lapply(names(r$distractor), function(it) {
        iss <- ctt_distractor_issues(r$distractor[[it]])
        data.frame(
          Item = it,
          Key = unname(r$key[it]),
          `Non-functioning` = paste(iss$low, collapse = ", "),
          `Positive r` = paste(iss$positive, collapse = ", "),
          Status = if (length(iss$low) + length(iss$positive) == 0) "OK" else "Review",
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)
      datatable(df, rownames = FALSE, extensions = "Buttons",
                selection = list(mode = "single", selected = 1, target = "row"),
                options = list(scrollX = TRUE, dom = "Bt", pageLength = 500,
                               buttons = list(list(extend = "excel", text = "Export Excel",
                                                   filename = "Distractor Overview")))) %>%
        formatStyle("Status", fontWeight = "bold",
                    backgroundColor = styleEqual(c("OK", "Review"), c("#d4edda", "#fff3cd")))
    }, server = FALSE)
    
    observeEvent(input$distractor_overview_rows_selected, {
      req(ctt_result()$distractor)
      items <- names(ctt_result()$distractor)
      idx <- input$distractor_overview_rows_selected
      if (length(idx) == 1 && idx <= length(items))
        updateSelectInput(session, "item_distractor", selected = items[idx])
    })
    
    output$distractor_plot <- renderPlot({
      df <- distractor_df()
      grp <- c("lower", "mid50", "mid75", "upper")
      grp <- intersect(grp, names(df))
      long <- do.call(rbind, lapply(seq_len(nrow(df)), function(i)
        data.frame(Option = df$Option[i], Key = df$correct[i] == "*",
                   Group = factor(grp, levels = grp, labels = c("Lower", "Mid 50", "Mid 75", "Upper")[seq_along(grp)]),
                   Proportion = unlist(df[i, grp]), stringsAsFactors = FALSE)))
      ggplot(long, aes(Group, Proportion, group = Option, colour = Option, linewidth = Key)) +
        geom_line() + geom_point(size = 2.5) +
        scale_linewidth_manual(values = c(`FALSE` = 0.7, `TRUE` = 1.8), guide = "none") +
        scale_y_continuous(labels = ax, limits = c(0, 1)) +
        labs(x = "Total-score group", y = "Proportion choosing the option",
             subtitle = "Bold line = key. A good distractor falls as ability rises; the key rises.") +
        theme_minimal() + theme(legend.position = "bottom")
    })
    
    output$distractor_note <- renderUI({
      df <- distractor_df()
      iss <- ctt_distractor_issues(df)
      low <- iss$low
      pos <- iss$positive
      tagList(
        div(class = "ctt-note", "Green row = answer key. rspP = proportion choosing the option; pBis = option-total correlation."),
        if (length(low) > 0)
          div(class = "ctt-flag ctt-flag-warn",
              "Non-functioning distractor (chosen by < 5%): ", paste(low, collapse = ", ")),
        if (length(pos) > 0)
          div(class = "ctt-flag ctt-flag-bad",
              "Distractor with positive item-total correlation (review): ", paste(pos, collapse = ", ")),
        if (length(low) == 0 && length(pos) == 0)
          div(class = "ctt-flag ctt-flag-ok", "All distractors are functioning.")
      )
    })

    # =====================================================
    # RELIABILITY
    # =====================================================
    output$reliability_box <- renderUI({
      req(ctt_result())
      r <- ctt_result()
      al <- ctt_alpha_label(r$alpha)
      ci <- r$scaleMean + c(-1, 1) * 1.96 * r$SEM
      sh <- r$split
      row <- function(a, b) tags$tr(tags$td(a), tags$td(b))
      
      div(
        class = "ctt-panel",
        div(class = "ctt-card-label", "Cronbach's \u03b1"),
        div(style = "display:flex;align-items:baseline;gap:12px;",
            span(style = "font-size:38px;font-weight:700;color:#0f172a;", fmt(r$alpha)),
            span(class = "ctt-badge", style = paste0("background:", al$color, ";"), al$label)),
        tags$table(
          class = "ctt-stat-table",
          row("SEM", fmt(r$SEM)),
          row("95% band around the mean score", paste0(fmt(ci[1], 2), " \u2013 ", fmt(ci[2], 2))),
          row("Split-half r (odd-even)", fmt(sh$r)),
          row("Spearman-Brown corrected", fmt(sh$sb)),
          row("Items / examinees", paste(r$nItem, "/", r$nPerson))
        ),
        tags$p(class = "ctt-note",
               "SEM = SD \u00d7 \u221a(1 \u2212 \u03b1); an observed score lies within \u00b11.96 SEM of the true score 95% of the time. ",
               "Cut-offs: George & Mallery (2003); Nunnally & Bernstein (1994).")
      )
    })
    
    output$alpha_if_deleted_plot <- renderPlot({
      req(ctt_result())
      r <- ctt_result()
      ev <- r$item_eval
      ev$Item <- factor(ev$Item, levels = ev$Item)
      ggplot(ev, aes(x = Item, y = Alpha_if_Deleted, fill = Raises_Alpha)) +
        geom_col(width = 0.7) +
        geom_hline(yintercept = r$alpha, linetype = "dashed", colour = "#dc3545") +
        scale_fill_manual(values = c(`FALSE` = "#0d6efd", `TRUE` = "#dc3545"),
                          labels = c(`FALSE` = "Keep", `TRUE` = "Raises alpha if deleted"),
                          name = NULL) +
        scale_y_continuous(labels = ax) +
        coord_cartesian(ylim = c(max(0, min(ev$Alpha_if_Deleted, na.rm = TRUE) - 0.05), NA)) +
        labs(x = NULL, y = "Alpha if item deleted",
             subtitle = paste0("Dashed line: overall alpha = ", fmt(r$alpha))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.position = "bottom")
    })

    output$score_histogram <- renderPlot({
      req(ctt_result())
      scores <- ctt_result()$score
      rng <- diff(range(scores, na.rm = TRUE))
      ggplot(data.frame(Score = scores), aes(x = Score)) +
        geom_histogram(bins = max(5, min(30, rng + 1)), fill = "#0d6efd", color = "white", alpha = 0.8) +
        geom_vline(xintercept = mean(scores, na.rm = TRUE), colour = "#dc3545", linetype = "dashed") +
        scale_x_continuous(labels = ax) +
        scale_y_continuous(labels = ax) +
        theme_minimal() +
        labs(x = "Total Score", y = "Frequency", subtitle = "Dashed line: mean")
    })

    output$score_descriptives_out <- renderDT({
      req(ctt_result())
      df <- ctt_result()$desc
      datatable(df, options = list(dom = "t", pageLength = 15), rownames = FALSE) %>%
        dt_round("Value")
    })

    output$person_scores_out <- renderDT({
      req(ctt_result())
      r <- ctt_result()
      out <- cbind(Respondent = seq_along(r$score), r$person)
      datatable(out, rownames = FALSE, extensions = "Buttons",
                options = list(scrollX = TRUE, pageLength = 10, dom = "Bfrtip",
                               buttons = list(list(extend = "excel", text = "Export Excel",
                                                   filename = "Person Scores")))) %>%
        dt_round(setdiff(names(out), "Respondent"), digits = min(nd(), 2L))
    }, server = FALSE)

  # ==== R Console Output & Model Export ====
  ctt_text_summary <- function(r) {
    paste(c(
      sprintf("Items: %d | Examinees: %d | Type: %s", r$nItem, r$nPerson, r$type),
      sprintf("Cronbach's alpha: %.3f (%s)", r$alpha, ctt_alpha_label(r$alpha)$label),
      sprintf("SEM: %.3f | Mean: %.3f | SD: %.3f", r$SEM, r$scaleMean, r$scaleSD),
      "",
      capture.output(print(
        cbind(r$item_eval[, c("Item", "Mean", "Difficulty", "Discrimination",
                              "Alpha_if_Deleted", "Recommendation")]),
        digits = 3, row.names = FALSE))
    ), collapse = "\n")
  }

  observeEvent(ctt_result(), {
    req(ctt_result())
    console_context$text <- ctt_text_summary(ctt_result())
  })

  output$export_ctt_rds <- downloadHandler(
    filename = function() { paste0("CTT_result_", Sys.Date(), ".rds") },
    content = function(file) {
      req(ctt_result())
      saveRDS(ctt_result(), file)
    }
  )

  # ==== Score New Data ====
  output$download_ctt_template <- downloadHandler(
    filename = function() { "CTT_template.xlsx" },
    content = function(file) {
      req(input$items_ctt)
      items <- input$items_ctt
      df <- data.frame(matrix(ncol = length(items), nrow = 0))
      colnames(df) <- items
      writexl::write_xlsx(df, file)
    }
  )

  ctt_newscores_reactive <- eventReactive(input$ctt_score_newdata_btn, {
    req(ctt_result(), input$ctt_newdata)
    r <- ctt_result()
    df <- ctt_read_table(input$ctt_newdata$datapath, input$ctt_newdata$name)

    items <- colnames(r$scored)
    matched <- intersect(items, colnames(df))
    missing <- setdiff(items, colnames(df))
    validate(need(length(matched) > 0, "None of the selected items were found in the uploaded file."))
    df_used <- df[, matched, drop = FALSE]

    if (!is.null(r$key)) {
      # response data: score with the key from the analysis
      df_used[] <- lapply(df_used, as.character)
      sc <- CTT::score(df_used, r$key[matched], output.scored = TRUE)
      total <- sc$score
      answered <- rowSums(!is.na(sc$scored))
    } else {
      for (j in seq_along(df_used)) {
        x <- as.character(df_used[[j]])
        df_used[[j]] <- as.numeric(gsub(",", ".", x, fixed = TRUE))
      }
      total <- rowSums(df_used, na.rm = TRUE)
      answered <- rowSums(!is.na(df_used))
    }

    list(
      scores = data.frame(Respondent = seq_along(total), Score = as.numeric(total),
                          Items_Answered = as.integer(answered)),
      matched = matched, missing = missing, n_items = length(items)
    )
  })

  output$ctt_newdata_check <- renderUI({
    res <- ctt_newscores_reactive()
    ok <- length(res$missing) == 0
    div(
      class = paste("ctt-flag", if (ok) "ctt-flag-ok" else "ctt-flag-warn"),
      sprintf("Matched %d of %d items. ", length(res$matched), res$n_items),
      if (!ok) paste("Not found in the file:", paste(res$missing, collapse = ", "),
                     "– scores are computed from matched items only.")
      else "All items found.",
      if (!ok) " The SEM band assumes the full item set, so interpret it with caution."
    )
  })

  ctt_newscores_final <- reactive({
    res <- ctt_newscores_reactive()
    ps <- ctt_person_scores(res$scores$Score, ctt_result(), as.numeric(input$ctt_conf_level))
    names(ps)[names(ps) == "Lower"] <- "Lower_CI"
    names(ps)[names(ps) == "Upper"] <- "Upper_CI"
    cbind(res$scores[, c("Respondent", "Items_Answered")], ps)
  })

  output$ctt_newscores_table <- DT::renderDataTable({
    out <- ctt_newscores_final()
    datatable(out, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 15)) %>%
      dt_round(setdiff(names(out), c("Respondent", "Items_Answered")), digits = min(nd(), 2L))
  })

  output$ctt_newscores_plot <- renderPlot({
    res <- ctt_newscores_reactive()
    ggplot(res$scores, aes(x = Score)) +
      geom_histogram(bins = 20, fill = "#0f766e", color = "white", alpha = 0.85) +
      scale_x_continuous(labels = ax) +
      scale_y_continuous(labels = ax) +
      theme_minimal() +
      labs(x = "Total score (new data)", y = "Frequency")
  })

  output$download_ctt_newscores <- downloadHandler(
    filename = function() { "CTT_New_Scores.csv" },
    content = function(file) {
      req(ctt_newscores_reactive())
      out <- ctt_round_df(ctt_newscores_final(), nd())
      # decimal comma -> semicolon-separated file (read.csv2 / Excel in comma locales)
      if (dec() == ",") utils::write.csv2(out, file, row.names = FALSE)
      else utils::write.csv(out, file, row.names = FALSE)
    }
  )

  # ==== AI Assistant ====
  # Update global AI context whenever results change
  observe({
    res_text <- ""
    if (!is.null(ctt_result())) {
      res_text <- ctt_text_summary(ctt_result())
    }
    ai_context$results_text <- res_text
    ai_context$module <- "Classical Test Theory (CTT)"
  })

  # ==== Report ====
  addResourcePath("ctt_reports", tempdir())
  ctt_report_path <- reactiveVal(NULL)
  ctt_report_stamp <- reactiveVal(0)

  render_ctt_report <- function(out_file) {
    # the app directory is the working directory while the app runs; fall back to the installed copy
    report_path <- "ctt_report.Rmd"
    if (!file.exists(report_path)) {
      report_path <- file.path(system.file("app", package = "measureR"), "ctt_report.Rmd")
    }
    work_dir <- tempfile("ctt_report_")
    dir.create(work_dir)
    tempReport <- file.path(work_dir, "ctt_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)

    rmarkdown::render(
      tempReport, output_file = out_file, quiet = TRUE,
      envir = new.env(parent = globalenv()),
      params = list(
        ctt_res = ctt_result(),
        dec = dec(),
        digits = nd(),
        console_out = console_context$text,
        ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text
      )
    )
    invisible(out_file)
  }

  observeEvent(input$ctt_generate_preview, {
    req(ctt_result())
    out_html <- file.path(tempdir(), "ctt_report_out.html")

    showModal(modalDialog("Generating Report Preview...", footer = NULL))
    tryCatch({
      render_ctt_report(out_html)
      ctt_report_path(out_html)
      ctt_report_stamp(ctt_report_stamp() + 1)
    }, error = function(e) {
      showNotification(paste("Error rendering report:", conditionMessage(e)), type = "error", duration = 10)
    }, finally = {
      removeModal()
    })
  })

  output$ctt_report_preview_frame <- renderUI({
    if (is.null(ctt_report_path()))
      return(div(class = "ctt-flag ctt-flag-ok",
                 "Run the analysis, then click 'Generate Report Preview'. The report uses the decimal separator set in Settings."))
    tags$iframe(src = paste0("ctt_reports/ctt_report_out.html?v=", ctt_report_stamp()),
                width = "100%", height = "850px", style = "border: none;")
  })

  output$download_report_ctt <- downloadHandler(
    filename = function() {
      paste0("CTT_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(ctt_result())
      render_ctt_report(file)
    }
  )
}
