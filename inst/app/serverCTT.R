server_ctt <- function(input, output, session, ai_context, console_context) {
    library(CTT)
    library(dplyr)
    library(DT)
    library(readxl)
    
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
        data("CTTdata", package="CTT")
        df <- as.data.frame(lapply(CTTdata, function(x)
          recode(x, A=1, B=2, C=3, D=4))) %>% 
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
        ext <- tools::file_ext(input$datafile_ctt$name)
        if (ext=="csv") df <- read.csv(input$datafile_ctt$datapath, stringsAsFactors=FALSE)
        else df <- read_excel(input$datafile_ctt$datapath)
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
        "Select Items:",
        choices = setdiff(names(raw_data()), "row_ID"),
        selected = setdiff(names(raw_data()), "row_ID"),
        multiple = TRUE
      )
    })
    
    output$item_selected_ui <- renderUI({
      req(raw_data())
      choices <- setdiff(names(raw_data()), "row_ID")
      div(class = "select-large", 
          selectInput("item_distractor", "Select Items:", 
                      choices = choices, multiple = FALSE, selected = '', width = '100%'))
    })
    output$item_dist <- renderUI({
      req(input$item_distractor)
      tags$div(
        style = "
      background:#f8f9fa;
      border-left:4px solid #0d6efd;
      padding:8px 12px;
      margin-bottom:10px;
    ",
        tags$span("Distractor Analysis: "),
        tags$b(input$item_distractor)
      )
    })
    
    
    
    # =====================================================
    # DATA TYPE BADGE
    # =====================================================
    is_response <- reactive(input$data_source_ctt %in% c("respkey","upload_respkey"))
    
    output$data_type_badge <- renderUI({
      div(
        style=paste0(
          "padding:10px;border-left:5px solid;",
          if (is_response()) "#fd7e14;background:#fff3cd;"
          else "#198754;background:#e9f7ef;"
        ),
        if (is_response())
          "Data type: Response with Key (scoring required before analysis)"
        else
          "Data type: Scored data (ready for CTT analysis)"
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
      
      if (!is_response()) {
      scored <- raw_data()[, input$items_ctt, drop=FALSE]
      score  <- rowSums(scored, na.rm = TRUE)
      }
      
      if (is_response()) {
        df <- raw_data()
        key <- df %>% dplyr::filter(row_ID=="KEY") %>% dplyr::select(input$items_ctt)
        data  <- df %>% dplyr::filter(row_ID!="KEY") %>% dplyr::select(input$items_ctt)
        scored  <- CTT::score(data, key, output.scored=TRUE)$scored
        colnames(scored) <- colnames(data)
        score  <- CTT::score(data, key, output.scored=TRUE)$score
      }
      ia  <- CTT::itemAnalysis(as.data.frame(scored), NA.Delete = TRUE,)

      removeModal()
      
      list(
        scored = scored,
        score = score,
        item = ia$itemReport,
        alpha = ia$alpha,
        sem = ia$scaleSD * sqrt(1 - ia$alpha),
        distractor =
          if (is_response())
            CTT::distractorAnalysis(items = data, key = key)
        else NULL,
        type = if (all(scored %in% c(0,1))) "dichotomous" else "polytomous"
      )
    })
    # =====================================================
    # ITEM TABLE
    # =====================================================
    output$item_table <- renderDT({
      req(ctt_result())
      selected_data <- input$items_ctt
      df <- data.frame(Item =selected_data , round(ctt_result()$item[,c("itemMean","pBis","bis")],3))
      use_disc <- if (ctt_result()$type=="dichotomous") "pBis" else "bis"
      
      datatable(df,
        rownames=FALSE,
        options=list(scrollX=TRUE, dom = 'Bt',pageLength=100,
                     buttons = list(list(extend = 'excel',text = 'Export Excel',
                                         filename = paste0('Item Analysis Results'))
                     )
        )
        ) %>%
        formatStyle(
          "itemMean",
          backgroundColor = styleInterval(
            c(0.3,0.7),
            c("#f8d7da","#d4edda","#f8d7da")
          )
        ) %>%
        formatStyle(
          use_disc,
          backgroundColor = styleInterval(
            c(0.2,0.4),
            c("#f8d7da","#fff3cd","#d4edda")
          )
        )
    },server = FALSE)
    output$icc_ctt <- renderPlot({
      req(ctt_result(), input$item_distractor)
      item_select <- input$item_distractor 
      item_vec <- ctt_result()$scored[, item_select]
      
      CTT::cttICC(ctt_result()$score, item_vec, colTheme="cavaliers", 
             cex=1.5, ylab = paste0('Item Mean (Difficulty)'),
             plotTitle = paste0('Item Characteristic Curve [',item_select, ']'))
    })
       
    # =====================================================
    # RELIABILITY BOX
    # =====================================================
    output$reliability_box <- renderUI({
      req(ctt_result())
      
      alpha <- round(ctt_result()$alpha, 3)
      sem   <- round(ctt_result()$sem, 3)
      
      # ===============================
      # KATEGORI & WARNA
      # ===============================
      if (alpha >= 0.90) {
        label <- "Excellent Reliability"
        color <- "#198754"   # green dark
      } else if (alpha >= 0.80) {
        label <- "Good Reliability"
        color <- "#28a745"
      } else if (alpha >= 0.70) {
        label <- "Acceptable Reliability"
        color <- "#ffc107"
      } else if (alpha >= 0.60) {
        label <- "Questionable Reliability"
        color <- "#fd7e14"
      } else {
        label <- "Poor Reliability"
        color <- "#dc3545"
      }
      
      HTML(paste0(
        "<div style='
      border-left:6px solid ", color, ";
      background:#f8f9fa;
      padding:14px;
      border-radius:6px;
      font-family:Segoe UI, sans-serif;
    '>",
        
        "<div style='
      display:inline-block;
      background:", color, ";
      color:white;
      padding:4px 10px;
      border-radius:12px;
      font-size:13px;
      font-weight:600;
      margin-bottom:8px;
    '>", label, "</div><br><br>",
        
        "<b>Cronbach’s α:</b> ", alpha, "<br>",
        "<b>SEM:</b> ", sem, "<br><br>",
        "</div>"
      ))
    })
    
    
    # =====================================================
    # DISTRACTOR FLAG
    # =====================================================
    output$show_distractor <- reactive(is_response())
    outputOptions(output,"show_distractor", suspendWhenHidden=FALSE)
    

    output$distractor_table <- DT::renderDT({
      req(ctt_result()$distractor, input$item_distractor)
      
      da <- ctt_result()$distractor
      req(input$item_distractor %in% names(da))
      
      df <- da[[input$item_distractor]]
      
      # ===============================
      # 1. ROUND ANGKA
      # ===============================
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- round(df[num_cols], 3)
      
      # ===============================
      # 2. FLAG JAWABAN BENAR (0 / 1)
      # ===============================
      DT::datatable(
        df,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Bt",
          buttons = list(list(extend = 'excel',text = 'Export Excel',
                              filename = paste0('Distractor Analysis: [',input$item_distractor,']'))
          )
        )

      )
    })
    
    # =====================================================
    # SCORE DISTRIBUTION & RELIABILITY
    # =====================================================
    output$split_half_out <- renderText({
      req(ctt_result())
      scored <- ctt_result()$scored
      if (ncol(scored) < 2) return("Need at least 2 items")
      
      # Odd-Even Split
      odd_idx <- seq(1, ncol(scored), by = 2)
      even_idx <- seq(2, ncol(scored), by = 2)
      
      score_odd <- rowSums(scored[, odd_idx, drop = FALSE], na.rm = TRUE)
      score_even <- rowSums(scored[, even_idx, drop = FALSE], na.rm = TRUE)
      
      r_oe <- cor(score_odd, score_even, use = "pairwise.complete.obs")
      sb_oe <- 2 * r_oe / (1 + r_oe)
      
      paste0("Spearman-Brown (Odd-Even Split): ", round(sb_oe, 3))
    })
    
    output$alpha_if_deleted_out <- renderDT({
      req(ctt_result())
      ia <- CTT::itemAnalysis(as.data.frame(ctt_result()$scored), NA.Delete = TRUE)
      df <- data.frame(
        Item = colnames(ctt_result()$scored),
        AlphaIfDeleted = round(ia$itemReport$alphaIfDeleted, 3)
      )
      datatable(df, options = list(dom = "t", pageLength = 100, scrollY = "300px"), rownames = FALSE)
    })
    
    output$score_histogram <- renderPlot({
      req(ctt_result())
      scores <- ctt_result()$score
      library(ggplot2)
      ggplot(data.frame(Score = scores), aes(x = Score)) +
        geom_histogram(binwidth = 1, fill = "#0d6efd", color = "white", alpha = 0.8) +
        theme_minimal() +
        labs(x = "Total Score", y = "Frequency")
    })
    
    output$score_descriptives_out <- renderDT({
      req(ctt_result())
      scores <- ctt_result()$score
      
      df <- data.frame(
        Statistic = c("N", "Mean", "Median", "Std. Deviation", "Min", "Max"),
        Value = c(
          length(na.omit(scores)),
          round(mean(scores, na.rm = TRUE), 3),
          round(median(scores, na.rm = TRUE), 3),
          round(sd(scores, na.rm = TRUE), 3),
          min(scores, na.rm = TRUE),
          max(scores, na.rm = TRUE)
        )
      )
      
      datatable(df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
    })
    
  # ==== R Console Output & Model Export ====
  observeEvent(ctt_result(), {
    req(ctt_result())
    out <- paste(capture.output(print(ctt_result())), collapse = "\n")
    console_context$text <- out
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
    req(input$items_ctt, input$ctt_newdata)
    ext <- tools::file_ext(input$ctt_newdata$name)
    df <- switch(
      ext,
      "csv" = read.csv(input$ctt_newdata$datapath),
      "xlsx" = readxl::read_excel(input$ctt_newdata$datapath),
      "xls" = readxl::read_excel(input$ctt_newdata$datapath),
      stop("Invalid file format")
    )
    
    # Calculate scores (sum score for CTT)
    items <- input$items_ctt
    df_used <- df[, intersect(colnames(df), items), drop = FALSE]
    # convert to numeric
    for (j in seq_along(df_used)) {
      df_used[[j]] <- as.numeric(as.character(df_used[[j]]))
    }
    scores <- rowSums(df_used, na.rm = TRUE)
    
    as.data.frame(scores)
  })

  output$ctt_newscores_table <- DT::renderDataTable({
    req(ctt_newscores_reactive())
    datatable(round(ctt_newscores_reactive(), 3), options = list(scrollX = TRUE))
  })

  output$download_ctt_newscores <- downloadHandler(
    filename = function() { "CTT_New_Scores.csv" },
    content = function(file) {
      req(ctt_newscores_reactive())
      write.csv(ctt_newscores_reactive(), file, row.names = FALSE)
    }
  )

  # ==== AI Assistant ====
  # Update global AI context whenever results change
  observe({
    res_text <- ""
    if (!is.null(ctt_result())) {
      fit <- ctt_result()
      res_text <- paste(capture.output(print(fit)), collapse = "\n")
    }
    ai_context$results_text <- res_text
    ai_context$module <- "Classical Test Theory (CTT)"
  })

  addResourcePath("ctt_reports", tempdir())
  ctt_report_path <- reactiveVal(NULL)
  
  observeEvent(input$ctt_generate_preview, {
    req(ctt_result())
    
    report_path <- file.path(system.file("app", package = "measureR"), "ctt_report.Rmd")
    if (report_path == "" || !file.exists(report_path)) {
      report_path <- "ctt_report.Rmd"
    }
    
    tempReport <- file.path(tempdir(), "ctt_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)
    
    out_html <- file.path(tempdir(), "ctt_report_out.html")
    
    showModal(modalDialog("Generating Report Preview...", footer = NULL))
    tryCatch({
      rmarkdown::render(tempReport, output_file = out_html,
        params = list(
          ctt_res = ctt_result(),
          console_out = console_context$text,
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text
        )
      )
      ctt_report_path(out_html)
    }, error = function(e) {
      showNotification(paste("Error rendering report:", e$message), type = "error")
    }, finally = {
      removeModal()
    })
  })
  
  output$ctt_report_preview_frame <- renderUI({
    req(ctt_report_path())
    tags$iframe(src = "ctt_reports/ctt_report_out.html", width = "100%", height = "800px", style = "border: none;")
  })

  output$download_report_ctt <- downloadHandler(
    filename = function() {
      paste0("CTT_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(ctt_report_path())
      file.copy(ctt_report_path(), file, overwrite = TRUE)
    }
  )
}
