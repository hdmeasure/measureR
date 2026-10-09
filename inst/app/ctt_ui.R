ctt_ui <- function(project) {
  
  tabsetPanel(
    id = "main_tab_ctt",
    
    # =====================================================
    # 1. DATA
    # =====================================================
    tabPanel(
      title = tagList(icon("upload"), "Data"),
      value = "prepare_data_tab",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("go_home", label = tagList(icon("home"), "Main Menu"),
                       class = "btn btn-danger btn-block", style = "width:100% !important;"),
          
          div(class = "ctt-step", "1  Choose data"),
          selectInput(
            "data_source_ctt", NULL,
            choices = c(
              "Upload scored data" = "upload_scored",
              "Upload response with key" = "upload_respkey",
              "Built-in dichotomous" = "diko",
              "Built-in polytomous" = "poli",
              "Built-in response with key" = "respkey"
            )
          ),
          conditionalPanel(
            condition = "input.data_source_ctt == 'upload_scored' || input.data_source_ctt == 'upload_respkey'",
            fileInput("datafile_ctt", "Upload data (csv / xlsx)"),
            div(class = "ctt-note", "Response with key: the first row must be the answer key.")
          ),
          
          div(class = "ctt-step", "2  Select items"),
          uiOutput("item_select_ui_ctt"),
          
          div(class = "ctt-step", "3  Run"),
          actionButton("run_ctt", label = tagList(icon("play"), "Run CTT Analysis"), 
                       class = "btn btn-success",
                       style = "width: 100% !important; margin-bottom: 10px;"),
          downloadButton("export_ctt_rds", "Export Model (.rds)", 
                         class = "btn btn-primary btn-sm", style = "width: 100% !important;")
        ),
        
        mainPanel(
          width = 9,
          h3(class = "ctt-title", "Classical Test Theory"),
          p(class = "ctt-lead",
            "Item analysis, reliability and scoring with published interpretation cut-offs."),
          uiOutput("data_cards_ctt"),
          uiOutput("data_type_badge"),
          
          tabsetPanel(
            type = "pills",
            tabPanel("Preview", br(), DTOutput("data_preview_ctt")),
            tabPanel("Response frequencies",
                     br(),
                     div(class = "ctt-note",
                         "Number of examinees choosing each value, per item. Use it to find out-of-range codes before running the analysis."),
                     br(), DTOutput("data_summary_ctt")),
            tabPanel("Data format & types",
                     br(),
                     tags$b("Required layout"),
                     div(class = "ctt-note",
                         "One row per examinee, one column per item. Scored data must be numeric; missing responses may be left empty."),
                     fluidRow(
                       column(4,
                              tags$small("Dichotomous"),
                              datatable(data.frame(ID = c("S1","S2","S3"), I1 = c(1,0,1), I2 = c(1,1,0), I3 = c(0,1,0)),
                                        options = list(dom = "t"), rownames = FALSE)),
                       column(4,
                              tags$small("Polytomous"),
                              datatable(data.frame(ID = c("S1","S2","S3"), I1 = c(1,3,2), I2 = c(4,2,3), I3 = c(3,3,2)),
                                        options = list(dom = "t"), rownames = FALSE)),
                       column(4,
                              tags$small("Response with key"),
                              datatable(data.frame(ID = c("KEY","S1","S2"), I1 = c("A","A","B"), I2 = c("C","C","D"), I3 = c("B","B","D")),
                                        options = list(dom = "t"), rownames = FALSE))
                     ),
                     br(),
                     tags$b("How the analysis differs by data type"),
                     ctt_type_guide_ui()
            )
          )
        )
      )
    ),
    
    # =====================================================
    # 2. ITEM ANALYSIS
    # =====================================================
    tabPanel(
      title = tagList(icon("chart-bar"), "Item Analysis"),
      value = "iteman_alysis_tab",
      div(class = "ctt-page",
        uiOutput("ctt_cards"),
        fluidRow(
          column(7,
            div(class = "ctt-section-head",
                h4("Item statistics"),
                span(class = "ctt-note", "Click a row to inspect the item.")),
            uiOutput("item_flags_ui"),
            DTOutput("item_table"),
            br(),
            ctt_guide_ui()
          ),
          column(5,
            div(
              class = "ctt-panel",
              uiOutput("item_selected_ui"),
              uiOutput("item_summary_ui"),
              plotOutput("icc_ctt", height = "300px"),
              conditionalPanel(
                condition = "output.show_distractor == true",
                div(class = "ctt-note", icon("circle-info"),
                    " Option-level results for this item are in the Distractors tab.")
              )
            )
          )
        )
      )
    ),
    
    # =====================================================
    # 2b. DISTRACTORS
    # =====================================================
    tabPanel(
      title = tagList(icon("list-ol"), "Distractors"),
      value = "distractor_tab",
      div(class = "ctt-page",
        uiOutput("distractor_unavailable"),
        conditionalPanel(
          condition = "output.show_distractor == true",
          fluidRow(
            column(5,
              div(class = "ctt-section-head",
                  h4("Items overview"),
                  span(class = "ctt-note", "Click a row to inspect the item.")),
              DTOutput("distractor_overview"),
              br(),
              div(class = "ctt-note",
                  "Non-functioning: option chosen by < 5% of examinees. Positive option-total r: ",
                  "a wrong option that high scorers prefer; review the item (Haladyna & Downing, 1993).")
            ),
            column(7,
              div(class = "ctt-panel",
                uiOutput("distractor_title"),
                DTOutput("distractor_table"),
                uiOutput("distractor_note"),
                plotOutput("distractor_plot", height = "280px")
              )
            )
          )
        )
      )
    ),
    
    # =====================================================
    # 3. RELIABILITY
    # =====================================================
    tabPanel(
      title = tagList(icon("check-double"), "Reliability"),
      value = "reliability_tab",
      div(class = "ctt-page",
        fluidRow(
          column(5,
            uiOutput("reliability_box")
          ),
          column(7,
            div(class = "ctt-section-head",
                h4("Alpha if item deleted"),
                span(class = "ctt-note", "Red bars: removing the item would raise α.")),
            plotOutput("alpha_if_deleted_plot", height = "340px")
          )
        )
      )
    ),
    
    # =====================================================
    # 4. SCORES
    # =====================================================
    tabPanel(
      title = tagList(icon("chart-area"), "Scores"),
      value = "scores_tab",
      div(class = "ctt-page",
        tabsetPanel(
          type = "pills",
          tabPanel("Distribution",
            br(),
            fluidRow(
              column(7, plotOutput("score_histogram", height = "330px")),
              column(5, DTOutput("score_descriptives_out"))
            )
          ),
          tabPanel("Person scores",
            br(),
            div(class = "ctt-note",
                "Observed score ± 1.96 × SEM (95% band) and Kelley's estimated true score, ",
                "T̂ = α(X − M) + M (Crocker & Algina, 1986; Kelley, 1923)."),
            br(),
            DTOutput("person_scores_out")
          ),
          tabPanel("Score new data",
            br(),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                p("Upload new examinees to score them with the items and key of the current analysis."),
                downloadButton("download_ctt_template", "Data template (Excel)", class = "btn-info btn-sm btn-block"),
                br(),
                fileInput("ctt_newdata", "Upload new data (Excel/CSV)", accept = c(".csv", ".xlsx", ".xls")),
                selectInput("ctt_conf_level", "Confidence level of SEM band:",
                            choices = c("68%" = 0.68, "90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                            selected = 0.95),
                actionButton("ctt_score_newdata_btn", "Calculate scores", icon = icon("calculator"), class = "btn-success btn-block")
              ),
              mainPanel(
                width = 9,
                uiOutput("ctt_newdata_check"),
                fluidRow(
                  column(7,
                    div(style = "text-align: right; margin-bottom: 5px;",
                        downloadButton("download_ctt_newscores", "Download scores (.csv)", class = "btn-primary btn-sm")),
                    DTOutput("ctt_newscores_table")
                  ),
                  column(5, plotOutput("ctt_newscores_plot", height = "280px"))
                )
              )
            )
          )
        )
      )
    ),
    
    # =====================================
    # 5. REPORT
    # =====================================
    tabPanel(
      title = tagList(icon("file-alt"), "Report"),
      value = "report_tab",
      div(class = "ctt-page",
        div(class = "ctt-toolbar",
            actionButton("ctt_generate_preview", tagList(icon("sync"), " Generate preview"), class = "btn btn-success"),
            downloadButton("download_report_ctt", "Download HTML report", class = "btn btn-primary")),
        div(class = "ctt-frame", uiOutput("ctt_report_preview_frame"))
      )
    ),
    
    # =====================================
    # 6. SETTINGS
    # =====================================
    tabPanel(
      title = tagList(icon("sliders-h"), "Settings"),
      value = "settings_tab_ctt",
      div(class = "ctt-page",
        fluidRow(
          column(5,
            wellPanel(
              tags$h4("Number formatting"),
              radioButtons("ctt_dec_sep", "Decimal separator:",
                           choices = c("Dot (.)" = ".", "Comma (,)" = ","),
                           selected = ".", inline = TRUE),
              selectInput("ctt_digits", "Decimal places:",
                          choices = c("2" = 2, "3" = 3, "4" = 4), selected = 3, width = "150px"),
              tags$p(class = "ctt-note",
                     "Applies to tables, plots, the HTML report and the exported score file ",
                     "(a comma gives a semicolon-separated .csv). Uploaded .csv files delimited by ';' ",
                     "are read with a decimal comma automatically.")
            )
          )
        )
      )
    ),
    
    # ===== INFO ======
    tabPanel(
      title = tagList(icon("info-circle"), "About"),
      fluidRow(
        column(
          width = 8, offset = 2,
          br(),
          div(
            style = "text-align:center;",
            tags$hr(),
            tags$h5("measureR Was Developed By:"),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=PSAwkTYAAAAJ&hl=id",
                target = "_blank",
                "Dr. Hasan Djidu, M.Pd."),
              tags$br(),
              "Universitas Sembilanbelas November Kolaka"
            ),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=24m-AysAAAAJ&hl=id",
                target = "_blank",
                "Prof. Dr. Heri Retnawati, M.Pd."),
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
          h4("Methodological References"),
          tags$ul(class = "ctt-refs",
            tags$li("Allen, M. J., & Yen, W. M. (1979). ", tags$i("Introduction to measurement theory"), ". Brooks/Cole."),
            tags$li("Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. ", tags$i("Psychometrika, 16"), "(3), 297\u2013334."),
            tags$li("Crocker, L., & Algina, J. (1986). ", tags$i("Introduction to classical and modern test theory"), ". Holt, Rinehart & Winston."),
            tags$li("Ebel, R. L., & Frisbie, D. A. (1991). ", tags$i("Essentials of educational measurement"), " (5th ed.). Prentice Hall."),
            tags$li("George, D., & Mallery, P. (2003). ", tags$i("SPSS for Windows step by step"), " (4th ed.). Allyn & Bacon."),
            tags$li("Haladyna, T. M., & Downing, S. M. (1993). How many options is enough for a multiple-choice test item? ", tags$i("Educational and Psychological Measurement, 53"), "(4), 999\u20131010."),
            tags$li("Kelley, T. L. (1923). ", tags$i("Statistical method"), ". Macmillan."),
            tags$li("Nunnally, J. C., & Bernstein, I. H. (1994). ", tags$i("Psychometric theory"), " (3rd ed.). McGraw-Hill.")
          ),
          h4("References (R Packages)"),
          uiOutput("package_references_ctt"),
          br(),
          div(
            style = "text-align:center;",
            tags$p(
              style = "font-size:13px; color:#777;",
              format(Sys.Date(), "%Y"), 
              "measureR. Hasan Djidu. All rights reserved."
            ) ))
      )
    )
  )
}
