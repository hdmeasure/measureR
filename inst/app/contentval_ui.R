# =========================================
# Content Validity UI
# =========================================

contentval_ui <- function(project) {
  
  tabsetPanel(
    id = "main_tab_contentval",
    
    # =====================================
    # Prepare Data
    # =====================================
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          actionButton(
            "go_home",
            label = tagList(icon("home"), "Main Menu"),
            class = "btn btn-danger btn-block",
            style = "width:100% !important;"
          ),
          br(),
          
          selectInput(
            "data_source_cv",
            "Select Data Source:",
            choices = c(
              "Upload Data" = "upload",
              "Built-in (Simulated Data)" = "sim"
            )
          ),
          
          conditionalPanel(
            condition = "input.data_source_cv == 'upload'",
            fileInput("datafile_cv", "Upload Data (csv / xlsx)"),
            uiOutput('id_select')
          ),
          
          conditionalPanel(
            condition = "input.data_source_cv == 'sim'",
            numericInput("n_item", "Number of Items:", 10, min = 2),
            numericInput("n_expert", "Number of Experts:", 5, min = 2),
          ),
          selectInput(
            "rating_scale",
            "Rating Scale:",
            choices = c(
              "Dichotomous (0/1)",
              "1–4 Likert",
              "1–5 Likert",
              "1–6 Likert",
              "1–7 Likert"
            )
          ),
        ),
        
        mainPanel(
          width = 9,
          
          # ===== Overview =====
          div(
            style = "
              background:#e9f2ff;
              border-left:5px solid #0d6efd;
              padding:14px;
              margin-bottom:15px;
              border-radius:0px;
            ",
            tags$h4("Content Validity – Overview"),
            tags$p(
              "This module evaluates expert agreement before conducting ",
              tags$b("EFA/CFA, CTT, or IRT"), "."
            ),
            tags$ul(
              tags$li("Aiken’s V – ordinal expert ratings"),
              tags$li("CVR – dichotomous judgments"),
              tags$li("I-CVI & S-CVI – item- and scale-level indices")
            )
          ),
          
          # ===== Example format =====
          tags$details(
            tags$summary(
              style = "font-weight:bold; cursor:pointer;",
              "Click to view example of required data format"
            ),
            br(),
            DT::DTOutput("example_table")
          ),
          
          br(),
          uiOutput("data_warning"),
          br(),
          uiOutput("data_type_message"),
          
          hr(),
          h5("Your Data Preview"),
          DT::DTOutput("data_preview_cv")
        )
      )
    ),
    
    # =====================================
    # Aiken
    # =====================================
    tabPanel(
      title = "Aiken’s V",
      column(12,
             br(),
        column(6, DT::DTOutput("aiken_table")),
        column(6,
               uiOutput("aiken_interpretation"),
               hr(),
               tags$b("Reference"),
               tags$p(
                 "Aiken, L. R. (1985). Three coefficients for analyzing the reliability ",
                 "and validity of ratings. ",
                 tags$i("Educational and Psychological Measurement, 45"),
                 "(1), 131–142."
               )
        )
      )
    ),
    
    # =====================================
    # CVR
    # =====================================
    tabPanel(
      title = "CVR (Lawshe)",
      column(12,
             br(),
        column(6, DT::DTOutput("cvr_table")),
        column(6,
               uiOutput("cvr_interpretation"),
               hr(),
               tags$b("Reference"),
               tags$p(
                 "Lawshe, C. H. (1975). A quantitative approach to content validity. ",
                 tags$i("Personnel Psychology, 28"),
                 ", 563–575."
               )
        )
      )
    ),
    
    # =====================================
    # CVI
    # =====================================
    tabPanel(
      title = "I-CVI & S-CVI",
      column(12,
             br(),
        column(6,
               DT::DTOutput("icvi_table"),
               br(),
               DT::DTOutput("scvi_table")
        ),
        column(6,
               uiOutput("cvi_interpretation"),
               hr(),
               tags$b("Reference"),
               tags$p(
                 "Lynn, M. R. (1986). Determination and quantification of content validity. ",
                 tags$i("Nursing Research, 35"),
                 "(6), 382–385."
               )
        )
      )
    ),
    # ===== INFO ======
    tabPanel(
      title = tagList(icon("info-circle"), "About"),
      column(12,
             br(),
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
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
          h4("References (R Packages)"),
          uiOutput("package_references_contentval"),
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
