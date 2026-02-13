ctt_ui <- function(project) {
  
  tabsetPanel(
    id = "main_tab_ctt",
    
    # =====================================================
    # PREPARE DATA
    # =====================================================
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
            "data_source_ctt",
            "Select Data Type:",
            choices = c(
              "UPLOAD Scored Data" = "upload_scored",
              "UPLOAD Response with Key" = "upload_respkey",
              "Built-in Dichotomous" = "diko",
              "Built-in Polytomous" = "poli",
              "Built-in Response with Key" = "respkey"
            )
            ),
          
          conditionalPanel(
            condition = "input.data_source_ctt == 'upload_scored' || input.data_source_ctt == 'upload_respkey'",
            fileInput("datafile_ctt", "Upload Data (csv / xlsx)")
          ),
          
          tags$hr(),
          uiOutput("item_select_ui_ctt"),
          
          actionButton(
            "run_ctt",
            label = tagList(icon("play"), "Run CTT Analysis"),
            class = "btn btn-success btn-block",
            style = "width:100% !important;"
          )
        ),
        
        mainPanel(
          width = 9,
          # ================= OVERVIEW =================
          div(
            style = "
              background:#e9f2ff;
              border-left:5px solid #0d6efd;
              padding:14px;
              border-radius:0px;
              margin-bottom:15px;
            ",
            tags$h4("Classical Test Theory – Overview"),
            tags$p(
              "This module provides classical item analysis.",
            ),
            tags$ul(
              tags$li("Item difficulty & discrimination"),
              tags$li("Reliability (α) & Standard Error Measurement (SEM)"),
              tags$li("Distractor analysis (for response data with key)")
            )
          ),
          
          # ================= EXAMPLE DATA =================
          fluidRow(
            tags$details(
              tags$summary(
                style = "font-weight:bold; cursor:pointer;",
                "Click to view example of required data format"
              ),              
              br(),
            column(4,
                   tags$b("Dichotomous Score"),
                   datatable(
                     data.frame(
                       ID = c("S1","S2","S3"),
                       I1 = c(1,0,1),
                       I2 = c(1,1,0),
                       I3 = c(0,1,0)
                     ),
                     options = list(dom = "t"),
                     rownames = FALSE
                   )
            ),
            column(4,
                   tags$b("Polytomous Score"),
                   datatable(
                     data.frame(
                       ID = c("S1","S2","S3"),
                       I1 = c(1,3,2),
                       I2 = c(4,2,3),
                       I3 = c(3,3,2)
                     ),
                     options = list(dom = "t"),
                     rownames = FALSE
                   )
            ),
            column(4,
                   tags$b("Response with Key"),
                   datatable(
                     data.frame(
                       ID = c("KEY","S1","S2"),
                       I1 = c("A","A","B"),
                       I2 = c("C","C","D"),
                       I3 = c("B","B","D")
                     ),
                     options = list(dom = "t"),
                     rownames = FALSE
                   )
            )
            )
          ),
          br(),
          
          uiOutput("data_type_badge"),
          hr(),
          
          h5("Data Preview"),
          DTOutput("data_preview_ctt"),
          
          br(),
          h5("Data Structure Summary"),
          DTOutput("data_summary_ctt")
        )
      )
    ),
    
    # =====================================================
    # RESULTS
    # =====================================================
    tabPanel(
      title = tagList(icon("chart-bar"), "Item Analysis"),
      value = "iteman_alysis_tab", 
      column(12,
        column(3,
               DTOutput("item_table")
        ),
        column(9,
               column(5,
                      #h4("Item Characterstic Curve"),
                      plotOutput("icc_ctt")
               ),
               column(4,
                      br(),
                      conditionalPanel(
                        condition = "output.show_distractor == true",
                        #h4("Distractor Analysis"),
                        uiOutput("item_dist"),
                        DTOutput("distractor_table")
                      ),
                      br(),
                      uiOutput("item_selected_ui")
               )
              
        )),
        column(12,
               tags$b("Reliability & SEM"),
               uiOutput("reliability_box"),
               br(),
               HTML(paste0(
                 "<b>References</b><br>",
                 "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests. <i>Psychometrika, 16</i>(3), 297–334.<br>",
                 "Crocker, L., & Algina, J. (1986). <i>Introduction to classical and modern test theory</i>. Holt, Rinehart & Winston.<br>",
                 "Willse, J. T. (2018). <i>CTT: Classical Test Theory Functions</i> [R package]."
               ))
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
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
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
