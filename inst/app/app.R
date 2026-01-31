# --- measureR  ---
# Author: Hasan Djidu

# ==== Load UI & Server Components ====
source("ui_module.R")
source("homepage_ui.R")
source("ctt_ui.R")
source("contentval_ui.R")
source("efa_ui.R")
source("cfa_ui.R")
source("lta_ui.R")
source("plotinfose.R")
source("lta_info_vis.R")

# ==== Load Logic / Server Modules ====
source("serverEFA.R")
source("serverCFA.R")
source("serverCTT.R")
source("serverContentval.R")
source("serverLTA.R")

# ==== Misc utilities ====
source("reference_list.R")
source("simDataDesc.R")
source("downloadPlot.R")
source("styleCSS.R")

# ==== Main Library ======
library(shiny)
library(shinyWidgets)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(tidyr)
options(shiny.maxRequestSize = 300 * 1024^2)  # 300 MB

# ===== UI =====
ui <- fluidPage(
  styleCSS,
  uiOutput("mainUI")
)
# ==== Server =====
server <- function(input, output, session) {

  observeEvent(TRUE, {
    showModal(modalDialog(
      title = NULL,
      div(
        style = "text-align:center; line-height:1.6;",
        tags$img(src = "logoMeasureR.png", height = "70px"),
        tags$h3("Welcome to measureR"),
        tags$p(
          "measureR is an interactive R Shiny application designed to support ",
          "educational and psychological measurement for research and teaching purposes."
        ),
        
        HTML(
          "
        If you use this application in academic or research work, please cite it as follows:<br><br>
        
        <strong>In-text citation:</strong>
        <span style='color:#2563eb;'>(Djidu, 2026)</span><br><br>
        
        <strong>Reference:</strong><br>
        Djidu, H. (2026).
        <em>measureR: Tools for educational and psychological measurement</em>.
        R package (Version 0.0.1).
        Available at:
        <a href='https://github.com/hdmeasure/measureR' target='_blank'>
        https://github.com/hdmeasure/measureR</a>.
        "
        )
      ),
      footer = modalButton("START"),
      easyClose = TRUE,
      size = "m"
    ))
  }, once = TRUE)
  
  
  # === reactive value to save current project ===
  project <- reactiveVal("home")

  # === observe event from homepage ===
  observeEvent(input$go_ctt, { project("ctt") })
  observeEvent(input$go_contentval, { project("contentval") })
  observeEvent(input$go_lta, { project("lta") })
  observeEvent(input$go_efa, { project("efa") })
  observeEvent(input$go_cfa, { project("cfa") })
  observeEvent(input$go_home, { project("home") })

  # === Render main UI ===
  output$mainUI <- renderUI({
    req(project())
    fluidPage(
      switch(
        project(),
        "ctt" = ctt_mod("ctt"),
        "contentval" = contentval_mod("contentval"),
        "lta" = lta_mod("lta"),
        "efa" = efa_mod("efa"),
        "cfa" = cfa_mod("cfa"),
        home_mod("home")
      )
    )
  })
  # === Logic server: jalankan modul sesuai project aktif ===
  observeEvent(project(), {
    current <- project()
    switch(
      current,
      "contentval" = server_contentval(input, output, session),
      "efa" = server_efa(input, output, session),
      "cfa" = server_cfa(input, output, session),
      "ctt" = server_ctt(input, output, session),
      "lta" = server_lta(input, output, session),
      NULL
    )
  })
      # Content Validity
    output$package_references_contentval <- renderUI({
      render_package_refs(c("shiny", "dplyr", "DT","readxl","ggplot2"))
    })
    # CTT
    output$package_references_ctt <- renderUI({
      render_package_refs(c("shiny", "CTT", "dplyr", "DT","readxl","ggplot2"))
    })
    # LTA / IRT
    output$package_references_lta <- renderUI({
      lta_reference <- list(
        Desjardins_Bulut_2018 = "Desjardins, C. D., & Bulut, O. (2018). <em>Handbook of educational measurement and psychometrics using R</em> (1st ed.). Chapman & Hall/CRC."
      )
      render_package_refs(c("shiny", "tidyverse", "mirt", "DT", "readxl", "dplyr","ggplot2"),
                          manual_refs = lta_reference)
    })
    # EFA
    output$package_references_efa <- renderUI({
      render_package_refs(c("shiny","tidyverse", "psych","DT", "readxl", "dplyr", "ggplot2"))
    })
    # CFA References
    output$package_references_cfa <- renderUI({
      # Define manual references
      manual_refs <- list(
        Alamer_2025 = "Alamer, A. (2025). Structural equation modeling (SEM) in L2 writing research: Simple tutorial and useful recommendations. <em>Research Methods in Applied Linguistics</em>, 4(2), 100202. <a href='https://doi.org/10.1016/j.rmal.2025.100202' target='_blank'>https://doi.org/10.1016/j.rmal.2025.100202</a>",
        Hair_2019 = "Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2019). <em>Multivariate data analysis</em> (Eighth edition). Cengage.",
        Hu_1999 = "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. <em>Structural Equation Modeling: A Multidisciplinary Journal</em>, 6(1), 1–55. <a href='https://doi.org/10.1080/10705519909540118' target='_blank'>https://doi.org/10.1080/10705519909540118</a>",
        Schumacker_2008 = "Schumacker, R. E., & Lomax, R. G. (2008). <em>A beginner's guide to structural equation modeling</em> (2nd ed.). Psychology Press.",
        Kline_2016 = "Kline, R. B. (2016). <em>Principles and practice of structural equation modeling</em> (4th ed.). Guilford Press.",
        Brown_2015 = "Brown, T. A. (2015). <em>Confirmatory factor analysis for applied research</em> (2nd ed.). Guilford Press."
      )

      render_package_refs(
        pkgs = c("shiny", "tidyverse", "psych", "lavaan", "DT", "readxl", "dplyr", "semPlot", "semptools",'semTools'),
        manual_refs = manual_refs
      )
    })

}

shinyApp(ui, server)
