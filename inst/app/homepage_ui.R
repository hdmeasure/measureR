homepage_ui <- function() {
  
  fluidPage(
    
    # =====================================
    # HERO SECTION
    # =====================================
    div(class = "landing",
        div(class = "hero",
            div(class = "welcome", "WELCOME TO"),
            div(class = "app-name", "measureR"),
            
            div(
              style = "
                background-color: #f5f5f5;
                border-left: 4px solid #4c8bf5;
                padding: 6px 14px;
                border-radius: 6px;
                margin-top: 6px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.1);
                font-size: 12px;
                max-width: 900px;
              ",
              tags$p(
                style = "font-size: 12px; margin-top: 2px; color: darkblue;",
                "An integrated R Shiny application for educational and psychological measurement, ",
                "supporting content validity, dimensionality analysis, classical test theory (CTT), ",
                "and item response theory (IRT) through an intuitive graphical interface."
              ),
              HTML(
                "<strong>How to Cite:</strong><br>
                 Djidu, H. (2026).
                 <em>measureR: Shiny application for educational and psychological measurement</em>.
                 GitHub repository.<br>
                 <a href='https://github.com/hdmeasure/measureR' target='_blank'>
                 https://github.com/hdmeasure/measureR
                 </a>"
              )
            )
        ),
        br(),
        div(class = "subtitle",
            "Choose the analysis module you want to work on:")
    ),
    
    br(),
  
    
    
    # =====================================
    # QUADRANT CONTAINER (NO AXIS LINES)
    # =====================================
    div(class = "quad-container",
        
        # ===== BIG CENTER LOGO =====
        div(class = "center-logo large-logo",
            tags$img(src = "logomeasureR.png")
        ),
        
        # =================================
        # 1️⃣ Content Validity (Top-Left)
        # =================================
        div(class = "quad-card align-tl", id = "card_val",
            onclick = "$('#go_contentval').click();",
            
            div(class = "project-icon",
                tags$i(class = "fa-solid fa-check-double")),
            div(class = "project-title", "Content Validity"),
            div(class = "project-desc",
                "Expert agreement analysis using Aiken’s V, CVR (Lawshe), ",
                "I-CVI, and S-CVI. Recommended as the first step in instrument development."
            ),
            actionButton("go_contentval", "Content Validity", class = "btn-pill")
        ),
        
        # =================================
        # 2️⃣ Dimensionality & Assumptions (Top-Right)
        # =================================
        div(class = "quad-card align-tr", id = "card_fa",
            
            div(class = "project-icon",
                tags$i(class = "fa-solid fa-sitemap")),
            div(class = "project-title", "Dimensionality & Assumptions"),
            div(class = "project-desc",
                "Evaluation of latent structure and measurement assumptions ",
                "through EFA, CFA"
            ),
            
            div(style = "display:flex; gap:8px; width:100%;",
                actionButton("go_efa", "EFA", class = "btn-pill", style="width:50%"),
                actionButton("go_cfa", "CFA", class = "btn-pill", style="width:50%")
            )
        ),
        
        # =================================
        # 3️⃣ Classical Test Theory (Bottom-Left)
        # =================================
        div(class = "quad-card align-bl", id = "card_ctt",
            onclick = "$('#go_ctt').click();",
            
            div(class = "project-icon",
                tags$i(class = "fa-solid fa-scale-balanced")),
            div(class = "project-title", "Classical Test Theory"),
            div(class = "project-desc",
                "Item difficulty, discrimination indices, reliability ",
                "(α), SEM, & distractor analysis using classical approaches."
            ),
            actionButton("go_ctt", "CTT", class = "btn-pill")
        ),
        
        # =================================
        # 4️⃣ Item Response Theory (Bottom-Right)
        # =================================
        div(class = "quad-card align-br", id = "card_lta",
            onclick = "$('#go_lta').click();",
            
            div(class = "project-icon",
                tags$i(class = "fa-solid fa-brain")),
            div(class = "project-title", "Item Response Theory"),
            div(class = "project-desc",
                "Model-based estimation of item and person parameters ",
                "for dichotomous and polytomous item responses."
            ),
            actionButton("go_lta", "IRT", class = "btn-pill")
        )
    ),
    
    # =====================================
    # FOOTER
    # =====================================
    div(
      style = "text-align:center;",
      tags$p(
        style = "font-size:13px; color:#777;",
        format(Sys.Date(), "%Y"),
        " measureR. Hasan Djidu. All rights reserved."
      )
    )
  )
}
