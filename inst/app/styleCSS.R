library(shiny)
# ==== styleCSS.R ====
styleCSS <- tags$head(
  # Import FontAwesome untuk ikon
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
  ),
  
  tags$style(HTML("
.container-fluid {
    padding-left: 5px !important;
    padding-right: 5px !important;
  }
  /* ================================
     STYLE UNTUK DATATABLE
  ================================ */
  table.dataTable {
    font-size: 12px !important;
    line-height: 0.7 !important;
    letter-spacing: -0.3px !important;
    border-collapse: collapse !important;
  }

  table.dataTable th,
  table.dataTable td {
    padding: 4px 6px !important;
  }

  table.dataTable thead th {
    background-color: darkcyan !important;
    color: white !important;
    height: 0.7 !important;
    font-weight: bold !important;
  }

  table.dataTable tbody tr:nth-child(even) {
    background-color: #f2f6fa !important;
  }

  /* ================================
     STYLE UNTUK TABEL BIASA
  ================================ */
  table, th, td {
    font-size: 12px !important;
    line-height: 0.7 !important;
    letter-spacing: -0.3px !important;
    border-collapse: collapse !important;
  }

  th {
    background-color: darkcyan !important;
    color: white !important;
    font-weight: bold !important;
  }

  tr:nth-child(even) {
    background-color: #f2f6fa !important;
  }

  td, th {
    padding: 4px 6px !important;
  }

  /* ================================
     STYLE UNTUK TOMBOL (BUTTON)
  ================================ */
  .btn {
    width: auto;
    padding: 5px 10px !important;
    font-size: 12px !important;
    line-height: 1 !important;
    border-radius: 8px !important;
    font-weight: 700 !important;
    transition: all 0.1s ease-in-out !important;
    display: block;
    margin: 0 auto;
  }

  .btn i {
    margin-right: 6px !important;
  }

  .btn:hover {
    box-shadow: 0 2px 6px rgba(0,0,0,0.2) !important;
  }

  .btn:active {
    transform: scale(0.97) !important;
    box-shadow: 0 1px 3px rgba(0,0,0,0.3) !important;
  }

  /* Warna Tombol Khusus */
  .btn-primary {
    background-color: #0077dd !important;
    border: none !important;
  }
  .btn-primary:hover {
    background-color: #0056b3 !important;
  }

  .btn-danger {
    background-color: #fd4084 !important;
    border: none !important;
  }
  .btn-danger:hover {
    background-color: #a71d2a !important;
  }

  .btn-success {
    background-color: #28a745 !important;
    border: none !important;
  }
  .btn-success:hover {
    background-color: #1e7e34 !important;
  }
  /* =======================================
     STYLE   DT BUTTONS
   ======================================= */

.dt-button {
  padding: 4px 10px !important;    
  font-size: 9px !important;        
  border-radius: 6px !important; 
  background-color: black !important;
  color: white !important;
  border: none !important;
  font-weight: 600 !important;
  margin: 3px !important;
  line-height: 1 !important;
}

/* Hover */
.dt-button:hover {
  background-color: #005bb0 !important;
}

/* Active */
.dt-button:active {
  transform: scale(0.97) !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
    font-size: 10px !important; 
    padding: 2px 6px !important;     
    border-radius: 6px !important;  
  }

  .dataTables_wrapper .dataTables_paginate {
    margin-top: 2px !important;      
  }

  /* ================================
     STYLE UNTUK INPUT (select, numeric, file)
  ================================ */
  select.form-control,
  input.form-control,
  .selectize-input,
  .selectize-dropdown-content {
    border-radius: 12px !important;
    font-size: 12px !important;
    line-height: 1.2 !important;
    border: 1px solid #ddd !important;
  }

  .selectize-input {
    padding: 6px 10px !important;
    min-height: 32px !important;
  }

  .selectize-dropdown-content {
    font-size: 12px !important;
  }

  /* ================================
     STYLE UNTUK HOMEPAGE / LANDING PAGE
  ================================ */
  body {
    background: #fff;
    font-family: 'Poppins', sans-serif;
    margin: 0;
    padding: 0;
  }

  .landing {
    text-align: center;
    margin-top: 25px;
  }

  .hero {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 5px;
  }

  .welcome {
    font-size: 16px;
    font-weight: 800;
    color: #444;
    letter-spacing: 1px;
  }

.app-name { font-size:36px; font-weight:900; margin-top:1px; color:transparent; -webkit-text-fill-color:black; -webkit-background-clip:text; background:none; }
.app-name { text-shadow:-2px -2px 3px #fff,2px -2px 3px #fff,-2px 2px 3px #fff,2px 2px 3px #fff,0 0 5px rgba(0,0,0,.45),0 0 10px rgba(0,0,0,.35),0 0 15px rgba(0,0,0,.25); }

  .subtitle{
    font-size: 13px;
    font-weight: 600;
    color: #444;
  }

  h4.page-subtitle {
    margin-top: 20px;
    color: #333;
  }

  .quad-container {
    position: relative;
    z-index: 0;
    width: 90vw;
    height: 60vh;
    margin: 10px auto;
    display: grid;
    grid-template-columns: 1fr 1fr;
    grid-template-rows: 1fr 1fr;
    gap: 10px;
    justify-items: center;
    align-items: center;
  }

  .quad-container::before,
  .quad-container::after {
    content: '';
    position: absolute;
    z-index: 1;
  }

.quad-container::before{
  top:50%;left:0;width:100%;height:3px;
  background:linear-gradient(90deg,#111827,#1e40af);
}

.quad-container::after{
  left:50%;top:0;width:3px;height:100%;
  background:linear-gradient(180deg,#f59e0b,#111827);
}

  .axis-label {
    position: absolute;
    font-weight: bold;
    color: blue;
    font-size: clamp(11px, 2vw, 16px);
    background: white;
    border-radius: 6px;
    z-index: 5;
  }

  .axis-label.x-top {
    top: 0;
    left: 50%;
    transform: translateX(-50%);
  }

  .axis-label.x-bottom {
    bottom: 0;
    left: 50%;
    transform: translateX(-50%);
  }

  .axis-label.y-left {
    left: 0;
    top: 50%;
    transform: translateY(-50%);
  }

  .axis-label.y-right {
    right: 0;
    top: 50%;
    transform: translateY(-50%);
  }

  .center-logo {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 6;
    padding: 6px;
  }

  .center-logo img {
    width: clamp(125px, 3vw, 170px);
    height: auto;
    border-radius: 50%;
  }

  .quad-card {
    width: 90%;
    height: 80%;
    border-radius: 16px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.15);
    padding: 1.5vw;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    align-items: center;
    text-align: center;
    z-index: 2;
    transition: transform .2s;
  }

  .quad-card:hover {
    transform: scale(1.05);
  }
#card_irt{background:#e8efff;border:2px solid #1e40af}
#card_ctt{background:#f3f4f6;border:2px solid #111827}
#card_fa {background:#ffedd5;border:2px solid #f59e0b}
#card_val{background:#fee2e2;border:2px solid #ef4444}

#card_irt .project-icon{background:#dbe7ff;color:#1e40af}
#card_ctt .project-icon{background:#e5e7eb;color:#111827}
#card_fa  .project-icon{background:#ffedd5;color:#f59e0b}
#card_val .project-icon{background:#fee2e2;color:#ef4444}

#card_irt .btn-pill{background:#1e40af}
#card_ctt .btn-pill{background:#111827}
#card_fa  .btn-pill{background:#f59e0b}
#card_val .btn-pill{background:#ef4444}



  .project-title {
    font-size: clamp(12px, 1.5vw, 27px);
    font-weight: 600;
    margin-bottom: 4px;
  }

  .project-desc {
    color: #444;
    font-size: clamp(12px, 1vw, 120px);
    line-height: 1.3em;
    padding: 0 .5vw;
  }

  .project-icon {
    font-size: clamp(25px, 2vw, 40px);
    width: clamp(40px, 4vw, 60px);
    height: clamp(40px, 4vw, 60px);
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 12px;
  }



  .btn-pill {
    display: block !important;
    width: 100% !important;
    padding: clamp(5px, .8vw, 10px) 0;
    border-radius: 8px;
    text-align: center;
    text-decoration: none;
    margin-top: 3px;
    color: #fff;
    font-size: clamp(9px, 1vw, 18px);
    transition: .1s;
  }

  .btn-pill:hover {
    opacity: .85;
    transform: scale(1.001);
  }
  
   /* Styling untuk labels */
         .shiny-input-container label {
           font-size: 12px !important;
           font-weight: normal !important;
           color: #555555 !important;
           margin-bottom: 3px !important;
         }
         
         /* Styling untuk input fields */
         .form-control {
           font-size: 12px !important;
           height: 30px !important;
           padding: 4px 8px !important;
         }
         
         /* Styling khusus untuk numeric input */
         input[type='number'] {
           font-size: 12px !important;
           height: 28px !important;

         }
         
         /* Styling untuk select input */
         select.form-control {
           font-size: 12px !important;
           height: 30px !important;
           padding: 4px 8px !important;
         }
/* === Style khusus untuk selectInput besar (itemtype) === */
.select-large .selectize-input {
  font-size: 18px !important;
  font-weight: bold !important;
  text-align: center !important;
  padding: 12px 16px !important;
  border-radius: 14px !important;
  background-color: lightgreen !important;
  border: 2px solid #cfcfcf !important;
  box-shadow: 0 3px 6px rgba(0,0,0,0.1) !important;
  transition: all 0.2s ease-in-out !important;
}

.select-large .selectize-input:hover {
  border-color: #007bff !important;
  box-shadow: 0 0 8px rgba(0,123,255,0.3) !important;
}

.select-large .selectize-input.focus {
  border-color: #0056b3 !important;
  box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25) !important;
}

.select-large .option {
  text-align: center !important;
  font-size: 16px !important;
  font-weight: bold !important;
}

/* === Style selectInput mini tanpa highlight pilihan (versi tinggi lebih pendek) === */
.select-mini .selectize-input {
  font-size: 12px !important;
  font-weight: 600 !important;
  text-align: center !important;
  justify-content: center !important;

  padding: 1px 6px !important;          
  line-height: 12px !important;  
  min-height: 18px !important;     
  height: 18px !important;
  
  border-radius: 8px !important;
  background: lightgreen !important;
  border: 1.5px solid #b8bced !important;
  box-shadow: 0 2px 3px rgba(0,0,0,0.07) !important;
  transition: all 0.2s ease-in-out !important;
}

.select-mini .selectize-input:hover {
  border-color: #756bff !important;
  box-shadow: 0 0 5px rgba(117,107,255,0.33) !important;
}

.select-mini .selectize-input.focus {
  border-color: #574cff !important;
  box-shadow: 0 0 0 0.15rem rgba(87,76,255,0.22) !important;
}

.select-mini .option {
  text-align: center !important;
  font-size: 12px !important;
  font-weight: 600 !important;
}

/* Hilangkan highlight */
.select-mini .selectize-dropdown .highlight {
  background: none !important;
  color: inherit !important;
  box-shadow: none !important;
}


         /* Styling untuk text input */
         input[type='text'] {
           font-size: 12px !important;
         }
         
         /* Styling untuk checkbox labels */
         .checkbox label {
           font-size: 12px !important;
           font-weight: normal !important;
           color: #555555 !important;
         }
  
  
  .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
      }
      .shiny-output-error { color: #dc3545; }
      .shiny-output-error:before { content: 'Error: '; }
      .small-input { max-width: 100px; }

  @media (max-width: 700px) {
    .quad-container {
      transform: scale(1);
      transform-origin: center top;
    }
  }
  
  .badge-warning{
  background:#fee2e2;
  border-left:4px solid #ef4444;
  color:#7f1d1d;
  padding:8px 12px;
  border-radius:8px;
  font-size:12px;
  line-height:1.4;
}

.badge-info{
  background:#e0e7ff;
  border-left:4px solid #3b82f6;
  color:#1e3a8a;
  padding:8px 12px;
  border-radius:8px;
  font-size:12px;
  line-height:1.4;
}

  

/* ===== CTT module ===== */
table.dataTable thead th{line-height:1.15 !important;}
.ctt-page{padding:14px 10px 4px 10px;}
.ctt-title{margin-top:0;font-weight:700;color:#0f172a;}
.ctt-lead{color:#64748b;margin-bottom:14px;}
.ctt-cards{display:flex;flex-wrap:wrap;gap:0;margin:0 0 14px 0;border:1px solid #e2e8f0;border-radius:10px;background:#fff;overflow:hidden;}
.ctt-card{flex:1 1 130px;padding:10px 16px;border-right:1px solid #e2e8f0;line-height:1.3;}
.ctt-card:last-child{border-right:none;}
.ctt-card-label{font-size:12px;color:#64748b;}
.ctt-card-value{font-size:24px;font-weight:700;color:#0f172a;}
.ctt-card-sub{font-size:12px;color:#475569;}
.ctt-dot{display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:5px;}
.ctt-panel{background:#f8fafc;border:1px solid #e2e8f0;border-radius:12px;padding:14px;margin-bottom:14px;}
.ctt-section-head{display:flex;align-items:baseline;justify-content:space-between;gap:10px;margin-bottom:6px;}
.ctt-section-head h4{margin:0;font-weight:700;}
.ctt-note{font-size:12px;color:#64748b;line-height:1.4;}
.ctt-step{font-weight:700;color:#0f766e;margin:14px 0 4px 0;font-size:13px;}
.ctt-guide{background:#fff;border:1px solid #e2e8f0;border-radius:10px;padding:8px 14px;}
.ctt-guide summary{cursor:pointer;font-weight:600;}
.ctt-guide-note{font-size:12px;color:#64748b;margin:8px 0 0 0;}
table.ctt-legend{width:100%;margin:6px 0;}
table.ctt-legend td{padding:2px 6px !important;line-height:1.3 !important;background:transparent !important;}
td.lg-good{background:#d4edda !important;font-weight:600;width:42%;}
td.lg-warn{background:#fff3cd !important;font-weight:600;width:42%;}
td.lg-bad{background:#f8d7da !important;font-weight:600;width:42%;}
.ctt-flag{padding:6px 12px;border-radius:8px;font-size:13px;margin-bottom:8px;line-height:1.4;}
.ctt-flag-ok{background:#f1f5f9;border-left:3px solid #94a3b8;}
.ctt-flag-warn{background:#fff7ed;border-left:3px solid #fd7e14;}
.ctt-flag-bad{background:#fef2f2;border-left:3px solid #dc3545;}
.ctt-badge{display:inline-block;color:#fff;padding:2px 12px;border-radius:12px;font-size:13px;font-weight:600;}
.ctt-toolbar{display:flex;gap:10px;margin-bottom:12px;}
.ctt-frame{border:1px solid #e2e8f0;border-radius:8px;background:#f9f9f9;padding:4px;}
.ctt-stat-table{width:100%;margin:6px 0 10px 0;}
.ctt-stat-table td{padding:5px 4px !important;line-height:1.3 !important;border-bottom:1px solid #e2e8f0;background:transparent !important;}
.ctt-stat-table td:last-child{text-align:right;font-weight:600;}
ul.ctt-refs{font-size:13px;color:#475569;padding-left:18px;}

.floating-widget {
  opacity: 0.6;
  transition: opacity 0.3s ease-in-out;
}
.floating-widget:hover {
  opacity: 1.0;
}

/* Professional Sidebar Styling for wellPanel */
.well {
  background-color: #f4f6f9 !important;
  border: 1px solid #e2e8f0 !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 20px rgba(0,0,0,0.05) !important;
  padding: 22px !important;
}

/* Fix dropdown transparency issues */
.dropdown-menu, .dropdown-menu .inner, .selectize-dropdown, .selectize-dropdown.form-control {
  background-color: #ffffff !important;
  opacity: 1 !important;
  z-index: 10000 !important;
}
  "))
)
