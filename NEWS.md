# measureR 0.0.3

* **CFA/SEM Module Major Upgrade:**
  - Added **Structural Equation Modeling (SEM)** support within the CFA module. SEM models are automatically detected via the `~` operator in lavaan syntax.
  - Expanded estimator options from 6 to **18** (ML, GLS, WLS, DWLS, ULS, DLS, PML, MLM, MLMVS, MLMV, MLF, MLR, WLSM, WLSMVS, WLSMV, ULSM, ULSMVS, ULSMV).
  - Added **decimal separator** option (dot/comma) for international compatibility.
  - Added **Variances tab** with Heywood case detection and warning alerts.
  - Implemented **persistent model counter** for accurate model tracking across deletions.
  - Added **auto-generated model notes** that detect syntax changes between model runs.
  - Added **model deletion** (trash icon) in the fit comparison table.
  - Expanded **color schemes** from 4 to 12 (added Pastel, Greyscale, Earth, Vibrant, Monochrome, Sunset, Rose, Mint).
  - Added **collapsible plot settings** panels (General & Layout, Node & Edge Sizes, Measurement Model, Colors & Display, Fit Indices).
  - Added **Measurement Model** sub-controls: Curve, Split Layout, SubScale width/height.
  - Added **fit indices selector** for customizing which fit indices appear on the path plot.
  - Added **fit indices embedded in path plot** as a legend box.
  - Added **Download Plot (PNG)** button.
  - Added **Export Report (HTML)** button with auto-generated HTML reports using RMarkdown.
  - Improved **HTMT calculation** to handle duplicate factor names in model syntax.
  - Relaxed **acceptable fit status** criteria for more realistic model evaluation.
  - Added SPSS (.sav) file import support via the `haven` package.
* Added new dependencies: `shinyBS`, `flextable`, `kableExtra`, `knitr`, `rmarkdown`, `officer`, `magick`, `haven`, `scales`.

# measureR 0.0.2

* Enhanced the Content Validity module with several methodological improvements:
  - Integrated official critical values of Aiken’s V (Aiken, 1985) for rating scales ranging from 3 to 7 categories.
  - Implemented scale-based Aiken’s V computation using user-selected theoretical rating ranges instead of observed minimum–maximum values.
  - Added dynamic selection of the item ID column for uploaded datasets.
  - Improved visual decision indicators for CVR, Aiken’s V, and CVI tables (color-coded validity status).
  - Disabled inferential evaluation of Aiken’s V for dichotomous (0/1) data.
* Improved data validation and robustness for uploaded content validity datasets.
* Minor UI refinements in Content Validity and CTT modules.

# measureR 0.0.1
* Initial release to CRAN.
* Includes a full Shiny-based graphical user interface for:
  - Content Validity (CV)
  - Exploratory Factor Analysis (EFA)
  - Confirmatory Factor Analysis (CFA)
  - Classical Test Theory (IRT)
  - Item Response Theory (IRT)
* Includes interactive visualizations, downloadable outputs, and built-in example datasets.
* Provides `run_measureR()` as the main entry point for launching the application.
