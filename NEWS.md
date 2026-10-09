# measureR 0.0.6

Focus: redesigned **Classical Test Theory (CTT)** module.

## Layout

The CTT module now follows the analysis workflow: **Data → Item Analysis → Distractors → Reliability → Scores → Report**, followed by Settings and About. Summary statistics are shown as one calm strip instead of coloured cards, the item table is more compact, and references moved to About.

* **Distractors** is a tab of its own (response with key): an overview of all items with non-functioning options (< 5%) and options with a positive option-total correlation, the option table of the selected item, and a plot of option choice by total-score group.
* **Reliability** combines alpha, SEM, the 95% band, and split-half reliability with the alpha-if-deleted chart.
* **Scores** groups the score distribution, person scores with the SEM band, and scoring of new data.

## CTT module

* **Item Analysis** is now a dashboard: summary cards (alpha, SEM, mean score, items to review), a full item table, and an item-detail panel (item selector, ICC, distractor analysis) beside it. Clicking a table row selects the item.
* Item table now reports difficulty (p), discrimination (item-total correlation), alpha-if-deleted, a flag for items whose removal raises alpha, and a **Retain / Revise / Eliminate** recommendation.
* Interpretation cut-offs follow published sources and are shown in the app and the report: discrimination (Ebel & Frisbie, 1991), difficulty (Allen & Yen, 1979; Crocker & Algina, 1986), Cronbach's alpha (George & Mallery, 2003; Nunnally & Bernstein, 1994), and distractor functioning (Haladyna & Downing, 1993).
* Discrimination for polytomous items now uses the item-total correlation (previously the biserial coefficient).
* **Built-in polytomous data replaced**: the old example was multiple-choice letters recoded A=1..D=4, which are not ordinal and gave meaningless results (alpha ~ 0.03). It is now a simulated 200 x 20 rating-scale dataset (1-4, graded response model).
* New "Analysis by Data Type" guide explaining how CTT differs for dichotomous, polytomous, and response-with-key data.
* **Prepare Data**: step-by-step sidebar, data summary cards (examinees, items, missing %), and tabs for preview, response frequencies, and the required data format.
* **Score Distribution & Reliability**: reliability card with SEM and 95% band, split-half (Spearman-Brown) cards, an alpha-if-deleted chart, histogram with mean line, and extended descriptives (skewness, kurtosis, SEM, alpha).
* Person scores now include an SEM-based confidence band (68/90/95/99%) and Kelley's estimated true score, in **Score New Data** and in a new Person Scores table.
* **Score New Data** now scores response-with-key data using the key, reports how many items matched, and shows a score histogram.
* Distractor analysis flags non-functioning distractors (< 5%) and distractors with positive item-total correlation.
* New **Settings** tab with the **decimal separator** (dot or comma) and decimal places. It applies to tables, cards, plots, the HTML report, and the exported score file (comma gives a semicolon-separated .csv). Semicolon-delimited uploaded .csv files are read with a decimal comma automatically.
* **Report** rewritten: the old report referenced fields that did not exist in the CTT result. The new report has an overview with an automatic summary, reliability (alpha, SEM, split-half, alpha-if-deleted chart), a colour-coded item table with cut-offs, a difficulty-discrimination plot, distractor analysis (response with key), score distribution, person scores with SEM band, the console output and references. The HTML download now renders on demand and no longer requires generating the preview first.
* The R console and AI assistant now receive a compact results summary instead of the raw result object.

# measureR 0.0.5

This release moves directly from 0.0.3 to 0.0.5 (version 0.0.4 was not released).
The application remains a **Shiny** app launched with `run_measureR()`.

## New features

* **AI assistant** in every module: ask free-form questions about the current
  results or generate a results summary. Supports Google Gemini, OpenAI, Groq,
  OpenRouter, and Anthropic; users supply their own API key in the Settings on
  the homepage. Reference documents (including PDF) can be attached as context.
* **R console panel**: a floating panel showing the R code behind each analysis,
  so results can be reproduced and validated outside the app.
* **HTML report export** for the Content Validity, CTT, EFA, CFA/SEM, and IRT
  modules, with an optional AI-generated summary embedded in the report.
* **IRT module** replaces the earlier LTA module, with item/test information
  visualisation and Excel/RDS export of results.
* **CFA/SEM**: variable aggregation (parceling) builder, scoring of new data,
  robust fit-index option, editable model-comparison table, and a method guide.
* Excel (`.xlsx`) export of results and data templates across modules.

## Other changes

* Added dependencies: `httr`, `jsonlite`, `pdftools`, `writexl`.
* Updated the homepage and styling.

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
