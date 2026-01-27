
# measureR <img src="man/figures/logomeasureR.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/measureR)](https://CRAN.R-project.org/package=measureR)
[![Downloads](https://cranlogs.r-pkg.org/badges/measureR)](https://cranlogs.r-pkg.org/badges/measureR)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/measureR)](https://cranlogs.r-pkg.org/badges/grand-total/measureR)

[![R-CMD-check](https://github.com/hdmeasure/measureR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hdmeasure/measureR/actions/workflows/R-CMD-check.yaml)

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

<!-- badges: end -->

measureR is an an R Shiny application for educational and psychological
measurement, including:

- **Content Validity (CV)**
- **Exploratory Factor Analysis (EFA)**
- **Confirmatory Factor Analysis (CFA)**
- **Classical Test Theory (CTT)**
- **Item Response Theory (IRT)**

All analyses can be performed **without writing any code**, making the
package accessible for researchers, students, and applied analysts.

------------------------------------------------------------------------

## Installation

``` r
# Install from CRAN (when available)
install.packages("measureR")

# Install development version from GitHub (optional)
remotes::install_github("hdmeasure/measureR")
```

------------------------------------------------------------------------

## Launch the Application

``` r
library(measureR)
run_measureR()
```

This opens the full Shiny application, including all LSA modules, data
upload, built-in datasets, interactive plots, and reporting features.

------------------------------------------------------------------------

## Features

### ✔ Content Validity (CV)

- Aiken’s V, CVR (Lawshe), I-CVI, and S-CVI/Ave computation.
- Automatic critical value comparison and interpretation badges.
- Clear tabular summaries and export-ready results.

### ✔ Exploratory Factor Analysis (EFA)

- KMO, Bartlett test, parallel analysis.
- Factor extraction with rotation.
- Factor scores and loading matrix export.
- Clean HTML summaries for clearer interpretation.

### ✔ Confirmatory Factor Analysis (CFA)

- Lavaan model editor.
- Fit measures, loadings, factor scores.
- Fully customized SEM path diagrams.

### ✔ Classical Test Theory (CTT)

- Item difficulty and discrimination indices.
- Test reliability (α), SEM, and score distribution analysis.
- Distractor analysis for multiple-choice items.
- Comprehensive item and test-level summary outputs.

### ✔ Item Response Theory (IRT)

- Supports dichotomous and polytomous items.
- Automatically fits Rasch, 2PL, 3PL (or PCM/GRM/GPCM).
- ICC plots, test information, factor scores.
- Multi-dimensional visualization with 3D surfaces and heatmaps.

------------------------------------------------------------------------

## Example Screenshots

### Home Page measureR

<figure>
<img src="man/figures/Homepage.png" alt="Homepage" />
<figcaption aria-hidden="true">Homepage</figcaption>
</figure>

### CV (Content Validity)

![CV1](man/figures/CV1.png) ![AIKEN](man/figures/AIKEN.png)
![CVR](man/figures/CVR.png) ![CVI](man/figures/CVI.png)

### CFA (Confirmatory Factor Analysis) & SEM (Structural Equation Modelling)

<figure>
<img src="man/figures/CFA1.png" alt="CFA1" />
<figcaption aria-hidden="true">CFA1</figcaption>
</figure>

<figure>
<img src="man/figures/CFA2.png" alt="CFA2" />
<figcaption aria-hidden="true">CFA2</figcaption>
</figure>

<figure>
<img src="man/figures/SEM.png" alt="SEM" />
<figcaption aria-hidden="true">SEM</figcaption>
</figure>

### CTT (Classical Test Theory)

![CTT1](man/figures/CTT1.png) ![CTT2](man/figures/CTT2.png)
![CTT3](man/figures/CTT3.png)

### IRT (Item Response Theory)

![IRT1](man/figures/IRT1.png) ![IRT2](man/figures/IRT2.png)

------------------------------------------------------------------------

## Citation

If you use measureR in publications, please cite:

Djidu, H. (2026). *measureR: An R Shiny application for educational and
psychological measurement. <https://github.com/hdmeasure/measureR>*

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests are welcome:

<https://github.com/hdmeasure/measureR/issues>

------------------------------------------------------------------------

## License

MIT License © 2026 Hasan Djidu
