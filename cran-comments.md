## Test environments
- local macOS (aarch64, R 4.6.1)

## R CMD check results
0 errors | 0 warnings | 1 note

* NOTE: Namespaces in Imports field not imported from. The package ships a
  Shiny application in `inst/app` that attaches/uses these packages, so they
  are declared in Imports.

## Notes
This version redesigns the Classical Test Theory module of the Shiny
application: a new layout (data, item analysis, distractors, reliability,
scores, report), item evaluation with published cut-offs, a dedicated
distractor analysis, person scores with an SEM-based band, a decimal-separator
setting, and a rewritten HTML report. The built-in polytomous example data are
now simulated.
