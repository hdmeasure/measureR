#' Launch the measureR Shiny Application
#'
#' This function starts the Shiny app included in the measureR package.
#'
#' @return Launches a Shiny application (no return value)
#' @examples
#' is.function(run_measureR)
#' @export
run_measureR <- function() {
  app_dir <- system.file("app", package = "measureR")
  if (app_dir == "") {
    stop("Could not find Shiny app. Try reinstalling the measureR package.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}