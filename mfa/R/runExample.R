#' runExample
#'
#' run a shiny app for multiple factor analysis on wines tasting experiment
#' @importFrom shiny runApp
#' @export

runExample <- function() {
  appDir <- system.file("shiny-examples", "wines_app", package = "mfa")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mfa`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}