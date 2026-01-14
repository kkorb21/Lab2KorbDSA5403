#' Launch the Coin Shiny App
#'
#' Opens the interactive Shiny application included in the
#' **Lab2KorbDSA5403** package. The app provides dynamic visualization
#' of the discrete‑theta Bayesian coin model, including prior,
#' likelihood, posterior, and credible intervals.
#'
#' @name runCoinApp
#'
#' @return Launches a Shiny application.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   runCoinApp()
#' }
runCoinApp <- function() {
  app_dir <- system.file("app", package = "Lab2KorbDSA5403")
  shiny::shinyAppDir(appDir = app_dir)
}

