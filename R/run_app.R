#' Launch the shiny app of the package EpiSimR
#'
#' @return No return value, called for side effects. This function launches a Shiny application.
#' @examples
#' if (interactive()) {
#'   library(EpiSimR)
#'   run_app()
#' }
#'
#' @import deSolve
#' @import shiny
#' @import dplyr
#' @import openxlsx
#' @importFrom graphics matplot legend
#' @importFrom DT datatable renderDT
#' @importFrom shinythemes shinytheme
#' @importFrom grDevices dev.off png

#' @export
run_app <- function() {

  # Get the path of the Shiny app inside the package
  app_dir <- system.file("app", package = "EpiSimR")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing the package.", call. = FALSE)
  }

  # Run the Shiny app
  shiny::runApp(app_dir, display.mode = "auto")
}
