#' Launch the Shiny app
#'
#' This function Launches the Shiny app included in the package EpiSimR.
#'
#' @import deSolve
#' @import shiny
#' @import dplyr
#' @import openxlsx
#' @importFrom graphics matplot legend
#' @importFrom DT datatable
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
