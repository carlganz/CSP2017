#' Run Apps
#'
#' Functions for running shiny apps
#'
#' @param app Select which app to run
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#' run_app("A")
#' run_app(2)
#' run_app("C")
#' }
#'

run_app <- function(app = c("A", "B", "C")) {
  stopifnot(length(app)==1)
  if (app %in% 1:3) {
    app <- c("A", "B", "C")[app]
  }

  shiny::runApp(
    system.file(paste("App", app, sep = "_"),
                package = "CG.CSP17")
  )

}