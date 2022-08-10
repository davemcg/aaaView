#' Wrapper that starts the aaaView Shiny App.
#'
#' @param app_path The path to the aaaView Rlibrary folder holding the
#' server.R and ui.R files.
#' @return Loads shiny app.
#' @export
launch <- function(app_path = system.file('app', package = "aaaView")) {
  shiny::runApp(app_path)
  }
