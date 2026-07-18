#' @title Run the Application
#' @export
run_app <- function() {
  # Read credentials from file excluded in .gitignore
  readRenviron("/tradestatistics/credentials.txt")

  tablerApp(
    ui = app_ui(),
    server = app_server
  )
}
