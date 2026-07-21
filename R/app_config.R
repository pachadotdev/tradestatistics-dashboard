#' @title Access files in the current app
#' @description If you manually change your package name in the DESCRIPTION,
#'  don't forget to change it here too, and in the config file.
#' @param ... character vectors, specifying subdirectory and file(s)
#'  within your package. The default, none, returns the root of the app.
app_sys <- function(...) {
  system.file(..., package = "tradestatisticsdashboard")
}
