#' @title Run the Shiny Application
#' @param ... arguments to pass to golem_opts.
#'  See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @export
run_app <- function(
    onStart = NULL,
    options = list(
      port = 3838,
      shiny.maxRequestSize = 100 * 1024^2
    ),
    enableBookmarking = "url",
    uiPattern = "/",
    ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
