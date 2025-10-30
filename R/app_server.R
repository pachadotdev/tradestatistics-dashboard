#' @title The application server-side
#' @param input,output,session Internal parameters for Shiny. DO NOT REMOVE.
app_server <- function(input, output, session) {
  # Modules ----
  mod_countries_server("co")
  mod_products_server("pr")
  mod_cite_server("cite")

  # Bookmarking ----

  observe({
    # Trigger this observer every time an input changes
    # strip shiny related URL parameters
    rvtl(input)
    setBookmarkExclude(c(
      "co-fmt", "pr-fmt", "waiter_shown", "waiter-content_waiter_hidden", "co-go", "pr-go"
    ))
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
}
