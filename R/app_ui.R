#' @title The application User-Interface
#' @param request Internal parameter for `Shiny`. DO NOT REMOVE.
app_ui <- function(request) {
  tagList(
    # External resources
    golem_add_external_resources(),
    page(
      title = "Open Trade Statistics",
      # layout = "fluid-vertical",
      layout = "navbar-sticky-dark",
      theme = "light",
      color = "teal",
      show_theme_button = FALSE,
      navbar = navbar_menu(
        brand = sidebar_brand(text = "Open Trade Statistics", href = "./"),
        menu_item("Welcome", tab_name = "welcome", icon = "home"),
        menu_item("Countries", tab_name = "co", icon = "globe-filled"),
        menu_item("Products", tab_name = "pr", icon = "shopping-cart-filled"),
        menu_item("Cite", tab_name = "cite", icon = "book-filled")
      ),
      body = body(
        useShinyjs(),
        br(),
        tab_items(
          tab_item(
            tab_name = "welcome",
            mod_welcome_ui("welcome")
          ),
          tab_item(
            tab_name = "co",
            mod_countries_ui("co")
          ),
          tab_item(
            tab_name = "pr",
            mod_products_ui("pr")
          ),
          tab_item(
            tab_name = "cite",
            mod_cite_ui("cite")
          )
        )
      ),
      footer = footer(left = "Made by Mauricio 'Pacha' Vargas Sepulveda", right = paste("Open Trade Statistics", get_year()))
    )
  )
}

#' @title Add external Resources to the Application
#' @description This function is internally used to add external
#'  resources inside the Shiny application.
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Open Trade Statistics"
    ),

    # Include custom JS that equalizes card heights where needed and waiter helper
    tags$script(src = "www/tabler-waiter.js"),
    tags$script(src = "www/progress-bar.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/tabler.css"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/waiter.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/progress-bar.css")
  )
}
