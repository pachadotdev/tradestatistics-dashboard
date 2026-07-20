#' @title The application User-Interface
app_ui <- function() {
  tagList(
    # External resources
    golem_add_external_resources(),
    page(
      title = "Open Trade Statistics",
      # layout = "fluid-vertical",
      layout = "navbar-sticky-dark",
      theme = "light",
      base = "stone",
      color = "teal",
      radius = 1.5,
      show_theme_button = FALSE,
      navbar = navbar_menu(
        brand = sidebar_brand(text = "Open Trade Statistics", href = "./"),
        menu_item("Welcome", tab_name = "welcome", icon = "home"),
        menu_item("Countries", tab_name = "co", icon = "globe-filled"),
        menu_item("Sectors", tab_name = "se", icon = "shopping-cart-filled"),
        menu_item("Cite", tab_name = "cite", icon = "book-filled")
      ),
      body = body(
        tags$br(),
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
            tab_name = "se",
            mod_sectors_ui("se")
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
#'  resources inside the application.
golem_add_external_resources <- function() {
  addResourcePath(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$title("Open Trade Statistics"),
    tags$link(rel = "icon", href = "www/favicon.ico")
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/tabler.css")
  )
}
