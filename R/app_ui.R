#' @title The application User-Interface
#' @export
app_ui <- function() {
  tagList(
    # External resources
    add_external_resources(),
    page(
      title = "Open Trade Statistics",
      # layout = "fluid-vertical",
      layout = "navbar-sticky-dark",
      theme = "light",
      base = "stone",
      color = "teal",
      radius = 1.0,
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
add_external_resources <- function() {
  addResourcePath(
    "www",
    app_sys("app/www")
  )

  tags$head(
    tags$title("Open Trade Statistics"),
    tags$link(rel = "icon", href = "www/favicon.ico"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "www/tabler.css")
    # Companion to sync_year_slider() (utils_server.R): fixes the year range
    # mirrored in the URL by syncUrl() whenever the server corrects an
    # out-of-range year selection, since that only happens from real slider
    # interaction otherwise. Kept as app-level JS (not a tabler package
    # change) since it only concerns this dashboard's own "y" sliders.
    tags$script(HTML(
      "document.addEventListener('tabler:message', function (e) {
         var d = e.detail || {};
         if (d.type !== 'tabler-syncYearUrl') return;
         var msg = d.message || {};
         if (!msg.id || !msg.value) return;
         var sp = new URLSearchParams(window.location.search);
         sp.set(msg.id, msg.value[0] + '-' + msg.value[1]);
         var qs = sp.toString();
         history.replaceState(null, '', window.location.pathname + (qs ? '?' + qs : '') + window.location.hash);
       });"
    ))
  )
}
