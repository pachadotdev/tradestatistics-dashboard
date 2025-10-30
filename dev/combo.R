if (!require("d3po")) {
  install.packages("d3po", repos = "https://pachadotdev.r-universe.dev")
}

library(shiny)
library(tabler)
library(d3po)

# Full menu for the sidebar
sidebar_nav <- navbar_menu(
  brand = sidebar_brand(text = "Examples", href = "./"),
  menu_item("Home", icon = "home"),
  menu_dropdown(
    "Layout",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Boxed", "./"),
      c("Combined", "./"),
      c("Condensed", "./"),
      c("Fluid", "./"),
      c("Fluid vertical", "./"),
      c("Horizontal", "./"),
      c("Navbar dark", "./"),
      c("Navbar overlap", "./"),
      c("Navbar sticky", "./"),
      c("Right vertical", "./"),
      c("RTL mode", "./"),
      c("Vertical", "./"),
      c("Vertical transparent", "./")
    )
  )
)

# Simplified menu for the top navbar (just labels, no icons for simplicity)
top_nav <- navbar_menu(
  menu_item("Button 1", icon = NULL),
  menu_dropdown(
    "Button 2",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Button 3", "./")
    )
  )
)

# Combine both for combo layout
main_navbar <- list(side = sidebar_nav, top = top_nav)

ui <- page(
  theme = "light",
  color = "teal",
  title = "Combo Layout",
  layout = "combo",
  show_theme_button = FALSE,
  navbar = main_navbar,
  body = list(
    # Page header
    page_header(
      title_text = "Combo Layout",
      pretitle_text = "Overview"
    ),
    # Page body content
    shiny::tags$div(
      class = "page-body",
      shiny::tags$div(
        class = "container-xl",
        column(
          6,
          card(
            title = "My title",
            footer = "Footer.",
            p("My text"),
            p("More text", class = "text-muted"),
            d3po_output("plot", width = "100%", height = "500px")
          )
        )
      )
    )
  ),
  footer = footer(
    left = "Tabler",
    right = shiny::tags$span("v1.4.0")
  )
)

server <- function(input, output, session) {
  output$plot <- render_d3po({
    set.seed(123)

    sim <- data.frame(
      x = rnorm(100),
      y = rnorm(100),
      letter = sample(letters[1:3], 100, replace = TRUE)
    )

    # for light theme
    axis_color <- "#000"
    tooltip_color <- "#fff"

    # for dark theme
    axis_color <- "#fff"
    tooltip_color <- "#000"

    d3po(sim) %>%
      po_scatter(daes(x = x, y = y, group = letter)) %>%
      po_labels(title = "Weight Distribution by Type") %>%
      po_background("transparent") %>%
      po_theme(axis = axis_color, tooltips = tooltip_color)
  })
}

shinyApp(ui, server)
