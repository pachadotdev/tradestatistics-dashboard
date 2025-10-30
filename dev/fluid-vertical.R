# change layout index
i <- 4
show_theme_button <- T

document()
load_all()

library(shiny)
library(d3po)
library(tabler)

layouts <- c(
    "boxed",
    "combo",
    "condensed",
    "fluid-vertical",
    "fluid",
    "horizontal",
    "navbar-dark",
    "navbar-overlap",
    "navbar-sticky",
    "rtl",
    "vertical-right",
    "vertical-transparent",
    "vertical"
)

colors <- c(
    "blue",
    "azure",
    "indigo",
    "purple",
    "pink",
    "red",
    "orange",
    "yellow",
    "lime",
    "green",
    "teal",
    "cyan"
)

themes <- c("light", "dark")

# for (i in seq_along(layouts)) {
my_layout <- layouts[i]

print(my_layout)

my_color <- colors[11]
my_theme <- themes[1]

# Other layouts use a single navbar
main_navbar <- navbar_menu(
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

ui <- page(
    theme = my_theme,
    color = my_color,
    title = paste(toupper(my_layout), "Layout"),
    layout = my_layout,
    show_theme_button = show_theme_button,
    navbar = main_navbar,
    body = list(
        # Page header
        page_header(
            title_text = paste(toupper(my_layout), "layout"),
            pretitle_text = "Overview"
        ),
        # Page body content
        div(
            class = "page-body",
            div(
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
                ),
                column(
                    6,
                    card(
                        title = "Buttons here",
                        footer = "Footer.",
                        # Dummy buttons for testing various variants of button()

                        # Basic colors
                        button("Primary", color = "primary"),
                        button("Secondary", color = "secondary"),
                        button("Success", color = "success"),

                        # Outline and sizes
                        button("Outline", color = "primary", outline = TRUE),
                        button("Large", color = "primary", size = "lg"),
                        button("Small", color = "primary", size = "sm"),

                        # Icons
                        button("Upload", icon = "upload", color = "primary"),
                        button(
                            "Download",
                            icon = "download",
                            color = "success"
                        ),

                        # Pill and square
                        button("Pill", color = "secondary", pill = TRUE),
                        button("Square", color = "danger", square = TRUE),

                        # Loading and full width
                        button("Loading", color = "primary", loading = TRUE),
                        button("Full width", color = "primary", block = TRUE),

                        # Disabled and link
                        button("Disabled", color = "primary", disabled = TRUE),
                        button("As link", href = "#", color = "primary")
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

        if (my_theme == "light") {
            axis_color <- "#000"
            tooltip_color <- "#fff"
        } else {
            axis_color <- "#fff"
            tooltip_color <- "#000"
        }

        d3po(sim) %>%
            po_scatter(daes(x = x, y = y, group = letter)) %>%
            po_labels(title = "Weight Distribution by Type") %>%
            po_background("transparent") %>%
            po_theme(axis = axis_color, tooltips = tooltip_color)
    })
}
# }

shinyApp(ui, server)
