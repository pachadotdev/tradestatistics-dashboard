#' @title Citation UI-side function
#' @param id Internal parameters for Shiny.
mod_cite_ui <- function(id) {
  ns <- NS(id)
  # Static metadata for the dashboard so reference managers (Zotero) can detect it
  tagList(
    # Highwire-style meta tags recognized by Zotero/other managers
    tags$head(
      tags$meta(name = "citation_title", content = "Open Trade Statistics"),
      tags$link(rel = "canonical", href = "https://shiny.tradestatistics.io"),
      tags$meta(
        name = "citation_author",
        content = "Vargas Sepulveda, Mauricio"
      ),
      tags$meta(name = "citation_publication_date", content = Sys.Date()),
      tags$meta(name = "citation_doi", content = "10.5281/zenodo.3738793"),
      tags$meta(name = "citation_publisher", content = "Open Trade Statistics"),
      tags$meta(
        name = "citation_abstract",
        content = "Interactive dashboard presenting UN Comtrade Plus based trade statistics."
      ),
      tags$script(
        type = "application/ld+json",
        HTML(
          toJSON(
            list(
              "@context" = "http://schema.org",
              "@type" = "SoftwareApplication",
              name = "Open Trade Statistics Dashboard",
              url = "https://shiny.tradestatistics.io",
              author = list(
                "@type" = "Person",
                name = "Vargas Sepulveda, Mauricio"
              ),
              publisher = list(
                "@type" = "Organization",
                name = "Open Trade Statistics"
              ),
              identifier = list(
                "@type" = "PropertyValue",
                propertyID = "DOI",
                value = "10.5281/zenodo.3738793"
              )
            ),
            auto_unbox = TRUE,
            pretty = TRUE
          )
        )
      )
    ),
    fluidRow(
      col_12(
        card(
          h2("Cite"),
          uiOutput(ns("citation_text")),
          br(),
          uiOutput(ns("citation_bibtex"))
        )
      )
    )
  )
}

#' @title Citation server-side function
#' @param id Internal parameter for Shiny.
mod_cite_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # configurable metadata (change these variables if you want different values)
    author_name_display <- "Mauricio Vargas Sepulveda" # used in human-readable text and meta tags
    author_name_bib <- "Vargas Sepulveda, Mauricio" # used in BibTeX author field
    site_url <- "https://shiny.tradestatistics.io"
    doi <- "10.5281/zenodo.3738793"

    output$citation_text <- renderUI({
      HTML(
        glue::glue(
          "{author_name_display}. \"OTS BETA DASHBOARD\". <i>Open Trade Statistics</i>.
        Accessed {format(Sys.Date(), '%B %d, %Y')} \u2014 {site_url}"
        )
      )
    })

    output$citation_bibtex <- renderUI({
      tags$pre(
        glue::glue(
          "@misc{{vargas_sepulveda_open_{ get_year() },
  title = {{Open Trade Statistics}},
  url = {{{site_url}}},
  author = {{{author_name_bib}}},
  doi = {{{doi}}},
  publisher = {{Open Trade Statistics}},
  year = {{{ get_year() }}},
  month = {{{ tolower(format(Sys.Date(), '%b')) }}},
  note = {{Accessed: { format(Sys.Date(), '%B %d, %Y') }}}
}}"
        )
      )
    })
  })
}
