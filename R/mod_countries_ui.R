#' @title Country profile UI-side function
#' @description A tabler Module.
#' @param id Internal parameter for Tabler.
mod_countries_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      # Filter -----
      row(
        col9(card(h2("Subset the data"))),
        col3(actionButton(ns("go"), "Give me the country profile", class = "btn-teal w-100"))
      ),
      br(),
      col12(card(sliderInput(
        ns("y"),
        "Years",
        min = available_yrs_min(),
        max = available_yrs_max(),
        value = c(2018, 2022),
        sep = "",
        step = 1,
        ticks = FALSE,
        width = "100%"
      ))),
      br(),
      row(
        col3(card(selectInput(
          ns("i"),
          "Importer",
          choices = tradestatisticsdashboard::countries,
          selected = "GBR",
          selectize = TRUE,
          width = "100%"
        ))),
        col3(card(selectInput(
          ns("e"),
          "Exporter",
          choices = c("All countries" = "ALL", tradestatisticsdashboard::countries),
          selected = "ALL",
          selectize = TRUE,
          width = "100%"
        ))),
        col6(card(
          selectInput(
            ns("t"),
            "Dataset",
            choices = c(
              `International Trade and Production Database for Estimation (ITPD-E)` = "itpde",
              `International Trade and Production Database for Simulation (ITPD-S)` = "itpds"
            ),
            selected = "itpde",
            selectize = TRUE,
            width = "100%"
          )
        ))
      ),

      # Trade ----

      hidden(
        div(
          id = ns("title_section"),
          br(),
          card(htmlOutput(ns("title"), container = tags$h1))
        )
      ),

      ## Aggregated trade -----

      hidden(
        div(
          id = ns("aggregated_trade"),
          br(),
          col12(
            card(
              htmlOutput(ns("trd_stl"), container = tags$h2),
              htmlOutput(ns("trd_stl_exp"), container = tags$h4),
              htmlOutput(ns("trd_smr_exp"), container = tags$p),
              htmlOutput(ns("trd_stl_imp"), container = tags$h4),
              htmlOutput(ns("trd_smr_imp"), container = tags$p)
            )
          ),
          br(),
          col12(card(d3po_output(ns("trd_exc_columns_agg"), height = "400px")))
        )
      ),

      ## Detailed exports ----

      hidden(
        div(
          id = ns("detailed_trade_exp"),
          br(),
          col12(card(
            htmlOutput(ns("exp_tt_yr"), container = tags$h2),
            p("These charts show exports evolution over the selected years (line chart) and exports composition by sector and industry (treemaps) for the first and last year.")
          )),
          br(),
          col12(card(d3po_output(ns("trd_line_exp"), height = "400px"))),
          br(),
          row(
            col6(card(d3po_output(ns("exp_tm_dtl_min_yr"), height = "500px"))),
            col6(card(d3po_output(ns("exp_tm_dtl_max_yr"), height = "500px")))
          )
        )
      ),

      ## Detailed imports ----

      hidden(
        div(
          id = ns("detailed_trade_imp"),
          br(),
          col12(
            card(
              htmlOutput(ns("imp_tt_yr"), container = tags$h2),
              p("These charts show imports evolution over the selected years (line chart) and imports composition by sector and industry (treemaps) for the first and last year.")
            )
          ),
          br(),
          col12(card(d3po_output(ns("trd_line_imp"), height = "400px"))),
          br(),
          row(
            col6(card(d3po_output(ns("imp_tm_dtl_min_yr"), height = "500px"))),
            col6(card(d3po_output(ns("imp_tm_dtl_max_yr"), height = "500px")))
          )
        )
      ),

      ## Download ----
      hidden(
        div(
          id = ns("download_data"),
          br(),
          card(
            htmlOutput(ns("dwn_stl"), container = tags$h2),
            p("Download the data used to generate these visualizations. Aggregated data includes yearly totals; detailed data includes trade by industry."),
            htmlOutput(ns("dwn_txt"), container = tags$p),
            uiOutput(ns("dwn_ctrl"))
          )
        )
      )
    )
  )
}
