#' @title Sector profile UI Function
#' @description A tabler Module.
#' @param id Internal parameter for Tabler.
mod_sectors_ui <- function(id) {
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
        col6(card(selectInput(
          ns("s"),
          "Sectors",
          # choices = list("Sectors" = tradestatisticsdashboard::sectors),
          choices = tradestatisticsdashboard::sectors,
          selected = "1",
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
              br(),
              htmlOutput(ns("trd_stl_trade"), container = tags$h4),
              htmlOutput(ns("trd_smr_trade"), container = tags$p)
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
          # row(
          #   col4(
          #     card(htmlOutput(ns("exp_tt_yr"), container = tags$h2)),
          #   ),
          #   col8(
          #     card(p("Bar charts show the top exporters for this sector. Treemaps displays all exporters."))
          #   )
          # ),
          col12(
            card(
              htmlOutput(ns("exp_tt_yr"), container = tags$h2),
              p("Bar charts show the top exporters for this sector. Treemaps displays all exporters.")
            )
          ),
          br(),
          row(
            col6(card(d3po_output(ns("exp_col_min_yr_usd"), height = "500px"))),
            col6(card(d3po_output(ns("exp_col_max_yr_usd"), height = "500px")))
          ),
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
              p("Bar charts show the top importers for this sector. Treemaps display all importers.")
            )
          ),
          br(),
          row(
            col6(card(d3po_output(ns("imp_col_min_yr_usd"), height = "500px"))),
            col6(card(d3po_output(ns("imp_col_max_yr_usd"), height = "500px")))
          ),
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
            p("Download the data behind these charts. Aggregated data shows yearly global totals; detailed data includes country-level breakdowns."),
            htmlOutput(ns("dwn_txt"), container = tags$p),
            uiOutput(ns("dwn_ctrl"))
          )
        )
      )
    )
  )
}
