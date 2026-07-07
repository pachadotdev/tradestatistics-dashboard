#' @title Sector profile UI Function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
mod_sectors_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      # Filter -----
      card(
        col_12(
          h2("Filter"),
          fluidRow(
            col_3(
              sliderInput(
                ns("y"),
                "Years",
                min = available_yrs_min(),
                max = available_yrs_max(),
                value = c(2018, 2022),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            col_6(
              selectInput(
                ns("s"),
                "Sectors",
                choices = list("Sectors" = tradestatisticsshiny::sectors),
                selected = "1",
                selectize = TRUE,
                width = "100%"
              )
            ),
            col_3(
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
            )
          ),
          fluidRow(
            col_12(
              align = "center",
              br(),
              actionButton(
                ns("go"),
                "Give me the sector profile",
                class = "btn btn-outline btn-dark"
              )
            )
          )
        )
      ),

      # Trade ----

      hidden(
        div(
          id = ns("title_section"),
          br(),
          br(),
          card(
            col_12(
              htmlOutput(ns("title"), container = tags$h1)
            )
          )
        )
      ),

      ## Aggregated trade -----

      hidden(
        div(
          id = ns("aggregated_trade"),
          br(),
          br(),
          card(
            htmlOutput(ns("trd_stl"), container = tags$h2),
            fluidRow(
              col_4(
                htmlOutput(ns("trd_stl_trade"), container = tags$h4),
                htmlOutput(ns("trd_smr_trade"), container = tags$p),
                p("The chart shows global trade trends for this sector. Trade values represent the sum of all countries' imports.")
              ),
              col_8(
                d3po_output(ns("trd_exc_columns_agg"), height = "500px")
              )
            )
          )
        )
      ),

      ## Detailed exports ----

      hidden(
        div(
          id = ns("detailed_trade_exp"),
          br(),
          br(),
          card(
            htmlOutput(ns("exp_tt_yr"), container = tags$h2),
            p("Bar charts show the top exporters for this sector. Treemaps displays all exporters."),
            fluidRow(
              col_6(
                d3po_output(ns("exp_col_min_yr_usd"), height = "500px")
              ),
              col_6(
                d3po_output(ns("exp_col_max_yr_usd"), height = "500px")
              ),
              col_6(
                d3po_output(ns("exp_tm_dtl_min_yr"), height = "500px")
              ),
              col_6(
                d3po_output(ns("exp_tm_dtl_max_yr"), height = "500px")
              )
            )
          )
        )
      ),

      ## Detailed imports ----

      hidden(
        div(
          id = ns("detailed_trade_imp"),
          br(),
          br(),
          card(
            htmlOutput(ns("imp_tt_yr"), container = tags$h2),
            p("Bar charts show the top importers for this sector. Treemaps display all importers."),
            fluidRow(
              col_6(
                d3po_output(ns("imp_col_min_yr_usd"), height = "500px")
              ),
              col_6(
                d3po_output(ns("imp_col_max_yr_usd"), height = "500px")
              ),
              col_6(
                d3po_output(ns("imp_tm_dtl_min_yr"), height = "500px")
              ),
              col_6(
                d3po_output(ns("imp_tm_dtl_max_yr"), height = "500px")
              )
            )
          )
        )
      ),

      ## Download ----
      hidden(
        div(
          id = ns("download_data"),
          br(),
          br(),
          card(
            htmlOutput(ns("dwn_stl"), container = tags$h2),
            p("Download the data behind these charts. Aggregated data shows yearly global totals; detailed data includes country-level breakdowns."),
            htmlOutput(ns("dwn_txt"), container = tags$p),
            uiOutput(ns("dwn_fmt")),
            br(),
            uiOutput(ns("dwn_agg")),
            br(),
            uiOutput(ns("dwn_dtl"))
          )
        )
      )
    )
  )
}
