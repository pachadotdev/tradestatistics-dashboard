#' @title Country profile UI-side function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
mod_countries_ui <- function(id) {
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
                value = c(2018, 2023),
                sep = "",
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            ),
            col_3(
              selectInput(
                ns("r"),
                "Reporter",
                choices = sort(tradestatisticsshiny::reporters_display[
                  tradestatisticsshiny::reporters_display != "ALL"
                ]),
                selected = "GBR",
                selectize = TRUE,
                width = "100%"
              )
            ),
            col_3(
              selectInput(
                ns("p"),
                "Partner",
                choices = c(
                  "All countries" = "ALL",
                  sort(tradestatisticsshiny::reporters_display[
                    tradestatisticsshiny::reporters_display != "ALL"
                  ])
                ),
                selected = "ALL",
                selectize = TRUE,
                width = "100%"
              )
            ),
            col_3(
              selectInput(
                ns("d"),
                "Convert dollars to a fixed year",
                choices = c("No", available_yrs_deflator()),
                selected = "",
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
                "Give me the country profile",
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
              col_6(
                htmlOutput(ns("trd_stl_exp"), container = tags$h4),
                htmlOutput(ns("trd_smr_exp"), container = tags$p)
              ),
              col_6(
                htmlOutput(ns("trd_stl_imp"), container = tags$h4),
                htmlOutput(ns("trd_smr_imp"), container = tags$p)
              )
            ),
            p("The chart below shows the evolution of exports and imports over the selected period."),
            fluidRow(
              col_12(
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
            p("These charts show export destinations (bar charts) and export composition by product category (treemaps) for the first and last year."),
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
            p("These charts show import sources (bar charts) and import composition by product category (treemaps) for the first and last year."),
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
            p("Download the data used to generate these visualizations. Aggregated data includes yearly totals; detailed data includes trade by product category."),
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
