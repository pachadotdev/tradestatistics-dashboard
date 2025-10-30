#' products UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_products_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useWaiter(),
    fluidRow(
      # Filter -----
      col_12(
        h2("Filter")
      ),
      col_4(
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
      col_4(
        selectInput(
          ns("s"),
          "Section/Commodity",
          choices = NULL,
          selected = NULL,
          selectize = TRUE,
          width = "100%",
          multiple = FALSE
        ) %>%
          helper(
            type = "inline",
            title = "Section/Commodity",
            content = c(
              "Subset the data for for any official section or commodity in the Harmonised System.",
              "",
              "<b>References</b>",
              "Hossain, K. and Nyirongo, V.<i><a href='https://unstats.un.org/wiki/display/comtrade/HS+2012+Classification+by+Section'>HS 2012 Classification by Section</a></i>. UN Stats Wiki, 2021."
            ),
            buttonLabel = "Got it!",
            easyClose = FALSE,
            fade = TRUE,
            size = "s"
          )
      ),
      col_4(
        selectInput(
          ns("d"),
          "Convert dollars to a fixed year",
          choices = c("No", available_yrs_deflator()),
          selected = "",
          selectize = TRUE,
          width = "100%"
        ) %>%
          helper(
            type = "inline",
            title = "Convert to dollars of the year",
            content = c("Uses present value and/or future value equations to adjust money value
                      by yearly changes in GDP deflator. The source for the GDP deflator data is The World Bank."),
            buttonLabel = "Got it!",
            easyClose = FALSE,
            fade = TRUE,
            size = "s"
          )
      ),
      col_12(
        align = "center",
        actionButton(ns("go"), "Give me the product profile",
          class = "btn-primary"
        )
      ),

      # Trade ----

      col_12(
        htmlOutput(ns("title"), container = tags$h2)
      ),

      ## Aggregated trade -----

      div(
        id = ns("aggregated_trade"),
        col_12(
          htmlOutput(ns("trd_stl"), container = tags$h3)
        ),
        col_3(
          htmlOutput(ns("trd_stl_trade"), container = tags$h4),
          htmlOutput(ns("trd_smr_trade"), container = tags$p)
        ),
        col_9(
          highchartOutput(ns("trd_exc_columns_agg"), height = "500px")
        )
      ),

      ## Detailed trade ----

      div(
        id = ns("detailed_trade"),
        col_12(
          htmlOutput(ns("exp_tt_yr"), container = tags$h3)
        ),
        col_6(
          htmlOutput(ns("exp_col_min_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_min_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_max_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_max_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_min_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_min_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_col_max_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("exp_col_max_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("exp_tt_min_yr"), container = tags$h4),
          highchartOutput(ns("exp_tm_dtl_min_yr"), height = "500px")
        ),
        col_6(
          htmlOutput(ns("exp_tt_max_yr"), container = tags$h4),
          highchartOutput(ns("exp_tm_dtl_max_yr"), height = "500px")
        ),
        col_12(
          htmlOutput(ns("imp_tt_yr"), container = tags$h3)
        ),
        col_6(
          htmlOutput(ns("imp_col_min_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_min_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_max_yr_usd_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_max_yr_usd"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_min_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_min_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_col_max_yr_pct_tt"), container = tags$h4),
          highchartOutput(ns("imp_col_max_yr_pct"), height = "400px")
        ),
        col_6(
          htmlOutput(ns("imp_tt_min_yr"), container = tags$h4),
          highchartOutput(ns("imp_tm_dtl_min_yr"), height = "500px")
        ),
        col_6(
          htmlOutput(ns("imp_tt_max_yr"), container = tags$h4),
          highchartOutput(ns("imp_tm_dtl_max_yr"), height = "500px")
        ),
        col_12(
          htmlOutput(ns("dwn_stl"), container = tags$h3),
          htmlOutput(ns("dwn_txt"), container = tags$p),
          uiOutput(ns("dwn_fmt")),
          uiOutput(ns("dwn_dtl"))
        )
      )
    )
  )
}

#' products Server Functions
#'
#' @noRd
mod_products_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Connect to SQL ----
    con <- sql_con()

    session$onSessionEnded(function() {
      if (!is.null(con) && dbIsValid(con)) {
        poolClose(con)
      }
    })

    # User inputs ----
    observe_helpers()

    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      return(y)
    })

    inp_s <- reactive({
      input$s
    }) # section/commodity

    inp_d <- reactive({
      input$d
    }) # adjust dollar

    inp_fmt <- reactive({
      input$fmt
    }) # format

    # Human-readable section name for glue templates
    section_name <- eventReactive(input$go, {
      s_code <- inp_s()
      if (is.null(s_code) || length(s_code) == 0) {
        return("Products")
      }

      if (nchar(s_code) == 2) {
        s <- names(tradestatisticsshiny::sections_display[
          tradestatisticsshiny::sections_display == s_code
        ])
        if (length(s) > 0 && !is.na(s) && nchar(s) > 0) {
          return(gsub(".* - ", "", s))
        }
      } else if (nchar(s_code) == 4) {
        s <- names(tradestatisticsshiny::commodities_short_display[
          tradestatisticsshiny::commodities_short_display == s_code
        ])
        if (length(s) > 0 && !is.na(s) && nchar(s) > 0) {
          return(gsub(".* - ", "", s))
        }
      }

      return(s_code)
    })

    # Titles ----

    title <- eventReactive(input$go, {
      glue("{ section_name() } multilateral trade profile between { min(inp_y()) } and { max(inp_y()) }")
    })

    # Visualize ----

    w <- Waiter$new("waiter-content", color = transparent(.5), html = spin_3())

    df_agg <- reactive({
      w$show()

      # Base data from yrc table
      d_base <- tbl(con, "yrc") %>%
        filter(!!sym("year") %in% !!inp_y())

      # Apply section/commodity filter if specified
      if (!is.null(inp_s()) && inp_s() != "") {
        if (nchar(inp_s()) == 4) {
          d_base <- d_base %>%
            filter(substr(!!sym("commodity_code"), 1, 4) == !!inp_s())
        } else if (nchar(inp_s()) == 2) {
          d_base <- d_base %>%
            filter(!!sym("section_code") == !!inp_s())
        }
      }

      # Use import data as measure of trade (more accurate from importer's perspective)
      d <- d_base %>%
        group_by(!!sym("year")) %>%
        summarise(
          trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      return(d)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    df_dtl <- reactive({
      # Base data from yrpc table
      d_base <- tbl(con, "yrpc") %>%
        filter(!!sym("year") %in% !!inp_y())

      # Apply section/commodity filter if specified
      if (!is.null(inp_s()) && inp_s() != "") {
        if (nchar(inp_s()) == 4) {
          d_base <- d_base %>%
            filter(substr(!!sym("commodity_code"), 1, 4) == !!inp_s())
        } else if (nchar(inp_s()) == 2) {
          d_base <- d_base %>%
            filter(!!sym("section_code") == !!inp_s())
        }
      }

      # Get commodities reference data
      commodities_ref <- tbl(con, "commodities") %>%
        distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color"), !!sym("commodity_code_short"), !!sym("commodity_name"))

      # For imports: use direct import data (more accurate)
      d_imp <- d_base %>%
        inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
        select(-!!sym("trade_value_usd_exp")) # Remove exports, we'll calculate them differently

      # For exports: use imports but swap reporter/partner perspective
      # If country A reports importing product X from country B,
      # then country B exported product X to country A
      d_exp <- d_base %>%
        inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
        mutate(
          !!sym("original_reporter") := !!sym("reporter_iso"),
          !!sym("reporter_iso") := !!sym("partner_iso"), # Swap perspective: partner becomes reporter
          !!sym("partner_iso") := !!sym("original_reporter"), # Original reporter becomes partner
          !!sym("trade_value_usd_exp") := !!sym("trade_value_usd_imp") # Their imports = exports from their partners
        ) %>%
        select(-!!sym("original_reporter"), -!!sym("trade_value_usd_imp"))

      # Combine the datasets
      d <- d_imp %>%
        left_join(
          d_exp,
          by = c(
            "year", "reporter_iso", "partner_iso", "commodity_code", "section_code",
            "section_name", "section_color", "commodity_code_short", "commodity_name"
          )
        ) %>%
        mutate(
          !!sym("trade_value_usd_exp") := ifelse(is.na(!!sym("trade_value_usd_exp")), 0, !!sym("trade_value_usd_exp")),
          !!sym("trade_value_usd_imp") := ifelse(is.na(!!sym("trade_value_usd_imp")), 0, !!sym("trade_value_usd_imp"))
        ) %>%
        collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      return(d)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d(), "yrpc") %>%
      bindEvent(input$go)

    ## Trade ----

    tr_tbl_agg <- eventReactive(input$go, {
      df_agg() %>%
        select(!!sym("year"), !!sym("trade_value_usd_imp"))
    })

    ### Tables ----

    imp_val_min_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("trade_value_usd_imp"))
    })

    imp_val_max_yr <- eventReactive(input$go, {
      tr_tbl_agg() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("trade_value_usd_imp"))
    })

    imp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_min_yr())
    })

    imp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_max_yr())
    })

    imports_growth <- eventReactive(input$go, {
      growth_rate(
        imp_val_max_yr(), imp_val_min_yr(), inp_y()
      )
    })

    imports_growth_2 <- eventReactive(input$go, {
      show_percentage(imports_growth())
    })

    imports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })

    imports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })

    ### Text/Visual elements ----

    trd_smr_txt <- eventReactive(input$go, {
      glue("The trade of { section_name() } { imports_growth_increase_decrease() } from
           { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) }
           (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() }).")
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      glue("{ section_name() } trade in { min(inp_y()) } and { max(inp_y()) }")
    })

    trd_exc_columns_agg <- reactive({
      d <- tr_tbl_agg()

      d <- tibble(
        year = as.character(d$year),
        trade = d$trade_value_usd_imp
      )

      hchart(d,
        "column",
        hcaes(x = "year", y = "trade"),
        color = "#26667f",
        tooltip = list(
          pointFormatter = custom_tooltip_short()
        )
      ) %>%
        hc_xAxis(title = list(text = "Year")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(formatter = JS("function() { return this.value / 1000000000 }"))
        ) %>%
        hc_title(text = trd_exc_columns_title())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      glue("Exports of { section_name() } in { min(inp_y()) } and { max(inp_y()) }, by country")
    })

    # Export column chart titles
    exp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) } (USD)")
    })

    exp_col_min_yr_pct_tt <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) } (%)")
    })

    exp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) } (USD)")
    })

    exp_col_max_yr_pct_tt <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) } (%)")
    })

    # Export column charts
    exp_col_min_yr_usd <- reactive({
      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(year == min(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
        color = "#85cca6",
        tooltip = list(pointFormatter = custom_tooltip_short())
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(formatter = JS("function() { return this.value / 1000000000 }"))
        ) %>%
        hc_title(text = exp_col_min_yr_usd_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_min_yr_pct <- reactive({
      d <- df_dtl() %>%
        filter(year == min(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
        color = "#85cca6",
        tooltip = list(pointFormatter = JS("function() { return this.point.name + ': <b>' + this.y + '%</b>'; }"))
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(title = list(text = "Percentage (%)")) %>%
        hc_title(text = exp_col_min_yr_pct_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      d <- df_dtl() %>%
        filter(year == max(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
        color = "#67c090",
        tooltip = list(pointFormatter = custom_tooltip_short())
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(formatter = JS("function() { return this.value / 1000000000 }"))
        ) %>%
        hc_title(text = exp_col_max_yr_usd_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_pct <- reactive({
      d <- df_dtl() %>%
        filter(year == max(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
        color = "#67c090",
        tooltip = list(pointFormatter = JS("function() { return this.point.name + ': <b>' + this.y + '%</b>'; }"))
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(title = list(text = "Percentage (%)")) %>%
        hc_title(text = exp_col_max_yr_pct_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Exports in { min(inp_y()) } by country")
    })

    exp_tm_dtl_min_yr <- reactive({
      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        group_by(!!sym("partner_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        )

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Exports in { max(inp_y()) } by country")
    })

    exp_tm_dtl_max_yr <- reactive({
      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        group_by(!!sym("partner_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
            collect(),
          by = c("partner_iso" = "country_iso")
        )

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      glue("Imports of { section_name() } in { min(inp_y()) } and { max(inp_y()) }, by country")
    })

    # Import column chart titles
    imp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) } (USD)")
    })

    imp_col_min_yr_pct_tt <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) } (%)")
    })

    imp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) } (USD)")
    })

    imp_col_max_yr_pct_tt <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) } (%)")
    })

    # Import column charts
    imp_col_min_yr_usd <- reactive({
      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(year == min(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
        color = "#518498",
        tooltip = list(pointFormatter = custom_tooltip_short())
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(formatter = JS("function() { return this.value / 1000000000 }"))
        ) %>%
        hc_title(text = imp_col_min_yr_usd_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_min_yr_pct <- reactive({
      d <- df_dtl() %>%
        filter(year == min(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
        color = "#518498",
        tooltip = list(pointFormatter = JS("function() { return this.point.name + ': <b>' + this.y + '%</b>'; }"))
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(title = list(text = "Percentage (%)")) %>%
        hc_title(text = imp_col_min_yr_pct_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      d <- df_dtl() %>%
        filter(year == max(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "trade_value_usd_imp"),
        color = "#26667f",
        tooltip = list(pointFormatter = custom_tooltip_short())
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(
          title = list(text = "USD billion"),
          labels = list(formatter = JS("function() { return this.value / 1000000000 }"))
        ) %>%
        hc_title(text = imp_col_max_yr_usd_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_pct <- reactive({
      d <- df_dtl() %>%
        filter(year == max(inp_y())) %>%
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        ) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        mutate(percentage = round(100 * !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp")), 1)) %>%
        mutate(country_name = factor(!!sym("country_name"),
          levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
        ))

      hchart(d, "column", hcaes(x = "country_name", y = "percentage"),
        color = "#26667f",
        tooltip = list(pointFormatter = JS("function() { return this.point.name + ': <b>' + this.y + '%</b>'; }"))
      ) %>%
        hc_xAxis(title = list(text = "Country")) %>%
        hc_yAxis(title = list(text = "Percentage (%)")) %>%
        hc_title(text = imp_col_max_yr_pct_tt())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Imports in { min(inp_y()) } by country")
    })

    imp_tm_dtl_min_yr <- reactive({
      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        group_by(!!sym("reporter_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        )

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_to_highcharts(d, d2)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Imports in { max(inp_y()) } by country")
    })

    imp_tm_dtl_max_yr <- reactive({
      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        group_by(!!sym("reporter_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(
          tbl(con, "countries") %>%
            select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
            collect(),
          by = c("reporter_iso" = "country_iso")
        )

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      out <- od_to_highcharts(d, d2)

      w$hide()
      return(out)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    # Additional processing functions can be added here

    # Outputs ----

    ## Titles / texts ----

    output$title <- renderText({
      title()
    })

    ## Dynamic / server side selectors ----

    updateSelectizeInput(session, "s",
      choices = list(
        "HS Sections" = tradestatisticsshiny::sections_display,
        "HS Commodities" = tradestatisticsshiny::commodities_short_display
      ),
      selected = "01",
      server = TRUE
    )

    ## Product profile ----

    ### Trade ----

    output$trd_stl <- eventReactive(input$go, {
      "Total multilateral Trade"
    })

    output$trd_stl_trade <- eventReactive(input$go, {
      "Trade Summary"
    })

    output$trd_smr_trade <- renderText(trd_smr_txt())

    output$trd_exc_columns_agg <- renderHighchart({
      trd_exc_columns_agg()
    })

    # ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())

    output$exp_col_min_yr_usd_tt <- renderText(exp_col_min_yr_usd_tt())
    output$exp_col_min_yr_usd <- renderHighchart({
      exp_col_min_yr_usd()
    })
    output$exp_col_min_yr_pct_tt <- renderText(exp_col_min_yr_pct_tt())
    output$exp_col_min_yr_pct <- renderHighchart({
      exp_col_min_yr_pct()
    })
    output$exp_col_max_yr_usd_tt <- renderText(exp_col_max_yr_usd_tt())
    output$exp_col_max_yr_usd <- renderHighchart({
      exp_col_max_yr_usd()
    })
    output$exp_col_max_yr_pct_tt <- renderText(exp_col_max_yr_pct_tt())
    output$exp_col_max_yr_pct <- renderHighchart({
      exp_col_max_yr_pct()
    })

    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- renderHighchart({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- renderHighchart({
      exp_tm_dtl_max_yr()
    })

    # ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    output$imp_col_min_yr_usd_tt <- renderText(imp_col_min_yr_usd_tt())
    output$imp_col_min_yr_usd <- renderHighchart({
      imp_col_min_yr_usd()
    })
    output$imp_col_min_yr_pct_tt <- renderText(imp_col_min_yr_pct_tt())
    output$imp_col_min_yr_pct <- renderHighchart({
      imp_col_min_yr_pct()
    })
    output$imp_col_max_yr_usd_tt <- renderText(imp_col_max_yr_usd_tt())
    output$imp_col_max_yr_usd <- renderHighchart({
      imp_col_max_yr_usd()
    })
    output$imp_col_max_yr_pct_tt <- renderText(imp_col_max_yr_pct_tt())
    output$imp_col_max_yr_pct <- renderHighchart({
      imp_col_max_yr_pct()
    })

    output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
    output$imp_tm_dtl_min_yr <- renderHighchart({
      imp_tm_dtl_min_yr()
    })
    output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
    output$imp_tm_dtl_max_yr <- renderHighchart({
      imp_tm_dtl_max_yr()
    })

    ## Download ----

    dwn_stl <- eventReactive(input$go, {
      "Download product data"
    })

    dwn_txt <- eventReactive(input$go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
    })

    dwn_fmt <- eventReactive(input$go, {
      selectInput(
        ns("fmt"),
        "Download data as:",
        choices = available_formats(),
        selected = NULL,
        selectize = TRUE
      )
    })

    output$dwn_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_s() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_dtl(), filename)
      }
    )

    output$dwn_stl <- renderText({
      dwn_stl()
    })
    output$dwn_txt <- renderText({
      dwn_txt()
    })
    output$dwn_fmt <- renderUI({
      dwn_fmt()
    })

    output$dwn_dtl <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_dtl_pre"), label = "Detailed data")
    })

    ## Additional outputs can be added here

    # Hide boxes until viz is ready ----

    ## observe the button being pressed
    observeEvent(input$go, {
      if (input$go > 0) {
        show(id = ns("aggregated_trade"))
        show(id = ns("detailed_trade"))
      } else {
        hide(id = ns("aggregated_trade"))
        hide(id = ns("detailed_trade"))
      }
    })

    # Additional functionality can be added here

    # Module-specific functionality ends here
  })
}