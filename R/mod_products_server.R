#' @title Product profile server-side function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
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
    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      return(y)
    })

    inp_s <- reactive({
      input$s
    }) # section/commodity
    
    inp_fmt <- reactive({
      input$fmt
    }) # format

    tbl_agg <- "yrc"
    tbl_dtl <- "yrpc"

    # Human-readable reporter/partner names for glue templates. Fallback to
    # the code when no display name is available.
    sname <- eventReactive(input$go, {
      scode <- inp_s()
      if (is.null(scode) || length(scode) == 0) {
        return("Products")
      }

      if (nchar(scode) == 2) {
        s <- names(tradestatisticsshiny::sections_display[
          tradestatisticsshiny::sections_display == scode
        ])
        if (length(s) > 0 && !is.na(s) && nchar(s) > 0) {
          return(gsub(".* - ", "", s))
        }
      } else if (nchar(scode) == 4) {
        s <- names(tradestatisticsshiny::commodities_short_display[
          tradestatisticsshiny::commodities_short_display == scode
        ])
        if (length(s) > 0 && !is.na(s) && nchar(s) > 0) {
          return(gsub(".* - ", "", s))
        }
      }

      return(scode)
    })

    title <- eventReactive(input$go, {
      glue("{ sname() }: Multilateral trade { min(inp_y()) } - { max(inp_y()) }")
    })

    # Visualize ----

    ## Data ----

    df_agg <- reactive({
      session$sendCustomMessage("showProgress", list(text = "Loading data..."))

      yrs <- inp_y()
      scode <- inp_s()

      year_in <- paste(as.integer(yrs), collapse = ",")
      sector_clause <- if (nchar(scode) == 4) {
        sprintf(" AND substr(industry_id, 1, 4) = '%s'", gsub("'", "''", scode))
      } else if (nchar(scode) == 2) {
        sprintf(" AND broad_sector_id = '%s'", gsub("'", "''", scode))
      } else {
        ""
      }

      d <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, SUM(trade) AS trade FROM %s WHERE year IN (%s)%s GROUP BY year",
        tbl_agg, year_in, sector_clause
      )))

      d[, `:=`(trade_value_usd_imp = trade, trade_value_usd_exp = trade)]
      d[, trade := NULL]

      return(d)
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    df_dtl <- reactive({
      yrs <- inp_y()
      scode <- inp_s()

      year_in <- paste(as.integer(yrs), collapse = ",")
      sector_clause <- if (nchar(scode) == 4) {
        sprintf(" AND substr(industry_id, 1, 4) = '%s'", gsub("'", "''", scode))
      } else if (nchar(scode) == 2) {
        sprintf(" AND broad_sector_id = '%s'", gsub("'", "''", scode))
      } else {
        ""
      }

      d_raw <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year IN (%s)%s",
        tbl_dtl, year_in, sector_clause
      )))

      # Get commodities reference data
      commodities_ref <- setDT(pool::dbGetQuery(con,
        "SELECT DISTINCT industry_id, broad_sector_id, commodity_code_short FROM commodities"
      ))

      d_raw <- merge(d_raw, commodities_ref, by = c("industry_id", "broad_sector_id"))

      # d_imp: imports perspective (importer is the reporter)
      if (nchar(scode) == 4) {
        d_imp <- d_raw[,
          .(trade_value_usd_imp = sum(trade, na.rm = TRUE)),
          by = .(year, importer, exporter, commodity_code = commodity_code_short)
        ]
      } else if (nchar(scode) == 2) {
        d_imp <- d_raw[,
          .(trade_value_usd_imp = sum(trade, na.rm = TRUE)),
          by = .(year, importer, exporter, commodity_code = broad_sector_id)
        ]
      }

      # d_exp: exports perspective (swap importer/exporter)
      if (nchar(scode) == 4) {
        d_exp <- d_raw[,
          .(trade_value_usd_exp = sum(trade, na.rm = TRUE)),
          by = .(year, importer = exporter, exporter = importer, commodity_code = commodity_code_short)
        ]
      } else if (nchar(scode) == 2) {
        d_exp <- d_raw[,
          .(trade_value_usd_exp = sum(trade, na.rm = TRUE)),
          by = .(year, importer = exporter, exporter = importer, commodity_code = broad_sector_id)
        ]
      }

      d <- merge(d_imp, d_exp, by = c("year", "importer", "exporter", "commodity_code"), all.x = TRUE)
      d[is.na(trade_value_usd_exp), trade_value_usd_exp := 0]
      d[is.na(trade_value_usd_imp), trade_value_usd_imp := 0]

      # Add section color
      if (nchar(scode) == 4L) {
        colors_ref <- setDT(pool::dbGetQuery(con,
          "SELECT DISTINCT commodity_code_short AS commodity_code, section_color FROM commodities"
        ))
        d <- merge(d, colors_ref, by = "commodity_code")
      } else if (nchar(scode) == 2) {
        colors_ref <- setDT(pool::dbGetQuery(con,
          "SELECT DISTINCT broad_sector_id AS commodity_code, section_color FROM commodities"
        ))
        d <- merge(d, colors_ref, by = "commodity_code")
      }

      return(d)
    }) |>
      bindCache(inp_y(), inp_s(), inp_t(), "yrpc") |>
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      yrs <- c(min(inp_y()), max(inp_y()))
      df_agg()[year %in% yrs, .(year, trade_value_usd_exp, trade_value_usd_imp)]
    })

    exp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_value_usd_exp]
    })

    exp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_value_usd_exp]
    })

    imp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_value_usd_imp]
    })

    imp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_value_usd_imp]
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
      glue("The trade of { sname() } { imports_growth_increase_decrease() } from
           { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) }
           (annualized { imports_growth_increase_decrease_2() } of { imports_growth_2() }).")
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      glue("{ sname() } trade in { min(inp_y()) } and { max(inp_y()) }")
    })

    trd_exc_columns_agg <- reactive({
      d <- trade_values()

      # Follow countries module structure: produce a flow column and color mapping
      d <- tibble(
        year = d$year,
        trade = d$trade_value_usd_imp,
        flow = "Imports"
      ) |>
        mutate(
          year = as.character(!!sym("year")),
          color = ifelse(!!sym("flow") == "Exports", "#67c090", "#26667f")
        )

      # convert to billions for display
      d <- d |>
        arrange(!!sym("year")) |>
        mutate(trade_billion = .data$trade / 1e9)

      d3po(d) |>
        po_bar(
          daes(
            x = .data$year,
            y = .data$trade_billion,
            group = .data$flow,
            color = .data$color,
            stack = FALSE
          )
        ) |>
        po_labels(
          x = "Year",
          y = "Trade Value (USD billion)",
          title = trd_exc_columns_title()
        ) |>
        po_format(
          y = format(.data$trade_billion, big.mark = " ", scientific = FALSE, digits = 2)
        ) |>
        po_tooltip(JS(
          "function(value, row) {
            if (!row) return '';
            var grp = (row.flow != null) ? row.flow : (row.group != null ? row.group : '');
            var val = (value != null && !isNaN(value)) ? Number(value) : (row.trade_billion != null && !isNaN(row.trade_billion) ? Number(row.trade_billion) : 0);
            var groupPrefix = grp ? (grp + ': ') : '';
            return groupPrefix + (val || 0) + ' billion';
          }"
        ))

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      glue("Exports of { sname() } in { min(inp_y()) } and { max(inp_y()) }, by country")
    })

    # Export column chart titles
    exp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Top Exporters in { min(inp_y()) }")
    })

    exp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Top Exporters in { max(inp_y()) }")
    })

    # Export column charts
    exp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name FROM countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == min_year]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      setorder(d, -trade_value_usd_imp)
      top4 <- d[seq_len(min(4L, .N)), country_name]
      d[!(country_name %in% top4), country_name := "Rest of the world"]
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

      d[, color := "#85cca6"]

      rest <- d[country_name == "Rest of the world"][, n := 5L]
      others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country_name := paste(n, country_name, sep = " - ")]
      d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = exp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country_name}: {trade_value_usd_imp} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name FROM countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == max_year]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      setorder(d, -trade_value_usd_imp)
      top4 <- d[seq_len(min(4L, .N)), country_name]
      d[!(country_name %in% top4), country_name := "Rest of the world"]
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

      d[, color := "#67c090"]

      rest <- d[country_name == "Rest of the world"][, n := 5L]
      others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country_name := paste(n, country_name, sep = " - ")]
      d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = exp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country_name}: {trade_value_usd_imp} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Exporters in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name, continent_name, continent_color FROM countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == min_year, .(trade_value = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(exporter)]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "country_iso")

      d2 <- unique(d[, .(continent_name, country_color = continent_color)])
      setorder(d2, continent_name)

      od_treemap(d, d2, title = exp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Top Exporters in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name, continent_name, continent_color FROM countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == max_year, .(trade_value = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(exporter)]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "country_iso")

      d2 <- unique(d[, .(continent_name, country_color = continent_color)])
      setorder(d2, continent_name)

      od_treemap(d, d2, title = exp_tt_max_yr())
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      glue("Imports of { sname() } in { min(inp_y()) } and { max(inp_y()) }, by country")
    })

    # Import column chart titles
    imp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Top Importers in { min(inp_y()) }")
    })

    imp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Top Importers in { max(inp_y()) }")
    })

    # Import column charts
    imp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name FROM countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == min_year]
      d <- merge(d, countries_data, by.x = "importer", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      setorder(d, -trade_value_usd_imp)
      top4 <- d[seq_len(min(4L, .N)), country_name]
      d[!(country_name %in% top4), country_name := "Rest of the world"]
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

      d[, color := "#518498"]

      rest <- d[country_name == "Rest of the world"][, n := 5L]
      others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country_name := paste(n, country_name, sep = " - ")]
      d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = imp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country_name}: {trade_value_usd_imp} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name FROM countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == max_year]
      d <- merge(d, countries_data, by.x = "importer", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      setorder(d, -trade_value_usd_imp)
      top4 <- d[seq_len(min(4L, .N)), country_name]
      d[!(country_name %in% top4), country_name := "Rest of the world"]
      d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

      d[, color := "#26667f"]

      rest <- d[country_name == "Rest of the world"][, n := 5L]
      others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country_name := paste(n, country_name, sep = " - ")]
      d[, trade_value_billion := round(trade_value_usd_imp / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_billion,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = imp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade_value_billion, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country_name}: {trade_value_billion} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Importers in { min(inp_y()) }")
    })

    imp_tm_dtl_min_yr <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name, continent_name, continent_color FROM countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == min_year, .(trade_value = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(importer)]
      d <- merge(d, countries_data, by.x = "importer", by.y = "country_iso")

      d2 <- unique(d[, .(continent_name, country_color = continent_color)])
      setorder(d2, continent_name)

      od_treemap(d, d2, title = imp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Top Importers in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(pool::dbGetQuery(con,
        "SELECT country_iso, country_name, continent_name, continent_color FROM countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == max_year, .(trade_value = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(importer)]
      d <- merge(d, countries_data, by.x = "importer", by.y = "country_iso")

      d2 <- unique(d[, .(continent_name, country_color = continent_color)])
      setorder(d2, continent_name)

      out <- od_treemap(d, d2, title = imp_tt_max_yr())

      session$sendCustomMessage("hideProgress", list())

      return(out)
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
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

    output$trd_exc_columns_agg <- render_d3po({
      trd_exc_columns_agg()
    })

    # ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())

    output$exp_col_min_yr_usd_tt <- renderText(exp_col_min_yr_usd_tt())
    output$exp_col_min_yr_usd <- render_d3po({
      exp_col_min_yr_usd()
    })

    output$exp_col_max_yr_usd_tt <- renderText(exp_col_max_yr_usd_tt())
    output$exp_col_max_yr_usd <- render_d3po({
      exp_col_max_yr_usd()
    })

    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- render_d3po({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- render_d3po({
      exp_tm_dtl_max_yr()
    })

    # ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    output$imp_col_min_yr_usd_tt <- renderText(imp_col_min_yr_usd_tt())
    output$imp_col_min_yr_usd <- render_d3po({
      imp_col_min_yr_usd()
    })

    output$imp_col_max_yr_usd_tt <- renderText(imp_col_max_yr_usd_tt())
    output$imp_col_max_yr_usd <- render_d3po({
      imp_col_max_yr_usd()
    })

    output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
    output$imp_tm_dtl_min_yr <- render_d3po({
      imp_tm_dtl_min_yr()
    })
    output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
    output$imp_tm_dtl_max_yr <- render_d3po({
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

    output$dwn_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_s() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_agg(), filename)
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

    output$dwn_agg <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_agg_pre"), label = "Aggregated data")
    })

    # Hide boxes until viz is ready ----

    ## observe the button being pressed
    observeEvent(input$go, {
      if (input$go > 0) {
        show(id = "title_section")
        show(id = "aggregated_trade")
        show(id = "detailed_trade_exp")
        show(id = "detailed_trade_imp")
        show(id = "download_data")
      }
    })
  })
}
