#' @title Sector profile server-side function
#' @description A shiny Module.
#' @param id Internal parameter for Tabler.
#' @param con Shared SQL connection, created and closed by `app_server()`.
mod_sectors_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # User inputs ----
    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      return(y)
    })

    inp_s <- reactive({
      input$s
    }) # sector

    inp_t <- reactive({
      input$t
    }) # table
    
    inp_fmt <- reactive({
      fmt <- input$fmt
      if (is.null(fmt) || !nzchar(fmt)) "csv" else fmt
    }) # format

    tbl_agg <- reactive(paste0(inp_t(), "_imp_exp"))
    tbl_dtl <- reactive(inp_t())

    # Update year slider based on available data in the selected table ----
    observeEvent(input$t, {
      tbl <- inp_t()
      yr_range <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT MIN(year) AS min_yr, MAX(year) AS max_yr FROM %s",
          tbl
        )),
        error = function(e) NULL
      )
      if (!is.null(yr_range) && nrow(yr_range) == 1 && !is.na(yr_range$min_yr)) {
        min_yr <- as.integer(yr_range$min_yr)
        max_yr <- as.integer(yr_range$max_yr)
        cur_min <- min(input$y[1], input$y[2])
        cur_max <- max(input$y[1], input$y[2])
        updateSliderInput(session, "y",
          min   = min_yr,
          max   = max_yr,
          value = c(
            max(min_yr, min(cur_min, max_yr)),
            min(max_yr, max(cur_max, min_yr))
          )
        )
      }
    }, ignoreInit = TRUE)

    # Human-readable reporter/partner names for glue templates ----
    sname <- eventReactive(input$go, {
      out <- names(tradestatisticsdashboard::sectors[tradestatisticsdashboard::sectors == inp_s()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_s())
      }
      gsub(".* - ", "", out)
    })

    title <- eventReactive(input$go, {
      glue("{ sname() }: Multilateral trade { min(inp_y()) } - { max(inp_y()) }")
    })

    # Visualize ----

    ## Data ----

    df_agg <- reactive({
      showProgress(session, text = "Loading data...")

      yrs <- inp_y()
      scode <- inp_s()

      year_in <- paste(as.integer(yrs), collapse = ",")
      sector_clause <- sprintf(" AND broad_sector_id = '%s'", gsub("'", "''", scode))

      d <- setDT(dbGetQuery(con, sprintf(
        "SELECT year, SUM(trade) * 1000000 AS trade FROM %s WHERE year IN (%s)%s GROUP BY year",
        tbl_dtl(), year_in, sector_clause
      )))

      return(d)
    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    df_dtl <- reactive({
      yrs <- inp_y()
      scode <- inp_s()

      year_in <- paste(as.integer(yrs), collapse = ",")
      sector_clause <- sprintf(" AND broad_sector_id = '%s'", gsub("'", "''", scode))

      d <- setDT(dbGetQuery(con, sprintf(
        "SELECT year, importer_iso3_dynamic AS importer, exporter_iso3_dynamic AS exporter, SUM(trade) * 1000000 AS trade FROM %s WHERE year IN (%s)%s GROUP BY year, importer_iso3_dynamic, exporter_iso3_dynamic",
        tbl_dtl(), year_in, sector_clause
      )))

      return(d)
    }) |>
      bindCache(inp_y(), inp_s(), inp_t(), "yrpc") |>
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      yrs <- c(min(inp_y()), max(inp_y()))
      df_agg()[year %in% yrs, .(year, trade)]
    })

    exp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade]
    })

    exp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade]
    })

    imp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade]
    })

    imp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade]
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
      d_all <- df_agg()
      req(nrow(d_all) > 0)

      d <- data.table(
        year  = as.character(d_all$year),
        trade = round(d_all$trade / 1e9, 2),
        flow  = "Trade",
        color = "#26667f"
      )

      d3po(d) |>
        po_bar(
          daes(
            x = .data$year,
            y = .data$trade,
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
          y = format(.data$trade, big.mark = " ", scientific = FALSE)
        ) |>
        po_tooltip(JS(
          "function(value, row) {
            if (!row) return '';
            var grp = (row.flow != null) ? row.flow : (row.group != null ? row.group : '');
            var val = (value != null && !isNaN(value)) ? Number(value) : (row.trade != null && !isNaN(row.trade) ? Number(row.trade) : 0);
            var groupPrefix = grp ? (grp + ': ') : '';
            return groupPrefix + (val || 0) + ' B' ;
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

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dynamic_code, country FROM dgd_countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == min_year]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "dynamic_code")
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]
      d <- d[trade > 0]

      setorder(d, -trade)
      top4 <- d[seq_len(min(4L, .N)), country]
      d[!(country %in% top4), country := "Rest of the world"]
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]

      d[, color := "#85cca6"]

      rest <- d[country == "Rest of the world"][, n := 5L]
      others <- d[country != "Rest of the world"][order(-trade)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country := paste(n, country, sep = " - ")]
      d[, trade := round(trade / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country,
            x = .data$trade,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = exp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country}: {trade} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dynamic_code, country FROM dgd_countries"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == max_year]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "dynamic_code")
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]
      d <- d[trade > 0]

      setorder(d, -trade)
      top4 <- d[seq_len(min(4L, .N)), country]
      d[!(country %in% top4), country := "Rest of the world"]
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]

      d[, color := "#67c090"]

      rest <- d[country == "Rest of the world"][, n := 5L]
      others <- d[country != "Rest of the world"][order(-trade)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country := paste(n, country, sep = " - ")]
      d[, trade := round(trade / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country,
            x = .data$trade,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = exp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country}: {trade} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Exporters in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dc.dynamic_code, dc.country, col.region_colour, reg.region
         FROM dgd_countries dc
         JOIN dgd_colours col ON col.iso3_dynamic = dc.dynamic_code
         JOIN dgd_regions reg ON reg.region_id = col.region_id"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == min_year, .(trade_value = sum(trade, na.rm = TRUE)), by = .(exporter)]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "dynamic_code")
      setnames(d, c("country", "region"), c("country_name", "continent_name"))
      d[, continent_name := tools::toTitleCase(gsub("_", " ", continent_name))]

      d2 <- unique(d[, .(continent_name, country_color = region_colour)])
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

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dc.dynamic_code, dc.country, col.region_colour, reg.region
         FROM dgd_countries dc
         JOIN dgd_colours col ON col.iso3_dynamic = dc.dynamic_code
         JOIN dgd_regions reg ON reg.region_id = col.region_id"
      ))

      # exporter column identifies the exporting country
      d <- df_dtl()[year == max_year, .(trade_value = sum(trade, na.rm = TRUE)), by = .(exporter)]
      d <- merge(d, countries_data, by.x = "exporter", by.y = "dynamic_code")
      setnames(d, c("country", "region"), c("country_name", "continent_name"))
      d[, continent_name := tools::toTitleCase(gsub("_", " ", continent_name))]

      d2 <- unique(d[, .(continent_name, country_color = region_colour)])
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

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dynamic_code, country FROM dgd_countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == min_year]
      d <- merge(d, countries_data, by.x = "importer", by.y = "dynamic_code")
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]
      d <- d[trade > 0]

      setorder(d, -trade)
      top4 <- d[seq_len(min(4L, .N)), country]
      d[!(country %in% top4), country := "Rest of the world"]
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]

      d[, color := "#518498"]

      rest <- d[country == "Rest of the world"][, n := 5L]
      others <- d[country != "Rest of the world"][order(-trade)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country := paste(n, country, sep = " - ")]
      d[, trade := round(trade / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country,
            x = .data$trade,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = imp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country}: {trade} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dynamic_code, country FROM dgd_countries"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == max_year]
      d <- merge(d, countries_data, by.x = "importer", by.y = "dynamic_code")
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]
      d <- d[trade > 0]

      setorder(d, -trade)
      top4 <- d[seq_len(min(4L, .N)), country]
      d[!(country %in% top4), country := "Rest of the world"]
      d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(country)]

      d[, color := "#26667f"]

      rest <- d[country == "Rest of the world"][, n := 5L]
      others <- d[country != "Rest of the world"][order(-trade)][, n := .I]
      d <- rbindlist(list(rest, others), fill = TRUE)
      d[, country := paste(n, country, sep = " - ")]
      d[, trade := round(trade / 1e9, 2)]
      d[, n := NULL]

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country,
            x = .data$trade,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = imp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) |>
        po_format(x = format(.data$trade, big.mark = " ", scientific = FALSE, digits = 2)) |>
        po_tooltip("{country}: {trade} billion")

    }) |>
      bindCache(inp_y(), inp_s(), inp_t()) |>
      bindEvent(input$go)

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Importers in { min(inp_y()) }")
    })

    imp_tm_dtl_min_yr <- reactive({
      min_year <- min(inp_y())

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dc.dynamic_code, dc.country, col.region_colour, reg.region
         FROM dgd_countries dc
         JOIN dgd_colours col ON col.iso3_dynamic = dc.dynamic_code
         JOIN dgd_regions reg ON reg.region_id = col.region_id"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == min_year, .(trade_value = sum(trade, na.rm = TRUE)), by = .(importer)]
      d <- merge(d, countries_data, by.x = "importer", by.y = "dynamic_code")
      setnames(d, c("country", "region"), c("country_name", "continent_name"))
      d[, continent_name := tools::toTitleCase(gsub("_", " ", continent_name))]

      d2 <- unique(d[, .(continent_name, country_color = region_colour)])
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

      countries_data <- setDT(dbGetQuery(con,
        "SELECT dc.dynamic_code, dc.country, col.region_colour, reg.region
         FROM dgd_countries dc
         JOIN dgd_colours col ON col.iso3_dynamic = dc.dynamic_code
         JOIN dgd_regions reg ON reg.region_id = col.region_id"
      ))

      # importer column identifies the importing country
      d <- df_dtl()[year == max_year, .(trade_value = sum(trade, na.rm = TRUE)), by = .(importer)]
      d <- merge(d, countries_data, by.x = "importer", by.y = "dynamic_code")
      setnames(d, c("country", "region"), c("country_name", "continent_name"))
      d[, continent_name := tools::toTitleCase(gsub("_", " ", continent_name))]

      d2 <- unique(d[, .(continent_name, country_color = region_colour)])
      setorder(d2, continent_name)

      out <- od_treemap(d, d2, title = imp_tt_max_yr())

      hideProgress(session)

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
        "Sectors" = tradestatisticsdashboard::sectors
      ),
      selected = "1",
      server = TRUE
    )

    ## Sector profile ----

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
      "Download sector data"
    })

    dwn_txt <- eventReactive(input$go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
    })

    dwn_ctrl <- eventReactive(input$go, {
      tagList(
        selectInput(
          ns("fmt"),
          "Download data as:",
          choices = available_formats(),
          selected = NULL,
          selectize = TRUE
        ),
        div(
          class = "d-flex gap-2",
          downloadButton(ns("dwn_agg_pre"), label = "Aggregated data"),
          downloadButton(ns("dwn_dtl_pre"), label = "Detailed data")
        )
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
    output$dwn_ctrl <- renderUI({
      dwn_ctrl()
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
