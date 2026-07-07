#' @title Country profile server-side function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
mod_countries_server <- function(id) {
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

    inp_i <- reactive({
      input$i
    }) # importer
    
    inp_e <- reactive({
      input$e
    }) # exporter

    inp_t <- reactive({
      input$t
    }) # table
    
    inp_fmt <- reactive({
      input$fmt
    }) # format

    # tbl_agg <- "itpde_imp_exp"
    # tbl_dtl <- "itpde"

    tbl_agg <- reactive(paste0(inp_t(), "_imp_exp"))
    tbl_dtl <- reactive(inp_t())

    # Update year slider based on available data in the selected table ----
    observeEvent(input$t, {
      tbl <- inp_t()
      yr_range <- tryCatch(
        pool::dbGetQuery(con, sprintf(
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

    # Human-readable importer/exporter names for glue templates. Fallback to
    # the code when no display name is available.
    rname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::countries[tradestatisticsshiny::countries == inp_i()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_i())
      }
      out
    })

    pname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::countries[tradestatisticsshiny::countries == inp_e()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_e())
      }
      out
    })

    title <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Multilateral trade { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    # Visualize ----

    ## Data ----

    df_agg <- reactive({
      session$sendCustomMessage("showProgress", list(text = "Loading data..."))

      years <- inp_y()
      importer <- inp_i()
      exporter <- inp_e()
      min_yr <- as.integer(min(years))
      max_yr <- as.integer(max(years))
      e <- gsub("'", "''", importer)

      if (exporter == "ALL") {
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
      }

      d <- merge(d_exp, d_imp, by = "year", all = TRUE)
      d[is.na(trade_exp), trade_exp := 0]
      d[is.na(trade_imp), trade_imp := 0]
      return(d)
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    df_dtl <- reactive({
      years <- inp_y()
      importer <- inp_i()
      exporter <- inp_e()
      min_yr <- as.integer(min(years))
      max_yr <- as.integer(max(years))
      e <- gsub("'", "''", importer)

      # Get sector reference data
      sectors_ref <- setDT(pool::dbGetQuery(con,
        "select industry_descr, industry_id from itpd_industries"
      ))

      if (exporter == "ALL") {
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, importer_iso3_dynamic AS importer, exporter_iso3_dynamic AS exporter, industry_id, broad_sector_id, trade * 1000000 AS trade FROM %s WHERE year BETWEEN %d AND %d AND (importer_iso3_dynamic = '%s' OR exporter_iso3_dynamic = '%s') AND importer_iso3_dynamic != exporter_iso3_dynamic",
          tbl_dtl(), min_yr, max_yr, e, e
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, importer_iso3_dynamic AS importer, exporter_iso3_dynamic AS exporter, industry_id, broad_sector_id, trade * 1000000 AS trade FROM %s WHERE year BETWEEN %d AND %d AND (
            (importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s') OR
            (importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s')
          ) AND importer_iso3_dynamic != exporter_iso3_dynamic",
          tbl_dtl(), min_yr, max_yr, e, i, i, e
        )))
      }

      d <- merge(d, sectors_ref, by = "industry_id")
      return(d)
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      yrs <- c(min(inp_y()), max(inp_y()))
      df_agg()[year %in% yrs, .(year, trade_exp, trade_imp)]
    })

    exp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_exp]
    })

    exp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_exp]
    })

    imp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_imp]
    })

    imp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_imp]
    })

    exp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_min_yr())
    })

    exp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_max_yr())
    })

    imp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_min_yr())
    })

    imp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_max_yr())
    })

    exports_growth <- eventReactive(input$go, {
      growth_rate(
        exp_val_max_yr(), exp_val_min_yr(), inp_y()
      )
    })

    exports_growth_2 <- eventReactive(input$go, {
      show_percentage(abs(exports_growth()))
    })

    exports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increased", "decreased")
    })

    exports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increase", "decrease")
    })

    imports_growth <- eventReactive(input$go, {
      growth_rate(
        imp_val_max_yr(), imp_val_min_yr(), inp_y()
      )
    })

    imports_growth_2 <- eventReactive(input$go, {
      show_percentage(abs(imports_growth()))
    })

    imports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })

    imports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })

    trd_rankings <- eventReactive(input$go, {
      min_max_y <- c(min(inp_y()), max(inp_y()))
      year_in <- paste(as.integer(min_max_y), collapse = ",")
      e <- gsub("'", "''", inp_i())

      # Exports: importer is exporter_iso3_dynamic, exporter is importer_iso3_dynamic
      d_exp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, importer_iso3_dynamic AS exporter, SUM(trade) * 1000000 AS trade_exp
         FROM %s WHERE year IN (%s) AND exporter_iso3_dynamic = '%s'
         AND importer_iso3_dynamic != exporter_iso3_dynamic
         GROUP BY year, importer_iso3_dynamic",
        tbl_agg(), year_in, e
      )))

      # Imports: importer is importer_iso3_dynamic, exporter is exporter_iso3_dynamic
      d_imp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, exporter_iso3_dynamic AS exporter, SUM(trade) * 1000000 AS trade_imp
         FROM %s WHERE year IN (%s) AND importer_iso3_dynamic = '%s'
         AND importer_iso3_dynamic != exporter_iso3_dynamic
         GROUP BY year, exporter_iso3_dynamic",
        tbl_agg(), year_in, e
      )))

      d <- merge(d_exp, d_imp, by = c("year", "exporter"), all = TRUE)
      d[is.na(trade_exp), trade_exp := 0]
      d[is.na(trade_imp), trade_imp := 0]
      setnames(d, "exporter", "exporter_iso3_dynamic")

      d[, trd_value_usd_bal := trade_exp + trade_imp]
      d[, bal_rank := frankv(trd_value_usd_bal, order = -1L, ties.method = "dense"), by = .(year)]
      d[, exp_share := trade_exp / sum(trade_exp, na.rm = TRUE), by = .(year)]
      d[, imp_share := trade_imp / sum(trade_imp, na.rm = TRUE), by = .(year)]

      return(d)
    })

    # Helper function to get ranking with tie information
    get_ranking_with_ties <- function(year_val) {
      if (inp_e() == "ALL") {
        return("N/A") # No ranking for multilateral trade
      }

      rankings_data <- trd_rankings()[year == year_val]
      partner_iso_val <- inp_e()

      partner_rank <- rankings_data[exporter_iso3_dynamic == partner_iso_val, bal_rank]

      if (length(partner_rank) == 0 || is.na(partner_rank)) {
        return("N/A")
      }

      # Check for ties
      tied_count <- rankings_data[bal_rank == partner_rank & exporter_iso3_dynamic != partner_iso_val, .N]

      if (tied_count > 0) {
        return(paste0(
          partner_rank, " (tied with ", tied_count, " other",
          ifelse(tied_count == 1, "", "s"), ")"
        ))
      } else {
        return(as.character(partner_rank))
      }
    }

    trd_rankings_no_min_yr <- eventReactive(input$go, {
      get_ranking_with_ties(min(inp_y()))
    })

    trd_rankings_no_max_yr <- eventReactive(input$go, {
      get_ranking_with_ties(max(inp_y()))
    })

    trd_rankings_remained <- eventReactive(input$go, {
      min_rank <- trd_rankings_no_min_yr()
      max_rank <- trd_rankings_no_max_yr()

      if (min_rank == "N/A" || max_rank == "N/A") {
        return("was")
      }

      # Extract just the numeric part for comparison (remove tie information)
      min_rank_num <- as.numeric(gsub(" \\(.*\\)", "", min_rank))
      max_rank_num <- as.numeric(gsub(" \\(.*\\)", "", max_rank))

      ifelse(
        min_rank_num == max_rank_num,
        "remained",
        "moved to"
      )
    })

    trd_rankings_exp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == min(inp_y()) & exporter_iso3_dynamic == inp_e(), exp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_exp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_exp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == max(inp_y()) & exporter_iso3_dynamic == inp_e(), exp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_exp_share_max_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == min(inp_y()) & exporter_iso3_dynamic == inp_e(), imp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_imp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_imp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == max(inp_y()) & exporter_iso3_dynamic == inp_e(), imp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_imp_share_max_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_imp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    ### GDP Context Functions ----

    # Trade as average % of GDP over the selected period.
    # Inner-joined with dgd so only years with available GDP data contribute.
    gdp_pct_avg <- eventReactive(input$go, {
      e      <- gsub("'", "''", inp_i())
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))

      if (inp_e() == "ALL") {
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
      } else {
        i <- gsub("'", "''", inp_e())
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
      }

      gdp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT dgd.year, MAX(dgd.gdp_pwt_const_d) AS gdp, dc.region_colour
         FROM dgd
         JOIN dgd_colours dc ON dc.iso3_dynamic = dgd.iso3_dynamic_d
         WHERE dgd.iso3_dynamic_d = '%s' AND dgd.year BETWEEN %d AND %d
         GROUP BY dgd.year, dc.region_colour",
        e, min_yr, max_yr
      )))

      d <- merge(d_exp, d_imp, by = "year", all = TRUE)
      d <- merge(d, gdp, by = "year")  # inner join: keep only years with GDP data
      d[is.na(trade_exp), trade_exp := 0]
      d[is.na(trade_imp), trade_imp := 0]
      d[, exp_pct := round(trade_exp / gdp * 100, 2)]
      d[, imp_pct := round(trade_imp / gdp * 100, 2)]

      list(
        exp_pct       = if (nrow(d) == 0) NA_real_ else round(mean(d$exp_pct, na.rm = TRUE), 2),
        imp_pct       = if (nrow(d) == 0) NA_real_ else round(mean(d$imp_pct, na.rm = TRUE), 2),
        region_colour = if (nrow(d) == 0) NA_character_ else d$region_colour[1L]
      )
    })

    # Get total exports for bilateral context (when exporter != "ALL")
    total_exp_val_min_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(exp_val_min_yr())
      }

      min_year <- min(inp_y())
      importer <- inp_i()

      d <- pool::dbGetQuery(con, sprintf(
        "SELECT SUM(trade) * 1000000 AS total_trade FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic != exporter_iso3_dynamic",
        tbl_agg(), as.integer(min_year), gsub("'", "''", importer)
      ))

      return(d$total_trade)
    })

    total_exp_val_max_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(exp_val_max_yr())
      }

      max_year <- max(inp_y())
      importer <- inp_i()

      d <- pool::dbGetQuery(con, sprintf(
        "SELECT SUM(trade) * 1000000 AS total_trade FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic != exporter_iso3_dynamic",
        tbl_agg(), as.integer(max_year), gsub("'", "''", importer)
      ))

      return(d$total_trade)
    })

    # Get total imports for bilateral context (when exporter != "ALL")
    total_imp_val_min_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(imp_val_min_yr())
      }

      min_year <- min(inp_y())
      importer <- inp_i()

      d <- pool::dbGetQuery(con, sprintf(
        "SELECT SUM(trade) * 1000000 AS total_trade FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s' AND importer_iso3_dynamic != exporter_iso3_dynamic",
        tbl_agg(), as.integer(min_year), gsub("'", "''", importer)
      ))

      return(d$total_trade)
    })

    total_imp_val_max_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(imp_val_max_yr())
      }

      max_year <- max(inp_y())
      importer <- inp_i()

      d <- pool::dbGetQuery(con, sprintf(
        "SELECT SUM(trade) * 1000000 AS total_trade FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s' AND importer_iso3_dynamic != exporter_iso3_dynamic",
        tbl_agg(), as.integer(max_year), gsub("'", "''", importer)
      ))

      return(d$total_trade)
    })

    ### Text/Visual elements ----

    trd_smr_txt_exp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s exports to the world { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s exports to { r_add_the(pname()) } { pname() } { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_exp_share_min_yr_2() } of exports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_exp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      avg_pct <- gdp_pct_avg()$exp_pct

      if (!is.na(avg_pct) && avg_pct > 0) {
        if (inp_e() == "ALL") {
          gdp_context <- glue(" On average, total exports represented { avg_pct }% of { r_add_the(rname()) } { rname() }'s GDP over the period.")
        } else {
          gdp_context <- glue(" On average, exports to { r_add_the(pname()) } { pname() } represented { avg_pct }% of { r_add_the(rname()) } { rname() }'s GDP over the period.")
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_smr_txt_imp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s imports from the world { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s imports from { r_add_the(pname()) } { pname() } { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_imp_share_min_yr_2() } of imports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_imp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      avg_pct <- gdp_pct_avg()$imp_pct

      if (!is.na(avg_pct) && avg_pct > 0) {
        if (inp_e() == "ALL") {
          gdp_context <- glue(" On average, total imports represented { avg_pct }% of { r_add_the(rname()) } { rname() }'s GDP over the period.")
        } else {
          gdp_context <- glue(" On average, imports from { r_add_the(pname()) } { pname() } represented { avg_pct }% of { r_add_the(rname()) } { rname() }'s GDP over the period.")
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Imports and Exports { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    trd_exc_columns_agg <- reactive({
      d_all <- df_agg()

      d <- rbindlist(list(
        data.table(year = d_all$year, trade = round(d_all$trade_exp / 1e9, 2), flow = "Exports"),
        data.table(year = d_all$year, trade = round(d_all$trade_imp / 1e9, 2), flow = "Imports")
      ))
      d[, `:=`(year = as.character(year), color = fifelse(flow == "Exports", "#67c090", "#26667f"))]

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
        # po_tooltip("{flow}: {trade} B") |>
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
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("Exports of { r_add_the(rname()) } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by sector")
      } else {
        glue("Exports of { r_add_the(rname()) } { rname() } to { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by sector")
      }
    })

    # Export line chart: trade evolution over selected years
    trd_line_exp_tt <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Exports { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } exports to { r_add_the(pname()) } { pname() }: { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    trd_line_exp <- reactive({
      importer <- inp_i()
      exporter  <- inp_e()
      e        <- gsub("'", "''", importer)
      min_yr   <- as.integer(min(inp_y()))
      max_yr   <- as.integer(max(inp_y()))

      if (exporter == "ALL") {
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade FROM %s
           WHERE exporter_iso3_dynamic = '%s' AND year BETWEEN %d AND %d
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year
           ORDER BY year",
          tbl_agg(), e, min_yr, max_yr
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, trade * 1000000 AS trade FROM %s
           WHERE exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
             AND year BETWEEN %d AND %d
             AND importer_iso3_dynamic != exporter_iso3_dynamic
           ORDER BY year",
          tbl_agg(), e, i, min_yr, max_yr
        )))
      }

      d[, `:=`(year = as.character(year), trade = round(trade / 1e9, 2), group = "Exports", color = "#67c090")]

      d3po(d) |>
        po_line(daes(x = .data$year, y = .data$trade, group = .data$group, color = .data$color)) |>
        po_labels(x = "Year", y = "Exports (USD billion)", title = trd_line_exp_tt()) |>
        po_tooltip("{year}: {trade} B")
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Exports Composition in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      reporter <- inp_i()
      dtl <- df_dtl()
      actual_min_yr <- min(dtl$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl[year == actual_min_yr & exporter == reporter],
        col = "trade", con = con
      )
      d2 <- se_colors(d, con = con)
      se_treemap(d, d2, title = exp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Exports Composition in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      reporter <- inp_i()
      dtl <- df_dtl()
      actual_max_yr <- max(dtl$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl[year == actual_max_yr & exporter == reporter],
        col = "trade", con = con
      )
      d2 <- se_colors(d, con = con)
      se_treemap(d, d2, title = exp_tt_max_yr())
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("Imports of { r_add_the(rname()) } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by sector")
      } else {
        glue("Imports of { r_add_the(rname()) } { rname() } from { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by sector")
      }
    })

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Imports Composition in { min(inp_y()) }")
    })

    # Import line chart: trade evolution over selected years
    trd_line_imp_tt <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Imports { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } imports from { r_add_the(pname()) } { pname() }: { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    trd_line_imp <- reactive({
      importer <- inp_i()
      exporter  <- inp_e()
      e        <- gsub("'", "''", importer)
      min_yr   <- as.integer(min(inp_y()))
      max_yr   <- as.integer(max(inp_y()))

      if (exporter == "ALL") {
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade FROM %s
           WHERE importer_iso3_dynamic = '%s' AND year BETWEEN %d AND %d
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year
           ORDER BY year",
          tbl_agg(), e, min_yr, max_yr
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, trade * 1000000 AS trade FROM %s
           WHERE importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s'
             AND year BETWEEN %d AND %d
             AND importer_iso3_dynamic != exporter_iso3_dynamic
           ORDER BY year",
          tbl_agg(), e, i, min_yr, max_yr
        )))
      }

      d[, `:=`(year = as.character(year), trade = round(trade / 1e9, 2), group = "Imports", color = "#26667f")]

      d3po(d) |>
        po_line(daes(x = .data$year, y = .data$trade, group = .data$group, color = .data$color)) |>
        po_labels(x = "Year", y = "Imports (USD billion)", title = trd_line_imp_tt()) |>
        po_tooltip("{year}: {trade} B")
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    imp_tm_dtl_min_yr <- reactive({
      reporter <- inp_i()
      dtl <- df_dtl()
      actual_min_yr <- min(dtl$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl[year == actual_min_yr & importer == reporter],
        col = "trade", con = con
      )
      d2 <- se_colors(d, con = con)
      se_treemap(d, d2, title = imp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Imports Composition in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      reporter <- inp_i()
      dtl <- df_dtl()
      actual_max_yr <- max(dtl$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl[year == actual_max_yr & importer == reporter],
        col = "trade", con = con
      )
      d2 <- se_colors(d, con = con)
      out <- se_treemap(d, d2, title = imp_tt_max_yr())
      session$sendCustomMessage("hideProgress", list())
      return(out)
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    ## Dynamic / server side selectors ----

    observeEvent(input$i, {
      updateSelectizeInput(session, "e",
        choices = c(`All countries` = "ALL", tradestatisticsshiny::countries[tradestatisticsshiny::countries != input$i]),
        selected = "ALL",
        server = TRUE
      )
    })

    ## Download ----

    dwn_stl <- eventReactive(input$go, {
      "Download country data"
    })

    dwn_txt <- eventReactive(input$go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
    })

    dwn_fmt <- eventReactive(input$go, {
      selectInput(
        ns("fmt"),
        "Format",
        choices = available_formats(),
        selected = NULL,
        selectize = TRUE
      )
    })

    ## Outputs ----

    ## Titles ----

    output$title <- renderText({
      title()
    })

    ## Country profile ----

    ### Trade ----

    output$trd_stl <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("Total multilateral Exports and Imports")
      } else {
        glue("Total bilateral Exports and Imports")
      }
    })

    output$trd_stl_exp <- eventReactive(input$go, {
      "Exports"
    })
    output$trd_stl_imp <- eventReactive(input$go, {
      "Imports"
    })

    output$trd_smr_exp <- renderText(trd_smr_txt_exp())
    output$trd_smr_imp <- renderText(trd_smr_txt_imp())

    output$trd_exc_columns_agg <- render_d3po({
      trd_exc_columns_agg()
    })

    ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())

    # Export line chart outputs
    output$trd_line_exp <- render_d3po({
      trd_line_exp()
    })

    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- render_d3po({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- render_d3po({
      exp_tm_dtl_max_yr()
    })

    ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    # Import line chart outputs
    output$trd_line_imp <- render_d3po({
      trd_line_imp()
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

    output$dwn_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_i() }_{ inp_e() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_dtl(), filename)
      }
    )

    output$dwn_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_i() }_{ inp_e() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
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
