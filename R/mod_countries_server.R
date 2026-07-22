#' @title Country profile server-side function
#' @description A tabler Module.
#' @param id Internal parameter for Tabler.
#' @param con Shared SQL connection, created and closed by `app_server()`.
mod_countries_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Flat name -> code lookup for country display names (countries data is
    # grouped by region for the select inputs, see flatten_countries()).
    countries_lookup <- flatten_countries()

    # User inputs ----
    inp_i <- reactive({
      input$i
    }) # importer

    inp_e <- reactive({
      input$e
    }) # exporter

    inp_t <- reactive({
      input$t
    }) # table

    # Actual min/max year available for the selected dataset's table, read
    # from the DB rather than hardcoded - so it stays correct if a dataset's
    # coverage changes. This is the source of truth inp_y() clamps against
    # below: the slider's own min/max (also updated via updateSliderInput()
    # further down, for a nicer UX) can't be relied on alone, since it needs
    # a client round-trip before input$y reflects it.
    tbl_yr_range <- reactive({
      tbl <- paste0(inp_t(), "_imp")
      tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT MIN(year) AS min_yr, MAX(year) AS max_yr FROM %s",
          tbl
        )),
        error = function(e) NULL
      )
    }) |>
      bindCache(inp_t())

    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      rng <- tbl_yr_range()
      if (!is.null(rng) && nrow(rng) == 1 && !is.na(rng$min_yr)) {
        min_yr <- as.integer(rng$min_yr)
        max_yr <- as.integer(rng$max_yr)
        y <- c(
          max(min_yr, min(y[1], max_yr)),
          min(max_yr, max(y[2], min_yr))
        )
      }
      return(y)
    })

    inp_fmt <- reactive({
      fmt <- input$fmt
      if (is.null(fmt) || !nzchar(fmt)) "csv" else fmt
    }) # format

    # tbl_agg <- "itpde_imp_exp"
    # tbl_dtl <- "itpde"

    tbl_agg <- reactive(paste0(inp_t(), "_imp_exp"))
    tbl_dtl <- reactive(inp_t())

    # Force every slow (DB-backed) reactive to compute here, eagerly and in a
    # known order, bracketed by showProgress()/hideProgress(). This MUST be
    # the first thing registered that depends on input$go: tabler's reactive
    # engine invalidates/reruns input$go's dependents one at a time, in the
    # order they were registered, and each one's *entire* downstream cascade
    # (e.g. an output eagerly re-pulling a bindEvent(input$go) reactive) runs
    # to completion before the next dependent gets its turn. If this observer
    # were registered later (e.g. after the reactives/outputs below), some
    # other output would already eagerly trigger a heavy DB-backed reactive
    # before this observer ever ran - so showProgress() would only fire once
    # that other (already slow) work is done, i.e. right at the end, with the
    # UI looking frozen/stale for the preceding few seconds.
    observeEvent(input$go, {
      withProgress(session, "Loading data...", {
        df_agg()
        df_dtl()
        trd_exc_columns_agg()
        trd_line_exp()
        trd_line_imp()
        exp_tm_dtl_min_yr()
        exp_tm_dtl_max_yr()
        imp_tm_dtl_min_yr()
        imp_tm_dtl_max_yr()
        reporter_context()
        bilateral_context()
        reporter_sanctions()
        partner_sanctions()
      })
    })

    # Update year slider based on available data in the selected table ----
    observeEvent(input$t,
      {
        yr_range <- tbl_yr_range()
        if (!is.null(yr_range) && nrow(yr_range) == 1 && !is.na(yr_range$min_yr)) {
          min_yr <- as.integer(yr_range$min_yr)
          max_yr <- as.integer(yr_range$max_yr)
          cur_min <- min(input$y[1], input$y[2])
          cur_max <- max(input$y[1], input$y[2])
          sync_year_slider(session, "y",
            min = min_yr,
            max = max_yr,
            value = c(
              max(min_yr, min(cur_min, max_yr)),
              min(max_yr, max(cur_max, min_yr))
            )
          )
        }
      },
      ignoreInit = TRUE
    )

    # Also correct the slider/URL when the user drags to a range that's
    # invalid for the *already selected* table (the observer above only
    # fires on a dataset switch) - e.g. the default 2018-2022 selection is
    # out of range for ITPD-S (up to 2019), even with no change to input$t.
    # inp_y() already clamps for query purposes regardless, so this is purely
    # to keep what's displayed (slider + URL) from contradicting the actual
    # results shown below.
    observeEvent(input$y,
      {
        yr_range <- tbl_yr_range()
        if (is.null(yr_range) || nrow(yr_range) != 1 || is.na(yr_range$min_yr)) {
          return()
        }
        min_yr <- as.integer(yr_range$min_yr)
        max_yr <- as.integer(yr_range$max_yr)
        raw <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
        clamped <- c(
          max(min_yr, min(raw[1], max_yr)),
          min(max_yr, max(raw[2], min_yr))
        )
        if (!identical(raw, clamped)) {
          sync_year_slider(session, "y", min = min_yr, max = max_yr, value = clamped)
        }
      },
      ignoreInit = TRUE
    )

    # Human-readable importer/exporter names for glue templates. Fallback to
    # the code when no display name is available.
    rname <- eventReactive(input$go, {
      country_name_from_code(inp_i(), countries_lookup)
    })

    pname <- eventReactive(input$go, {
      country_name_from_code(inp_e(), countries_lookup)
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
      years <- inp_y()
      importer <- inp_i()
      exporter <- inp_e()
      min_yr <- as.integer(min(years))
      max_yr <- as.integer(max(years))
      e <- gsub("'", "''", importer)

      if (exporter == "ALL") {
        d_exp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
        d_imp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d_exp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
        d_imp <- setDT(dbGetQuery(con, sprintf(
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
      sectors_ref <- setDT(dbGetQuery(
        con,
        "select industry_descr, industry_id from itpd_industries"
      ))

      if (exporter == "ALL") {
        d <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, importer_iso3_dynamic AS importer, exporter_iso3_dynamic AS exporter, industry_id, broad_sector_id, trade * 1000000 AS trade FROM %s WHERE year BETWEEN %d AND %d AND (importer_iso3_dynamic = '%s' OR exporter_iso3_dynamic = '%s') AND importer_iso3_dynamic != exporter_iso3_dynamic",
          tbl_dtl(), min_yr, max_yr, e, e
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(dbGetQuery(con, sprintf(
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
      d_exp <- setDT(dbGetQuery(con, sprintf(
        "SELECT year, importer_iso3_dynamic AS exporter, SUM(trade) * 1000000 AS trade_exp
         FROM %s WHERE year IN (%s) AND exporter_iso3_dynamic = '%s'
         AND importer_iso3_dynamic != exporter_iso3_dynamic
         GROUP BY year, importer_iso3_dynamic",
        tbl_agg(), year_in, e
      )))

      # Imports: importer is importer_iso3_dynamic, exporter is exporter_iso3_dynamic
      d_imp <- setDT(dbGetQuery(con, sprintf(
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
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
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
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
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
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
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
      if (length(result) == 0 || is.na(result)) {
        return(0)
      }
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
      e <- gsub("'", "''", inp_i())
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))

      if (inp_e() == "ALL") {
        d_exp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
        d_imp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e
        )))
      } else {
        i <- gsub("'", "''", inp_e())
        d_exp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_exp
           FROM %s WHERE year BETWEEN %d AND %d AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
        d_imp <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_imp
           FROM %s WHERE year BETWEEN %d AND %d AND importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s'
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year",
          tbl_agg(), min_yr, max_yr, e, i
        )))
      }

      gdp <- setDT(dbGetQuery(con, sprintf(
        "SELECT dgd.year, MAX(dgd.gdp_pwt_const_d) AS gdp, dc.region_colour
         FROM dgd
         JOIN dgd_colours dc ON dc.iso3_dynamic = dgd.iso3_dynamic_d
         WHERE dgd.iso3_dynamic_d = '%s' AND dgd.year BETWEEN %d AND %d
         GROUP BY dgd.year, dc.region_colour",
        e, min_yr, max_yr
      )))

      d <- merge(d_exp, d_imp, by = "year", all = TRUE)
      d <- merge(d, gdp, by = "year") # inner join: keep only years with GDP data
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

      d <- dbGetQuery(con, sprintf(
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

      d <- dbGetQuery(con, sprintf(
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

      d <- dbGetQuery(con, sprintf(
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

      d <- dbGetQuery(con, sprintf(
        "SELECT SUM(trade) * 1000000 AS total_trade FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s' AND importer_iso3_dynamic != exporter_iso3_dynamic",
        tbl_agg(), as.integer(max_year), gsub("'", "''", importer)
      ))

      return(d$total_trade)
    })

    ### Background / country context ----

    # Population, GDP per capita and WTO/EU membership for the reporter,
    # taken from the most recent year within the selected range that has
    # data. If none of the selected years have data (e.g. the range is more
    # recent than the underlying dataset), no row is returned and this
    # section is omitted rather than showing stale figures from outside the
    # requested period.
    reporter_context <- eventReactive(input$go, {
      e <- gsub("'", "''", inp_i())
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))
      setDT(dbGetQuery(con, sprintf(
        "SELECT year, pop_o, gdp_wdi_cap_const_o, member_wto_o, member_eu_o
         FROM dgd
         WHERE iso3_dynamic_o = '%s' AND year BETWEEN %d AND %d
           AND pop_o IS NOT NULL AND gdp_wdi_cap_const_o IS NOT NULL
         ORDER BY year DESC
         LIMIT 1",
        e, min_yr, max_yr
      )))
    })

    # Bilateral gravity context (distance, contiguity, language, colonial ties)
    # only meaningful when a specific partner is selected. Same rule as
    # reporter_context(): restricted to the selected year range so nothing
    # outside the requested period is shown.
    bilateral_context <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(data.table())
      }
      e <- gsub("'", "''", inp_i())
      i <- gsub("'", "''", inp_e())
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))
      setDT(dbGetQuery(con, sprintf(
        "SELECT year, distance, colony_ever, common_colonizer, common_language, contiguity
         FROM dgd
         WHERE iso3_dynamic_o = '%s' AND iso3_dynamic_d = '%s' AND year BETWEEN %d AND %d
         ORDER BY year DESC
         LIMIT 1",
        e, i, min_yr, max_yr
      )))
    })

    reporter_sanctions <- eventReactive(input$go, {
      fetch_sanctions(con, inp_i(), inp_y())
    })

    partner_sanctions <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        return(data.table())
      }
      fetch_sanctions(con, inp_e(), inp_y())
    })

    country_background_txt <- eventReactive(input$go, {
      reporter_disp <- paste(r_add_upp_the(rname()), rname())
      partner_disp <- if (inp_e() != "ALL") paste(r_add_the(pname()), pname()) else NULL

      context_txt <- gravity_context_narrative(
        reporter_name = reporter_disp,
        reporter_info = reporter_context(),
        partner_name = partner_disp,
        bilateral_info = bilateral_context()
      )

      sanctions_reporter_txt <- sanctions_narrative(reporter_sanctions(), reporter_disp, countries_lookup)
      sanctions_partner_txt <- if (inp_e() != "ALL") {
        sanctions_narrative(partner_sanctions(), paste(r_add_upp_the(pname()), pname()), countries_lookup)
      } else {
        ""
      }

      paste(context_txt, sanctions_reporter_txt, sanctions_partner_txt)
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
      req(nrow(d_all) > 0)

      d <- rbindlist(list(
        data.table(year = d_all$year, trade = round(d_all$trade_exp / 1e9, 2), flow = "Exports"),
        data.table(year = d_all$year, trade = round(d_all$trade_imp / 1e9, 2), flow = "Imports")
      ))
      d[, `:=`(year = as.character(year), color = fifelse(flow == "Exports", "#67c090", "#26667f"))]

      d3po(d) |>
        po_bar(
          daes(x = .data$year, y = .data$trade, group = .data$flow, color = .data$color, stack = FALSE)
        ) |>
        po_labels(
          x = "Year", y = "Trade Value (USD billion)", title = trd_exc_columns_title()
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
      exporter <- inp_e()
      e <- gsub("'", "''", importer)
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))

      if (exporter == "ALL") {
        d <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade FROM %s
           WHERE exporter_iso3_dynamic = '%s' AND year BETWEEN %d AND %d
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year
           ORDER BY year",
          tbl_agg(), e, min_yr, max_yr
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(dbGetQuery(con, sprintf(
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
      req(nrow(dtl) > 0)
      dtl_exp <- dtl[exporter == reporter]
      req(nrow(dtl_exp) > 0)
      actual_min_yr <- min(dtl_exp$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl_exp[year == actual_min_yr],
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
      req(nrow(dtl) > 0)
      dtl_exp <- dtl[exporter == reporter]
      req(nrow(dtl_exp) > 0)
      actual_max_yr <- max(dtl_exp$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl_exp[year == actual_max_yr],
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
      exporter <- inp_e()
      e <- gsub("'", "''", importer)
      min_yr <- as.integer(min(inp_y()))
      max_yr <- as.integer(max(inp_y()))

      if (exporter == "ALL") {
        d <- setDT(dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) * 1000000 AS trade FROM %s
           WHERE importer_iso3_dynamic = '%s' AND year BETWEEN %d AND %d
           AND importer_iso3_dynamic != exporter_iso3_dynamic
           GROUP BY year
           ORDER BY year",
          tbl_agg(), e, min_yr, max_yr
        )))
      } else {
        i <- gsub("'", "''", exporter)
        d <- setDT(dbGetQuery(con, sprintf(
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
      req(nrow(dtl) > 0)
      dtl_imp <- dtl[importer == reporter]
      req(nrow(dtl_imp) > 0)
      actual_min_yr <- min(dtl_imp$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl_imp[year == actual_min_yr],
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
      req(nrow(dtl) > 0)
      dtl_imp <- dtl[importer == reporter]
      req(nrow(dtl_imp) > 0)
      actual_max_yr <- max(dtl_imp$year, na.rm = TRUE)
      d <- se_aggregate_by_sector(
        dtl_imp[year == actual_max_yr],
        col = "trade", con = con
      )
      d2 <- se_colors(d, con = con)
      se_treemap(d, d2, title = imp_tt_max_yr())
    }) |>
      bindCache(inp_y(), inp_i(), inp_e(), inp_t()) |>
      bindEvent(input$go)

    ## Dynamic / server side selectors ----

    observeEvent(input$i, {
      # countries is grouped by region (list of vectors); filter each group's
      # vector individually and drop groups left empty, rather than comparing
      # the whole (nested) list to a scalar.
      remaining <- lapply(tradestatisticsdashboard::countries, function(x) x[x != input$i])
      remaining <- remaining[lengths(remaining) > 0]
      updateSelectizeInput(session, "e",
        choices = c(`All countries` = "ALL", remaining),
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

    dwn_ctrl <- eventReactive(input$go, {
      tagList(
        selectInput(
          ns("fmt"),
          "Format",
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

    ## Outputs ----

    ## Titles ----

    output$title <- renderText({
      title()
    })

    ## Background information ----

    output$country_background <- renderText(country_background_txt())

    ## Country profile ----

    ### Trade ----

    trd_stl <- eventReactive(input$go, {
      if (inp_e() == "ALL") {
        glue("Total multilateral Exports and Imports")
      } else {
        glue("Total bilateral Exports and Imports")
      }
    })

    trd_stl_exp <- eventReactive(input$go, {
      "Exports"
    })
    trd_stl_imp <- eventReactive(input$go, {
      "Imports"
    })

    output$trd_stl <- renderText(trd_stl())
    output$trd_stl_exp <- renderText(trd_stl_exp())
    output$trd_stl_imp <- renderText(trd_stl_imp())

    output$trd_smr_exp <- renderText(trd_smr_txt_exp())
    output$trd_smr_imp <- renderText(trd_smr_txt_imp())

    output$trd_exc_columns_agg <- renderWidget({
      trd_exc_columns_agg()
    })

    ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())

    # Export line chart outputs
    output$trd_line_exp <- renderWidget({
      trd_line_exp()
    })

    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- renderWidget({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- renderWidget({
      exp_tm_dtl_max_yr()
    })

    ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    # Import line chart outputs
    output$trd_line_imp <- renderWidget({
      trd_line_imp()
    })

    output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
    output$imp_tm_dtl_min_yr <- renderWidget({
      imp_tm_dtl_min_yr()
    })
    output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
    output$imp_tm_dtl_max_yr <- renderWidget({
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
    output$dwn_ctrl <- renderUI({
      dwn_ctrl()
    })

    # Hide boxes until viz is ready ----

    ## observe the button being pressed
    observeEvent(input$go, {
      if (input$go > 0) {
        show(id = "title_section")
        show(id = "background_info")
        show(id = "aggregated_trade")
        show(id = "detailed_trade_exp")
        show(id = "detailed_trade_imp")
        show(id = "download_data")
      }
    })
  })
}
