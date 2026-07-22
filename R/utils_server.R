#' @title Available Server parameters expressed as functions
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

#' @title Open SQL connection
open_con <- function() {
  dbConnect(
    drv = Postgres(),
    dbname = Sys.getenv("TRADESTATISTICS_SQL_NAME"),
    host = Sys.getenv("TRADESTATISTICS_SQL_HOST"),
    user = Sys.getenv("TRADESTATISTICS_SQL_USER"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PASSWORD"),
    port = Sys.getenv("TRADESTATISTICS_SQL_PORT")
  )
}

#' @title CLose SQL connection
close_con <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}

# TIME ----

#' @title Get Current Year
get_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
}

#' @title Correct the year slider (and bookmarkable URL) to a valid range
#' @description `updateSliderInput()` alone fixes the slider's visible
#'   position/badge, but the "y" parameter mirrored into the URL by
#'   `syncUrl()` (see app_server.R) only updates from real client-side
#'   interaction with the slider - not from a server-pushed correction like
#'   this one - so left alone it would keep showing the raw, out-of-range
#'   selection even after the slider itself has been corrected. This sends an
#'   extra small custom message (handled by the inline script added in
#'   `add_external_resources()`, app_ui.R) that fixes the URL query string to
#'   match.
#' @param session module session (its `$ns()` is used to namespace `inputId`)
#' @param inputId slider input id, unnamespaced (e.g. `"y"`)
#' @param min,max,value new slider bounds/value, as for `updateSliderInput()`
sync_year_slider <- function(session, inputId, min, max, value) {
  updateSliderInput(session, inputId, min = min, max = max, value = value)
  id <- if (is.function(session[["ns"]])) session$ns(inputId) else inputId
  session$sendCustomMessage("tabler-syncYearUrl", list(id = id, value = value))
}

# DATA VALIDATION ----

#' @title Ensure data frame contains expected columns to avoid mutate/group_by errors
#' @param df input data frame
#' @param cols vector of expected column names
ensure_cols <- function(df, cols) {
  for (c in cols) {
    if (!c %in% names(df)) df[[c]] <- NA_character_
  }
  df
}

# ORIGIN/DESTINATION TREEMAPS -----

#' @title Origin-Destination Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
od_treemap <- function(d, d2, title = NULL) {
  d <- setDT(copy(d))
  d2 <- setDT(copy(d2))

  if (nrow(d) == 0L || nrow(d2) == 0L || !("continent_name" %in% names(d))) {
    return(NULL)
  }

  d$continent_name <- factor(d$continent_name, levels = d2$continent_name)
  setorder(d, continent_name)

  # Compute level order by total trade value
  lvl_dt <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(continent_name, country_name)]
  lvl_dt$continent_name <- as.character(lvl_dt$continent_name)
  setorder(lvl_dt, -trade_value)
  new_lvls <- unique(lvl_dt$continent_name)

  # Build ordered colors vector
  d2_ord <- copy(d2)
  d2_ord$continent_name <- factor(d2_ord$continent_name, levels = new_lvls)
  setorder(d2_ord, continent_name)
  new_colors <- d2_ord$country_color

  # Aggregate dd and join colors
  dd <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(continent_name, country_name)]
  colors_join <- copy(d2)[, .(continent_name, color = country_color)]
  dd <- merge(dd, colors_join, by = "continent_name", all.x = TRUE)

  d3po(dd) |>
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$continent_name,
        subgroup = .data$country_name,
        color = .data$color,
        tiling = "binary"
      )
    ) |>
    po_labels(
      align = "left-top",
      title = title,
      subtitle = JS(
        "function(_v, row) { if (row && row.mode === 'drilled') return 'Displaying Countries'; return 'Displaying Continents'; }"
      ),
      labels = JS(
        "function(percentage, row) {
          var pct = (percentage).toFixed(2) + '%';

          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }

          function formatBillion(v) {
            var s = (Number(v) / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }

          var group = (row && (row.group || row.continent_name || row.name)) ? (row.group || row.continent_name || row.name) : '';
          var subgroup = (row && (row.subgroup || row.country_name)) ? (row.subgroup || row.country_name) : '';
          var rawValue = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
          var value = formatBillion(rawValue);

          if (!row || !subgroup) {
            return group + '<br/>' + value + '<br/>' + pct;
          }

          return subgroup + '<br/>' + value + '<br/>' + pct;
        }"
      )
    ) |>
    po_tooltip(JS(
      "function(percentage, row) {
        var pct = (percentage).toFixed(2) + '%';

        var forceBillions = true;
        function formatNumber(v) {
          if (v === null || v === undefined || v === '') return '';
          var n = Number(v);
          if (isNaN(n)) return String(v);
          var abs = Math.abs(n);
          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }
          if (forceBillions) {
            var s = (n / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }
          if (abs >= 1e9) { var s = (n / 1e9).toFixed(2); return stripZeros(s) + 'B'; }
          if (abs >= 1e6) { var s = (n / 1e6).toFixed(2); return stripZeros(s) + 'M'; }
          if (abs >= 1e3) { var s = (n / 1e3).toFixed(1); return stripZeros(s) + 'k'; }
          return n.toLocaleString(undefined, {maximumFractionDigits: 2});
        }

        var group = (row && (row.group || row.continent_name || row.name)) ? (row.group || row.continent_name || row.name) : '';
        var subgroup = (row && (row.subgroup || row.country_name)) ? (row.subgroup || row.country_name) : '';
        var raw = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
        var value = formatNumber(raw);

        if (!row || !subgroup) {
          return '<b>' + group + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
        }

        return '<b>' + subgroup + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
      }"
    ))
}

# SECTOR TREEMAPS ----

#' @title Add Percentages to Sections
#' @param d input dataset
#' @param col column to collapse
#' @param con SQL connection
se_aggregate_by_sector <- function(d, col, con) {
  d <- setDT(copy(d))
  d <- d[, c("industry_id", "broad_sector_id", col), with = FALSE]
  setnames(d, col, "trade_value")

  d <- se_aggregate_sectors(d, con = con)

  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]

  d[, sum_trade_value := sum(trade_value, na.rm = TRUE), by = .(broad_sector)]
  setorder(d, -sum_trade_value)
  d[, sum_trade_value := NULL]

  sections <- unique(d[["broad_sector"]])
  if (length(sections) == 0L) {
    return(d)
  }
  d <- rbindlist(lapply(sections, function(s) {
    setorder(d[broad_sector == s], -trade_value)
  }))

  return(d)
}

#' @title Colorize Sectors
#' @param d input dataset
#' @param con SQL connection
se_colors <- function(d, con) {
  d <- setDT(copy(d))
  if (!("broad_sector" %in% names(d)) || nrow(d) == 0L) {
    return(data.table())
  }
  sectors <- unique(d[["broad_sector"]])
  colors_ref <- setDT(dbGetQuery(
    con,
    "SELECT s.broad_sector AS broad_sector, c.colour AS sector_color
     FROM itpd_sectors s JOIN itpd_colours c ON s.broad_sector_id = c.broad_sector_id"
  ))
  colors_ref[broad_sector %in% sectors]
}

#' @title Aggregate Sectors
#' @param d input dataset
#' @param con SQL connection
se_aggregate_sectors <- function(d, con) {
  industries_ref <- setDT(dbGetQuery(
    con,
    "SELECT industry_id, industry_descr AS commodity_name FROM itpd_industries"
  ))
  sectors_ref <- setDT(dbGetQuery(
    con,
    "SELECT broad_sector_id, broad_sector AS broad_sector FROM itpd_sectors"
  ))
  colours_ref <- setDT(dbGetQuery(
    con,
    "SELECT broad_sector_id, colour AS sector_color FROM itpd_colours"
  ))

  d <- setDT(copy(d))
  d <- merge(d, industries_ref, by = "industry_id")
  d <- merge(d, sectors_ref, by = "broad_sector_id")
  d <- merge(d, colours_ref, by = "broad_sector_id")
  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)),
    by = .(industry_id, broad_sector_id, sector_color, broad_sector, commodity_name)
  ]
  return(d)
}

#' @title Sector Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
se_treemap <- function(d, d2, title = NULL) {
  d <- setDT(copy(d))
  d2 <- setDT(copy(d2))

  if (nrow(d) == 0L || nrow(d2) == 0L || !("broad_sector" %in% names(d))) {
    return(NULL)
  }

  d$broad_sector <- factor(d$broad_sector, levels = d2$broad_sector)
  setorder(d, broad_sector)

  # Compute new level order by total trade value per section
  lvl_dt <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]
  lvl_dt$broad_sector <- as.character(lvl_dt$broad_sector)
  setorder(lvl_dt, -trade_value)
  new_lvls <- unique(lvl_dt$broad_sector)

  # Build ordered colors vector
  d2_ord <- copy(d2)
  d2_ord$broad_sector <- factor(d2_ord$broad_sector, levels = new_lvls)
  setorder(d2_ord, broad_sector)
  new_colors <- d2_ord$sector_color

  # Aggregate dd and join colors
  dd <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]
  colors_join <- copy(d2)[, .(broad_sector, color = sector_color)]
  dd <- merge(dd, colors_join, by = "broad_sector", all.x = TRUE)

  d3po(dd) |>
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$broad_sector,
        subgroup = .data$commodity_name,
        color = .data$color,
        tiling = "binary"
      )
    ) |>
    po_labels(
      align = "left-top",
      title = title,
      subtitle = JS(
        "function(_v, row) {
          if (row && row.mode === 'drilled') {
            return 'Displaying Industries';
          } else {
            return 'Displaying Sectors';
          }
        }"
      ),
      labels = JS(
        "function(percentage, row) {
          // format percentage with two decimals
          var pct = (percentage).toFixed(2) + '%';

          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }

          function formatBillion(v) {
            var s = (Number(v) / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }

          // prefer row.group/row.subgroup fields from the d3po/po_treemap internals
          var section = (row && (row.group || row.sector_name || row.name)) ? (row.group || row.sector_name || row.name) : '';
          var commodity = (row && (row.subgroup || row.commodity_name)) ? (row.subgroup || row.commodity_name) : '';
          var rawValue = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
          var value = formatBillion(rawValue);

          // If no subgroup present (level 1), show only the section
          if (!row || !commodity) {
            return section + '<br/>' + value + '<br/>' + pct;
          }

          // Level 2: show commodity only (not repeated section + commodity)
          return commodity + '<br/>' + value + '<br/>' + pct;
        }"
      )
    ) |>
    po_tooltip(JS(
      "function(percentage, row) {
        var pct = (percentage).toFixed(2) + '%';

        // tooltip formatter: duplicate the same formatter used by labels to avoid R escaping issues
        var forceBillions = true;
        function formatNumber(v) {
          if (v === null || v === undefined || v === '') return '';
          var n = Number(v);
          if (isNaN(n)) return String(v);
          var abs = Math.abs(n);
          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }
          if (forceBillions) {
            var s = (n / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }
          if (abs >= 1e9) { var s = (n / 1e9).toFixed(2); return stripZeros(s) + 'B'; }
          if (abs >= 1e6) { var s = (n / 1e6).toFixed(2); return stripZeros(s) + 'M'; }
          if (abs >= 1e3) { var s = (n / 1e3).toFixed(1); return stripZeros(s) + 'k'; }
          return n.toLocaleString(undefined, {maximumFractionDigits: 2});
        }

        // prefer row.group/row.subgroup fields from the d3po/po_treemap internals
        var section = (row && (row.group || row.sector_name || row.name)) ? (row.group || row.sector_name || row.name) : '';
        var commodity = (row && (row.subgroup || row.commodity_name)) ? (row.subgroup || row.commodity_name) : '';
        var raw = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
        var value = formatNumber(raw);

        // If no subgroup present (level 1), show only the section
        if (!row || !commodity) {
          return '<b>' + section + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
        }

        // Level 2: show commodity only (not repeated section + commodity)
        return '<b>' + commodity + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
      }"
    ))
}

#' @title Add definite article for reporter names
#' @description Grammar helper function that adds "the" for reporter names such as
#' "United Kingdom" and "United States"
#' @param name Character string of the reporter name
#' @return Character string: "the" for names requiring the article, empty string otherwise
r_add_the <- function(name = NULL) {
  # Return 'the' for reporter names that typically take the article
  if (is.null(name)) {
    return("")
  }
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

#' @title Add capitalized definite article for reporter names
#' @description Grammar helper function that adds "The" (capitalized) for reporter names that
#'  typically take the definite article, used at the beginning of sentences.
#' @param name Character string of the reporter name
#' @return Character string: "The" for names requiring the article, empty string otherwise
r_add_upp_the <- function(name = NULL) {
  v <- r_add_the(name)
  if (nchar(v) == 0) {
    return("")
  }
  # Capitalize only the first letter ("The") rather than returning all caps
  paste0(toupper(substr(v, 1, 1)), tolower(substr(v, 2, nchar(v))))
}

# FORMAT TEXTS ----

#' @title Format for Dollars
#' @param x input number
show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
    paste0(round(x / 10e8, 2), "B"),
    paste0(round(x / 10e5, 2), "M")
  )
}

#' @title Format for Percentages
#' @param x input number
show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

#' @title Compute Compound Annualized Growth Rate
#' @param p final value
#' @param q initial value
#' @param t time period
growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

# COUNTRY LOOKUPS ----

#' @title Flatten the region-grouped countries data into a single name -> code vector
#' @description `tradestatisticsdashboard::countries` is a list of named vectors
#' (one per region, name = country, value = dynamic code), used to build grouped
#' select inputs. This collapses it into one flat named vector for lookups.
flatten_countries <- function() {
  do.call(c, unname(tradestatisticsdashboard::countries))
}

#' @title Look up a country's display name from its dynamic code
#' @param code Dynamic country code (e.g. "GBR")
#' @param lookup Flat name -> code vector, as returned by `flatten_countries()`
#' @return The country name, or `code` itself if not found
country_name_from_code <- function(code, lookup) {
  if (is.null(code) || length(code) == 0 || is.na(code) || nchar(code) == 0) {
    return(code)
  }
  out <- names(lookup)[match(code, lookup)]
  if (length(out) == 0 || is.na(out[1]) || nchar(out[1]) == 0) {
    return(code)
  }
  out[1]
}

# NARRATIVE TEXT (BACKGROUND / SANCTIONS) ----

#' @title Join a vector of items into a human-readable "a, b and c" list
format_list_and <- function(x) {
  x <- unique(x)
  x <- x[!is.na(x) & nchar(x) > 0]
  if (length(x) == 0) {
    return("")
  }
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

#' @title Human-readable labels for GSDB sanction objectives
sanction_objective_labels <- c(
  obj_democracy = "promoting democracy",
  obj_destab_regime = "destabilizing the government",
  obj_end_war = "ending a war",
  obj_human_rights = "addressing human rights violations",
  obj_other = "other objectives",
  obj_policy_change = "policy change",
  obj_prevent_war = "preventing war",
  obj_territorial_conflict = "resolving a territorial conflict",
  obj_terrorism = "countering terrorism"
)

#' @title Fetch GSDB dyadic sanctions imposed against a country in a year range
#' @param con SQL connection
#' @param code Dynamic code of the sanctioned country
#' @param years Numeric vector of years to filter on (min/max are used)
fetch_sanctions <- function(con, code, years) {
  if (is.null(code) || is.na(code) || nchar(code) == 0) {
    return(data.table())
  }
  e <- gsub("'", "''", code)
  min_yr <- as.integer(min(years))
  max_yr <- as.integer(max(years))
  setDT(dbGetQuery(con, sprintf(
    "SELECT case_id, sanctioning_state_dynamic, trade, financial,
            obj_democracy, obj_destab_regime, obj_end_war, obj_human_rights, obj_other,
            obj_policy_change, obj_prevent_war, obj_territorial_conflict, obj_terrorism,
            suc_failed, suc_nego_settlement, suc_ongoing, suc_success_part, suc_success_total
     FROM gsdb_dyadic
     WHERE sanctioned_state_dynamic = '%s' AND year BETWEEN %d AND %d",
    e, min_yr, max_yr
  )))
}

#' @title Build a Wikipedia-style paragraph describing sanctions applied against a country
#' @param d data.table as returned by `fetch_sanctions()`
#' @param country_name Display name of the sanctioned country (article included, e.g. "the United Kingdom")
#' @param lookup Flat name -> code vector, as returned by `flatten_countries()`, used to name the senders
sanctions_narrative <- function(d, country_name, lookup) {
  if (is.null(d) || nrow(d) == 0) {
    return("")
  }

  n_cases <- uniqueN(d$case_id)
  n_trade <- uniqueN(d[trade == 1, case_id])
  n_financial <- uniqueN(d[financial == 1, case_id])

  senders_codes <- unique(d$sanctioning_state_dynamic)
  senders <- vapply(senders_codes, country_name_from_code, character(1), lookup = lookup)
  senders_txt <- format_list_and(senders)
  senders_txt <- gsub("\\s+\\(*.ISO.*\\)", "", senders_txt)

  obj_cols <- intersect(names(sanction_objective_labels), names(d))
  objectives <- sanction_objective_labels[obj_cols[vapply(obj_cols, function(col) any(d[[col]] == 1, na.rm = TRUE), logical(1))]]
  objectives_txt <- format_list_and(unname(objectives))

  outcome_txt <- if (any(d$suc_success_total == 1, na.rm = TRUE)) {
    "were largely successful"
  } else if (any(d$suc_success_part == 1, na.rm = TRUE)) {
    "were partially successful"
  } else if (any(d$suc_nego_settlement == 1, na.rm = TRUE)) {
    "led to a negotiated settlement"
  } else if (any(d$suc_ongoing == 1, na.rm = TRUE)) {
    "remain ongoing"
  } else if (any(d$suc_failed == 1, na.rm = TRUE)) {
    "failed to achieve their stated goals"
  } else {
    "had an undetermined outcome"
  }

  type_txt <- if (n_trade > 0 && n_financial > 0) {
    "both trade and financial sanctions"
  } else if (n_trade > 0) {
    "trade sanctions"
  } else if (n_financial > 0) {
    "financial sanctions"
  } else {
    "sanctions"
  }

  objectives_sentence <- if (nchar(objectives_txt) > 0) {
    glue(" The stated objectives included { objectives_txt }.")
  } else {
    ""
  }

  glue("{ country_name } was subject to { n_cases } sanction{ ifelse(n_cases == 1, '', 's') } imposed by { senders_txt }, involving { type_txt }.{ objectives_sentence } These measures { outcome_txt }.")
}

#' @title Build a Wikipedia-style paragraph with population, GDP per capita and bilateral gravity context
#' @param reporter_name Display name of the reporter country (article included)
#' @param reporter_info Single-row data.table from the `dgd` reporter-context query
#' @param partner_name Display name of the partner country (article included), or `NULL` for multilateral profiles
#' @param bilateral_info Single-row data.table from the `dgd` bilateral-context query, or `NULL`
gravity_context_narrative <- function(reporter_name, reporter_info, partner_name = NULL, bilateral_info = NULL) {
  base <- ""
  if (!is.null(reporter_info) && nrow(reporter_info) > 0) {
    yr <- reporter_info$year[1]
    pop <- reporter_info$pop_o[1]
    gdp_cap <- reporter_info$gdp_wdi_cap_const_o[1]

    if (!is.na(pop) && !is.na(gdp_cap)) {
      pop_txt <- paste0(format(round(pop, 1), nsmall = 1), " million")
      gdp_cap_txt <- paste0("$", formatC(round(gdp_cap), format = "d", big.mark = ","))
      base <- glue("As of { yr }, { reporter_name } had a population of { pop_txt } and a GDP per capita of { gdp_cap_txt }.")
      base <- gsub(", The", ", the", base)
    }

    membership <- character(0)
    if (isTRUE(reporter_info$member_wto_o[1] == 1)) membership <- c(membership, "the World Trade Organization (WTO)")
    if (isTRUE(reporter_info$member_eu_o[1] == 1)) membership <- c(membership, "the European Union (EU)")
    if (length(membership) > 0) {
      base <- paste0(base, glue(" { reporter_name } is a member of { format_list_and(membership) }."))
    }
  }

  bilateral_sentence <- ""
  if (!is.null(bilateral_info) && nrow(bilateral_info) > 0 && !is.null(partner_name)) {
    dist <- bilateral_info$distance[1]
    if (!is.na(dist)) {
      dist_txt <- paste0(formatC(round(dist), format = "d", big.mark = ","), " km")
      bilateral_sentence <- paste0(bilateral_sentence, glue(" { reporter_name } and { partner_name } are separated by a distance of approximately { dist_txt }."))
    }
    if (isTRUE(bilateral_info$contiguity[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " The two countries share a land border.")
    }
    if (isTRUE(bilateral_info$common_language[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " They also share a common official language.")
    }
    if (isTRUE(bilateral_info$colony_ever[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, glue(" { reporter_name } and { partner_name } have a shared colonial history."))
    }
    if (isTRUE(bilateral_info$common_colonizer[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " Both countries were once part of the same colonial empire.")
    }
  }

  trimws(paste0(base, bilateral_sentence))
}

#' @title Typing reactiveValues is too long
#' @param ... elements to pass to the function
#' @rdname reactives
rv <- function(...) tabler::reactiveValues(...)

#' @rdname reactives
rvtl <- function(...) tabler::reactiveValuesToList(...)
