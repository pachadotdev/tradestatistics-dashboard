#' @title Available Server parameters expressed as functions
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

# Read credentials from file excluded in .gitignore
readRenviron("/tradestatistics/credentials.txt")

#' @title SQL connection
sql_con <- function() {
  dbPool(
    drv = Postgres(),
    dbname = Sys.getenv("TRADESTATISTICS_SQL_NAME"),
    host = Sys.getenv("TRADESTATISTICS_SQL_HOST"),
    user = Sys.getenv("TRADESTATISTICS_SQL_USER"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PASSWORD"),
    port = Sys.getenv("TRADESTATISTICS_SQL_PORT")
  )
}

# TIME ----

#' @title Get Current Year
get_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
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

  if (nrow(d) == 0L || nrow(d2) == 0L || !("continent_name" %in% names(d))) return(NULL)

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

  d3po(dd) %>%
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$continent_name,
        subgroup = .data$country_name,
        color = .data$color,
        tiling = "binary"
      )
    ) %>%
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
    ) %>%
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

# PRODUCT TREEMAPS ----

#' @title Add Percentages to Sections
#' @param d input dataset
#' @param col column to collapse
#' @param con SQL connection
p_aggregate_by_sector <- function(d, col, con) {
  d <- setDT(copy(d))
  d <- d[, c("industry_id", "broad_sector_id", col), with = FALSE]
  setnames(d, col, "trade_value")

  d <- p_aggregate_products(d, con = con)

  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]

  d[, sum_trade_value := sum(trade_value, na.rm = TRUE), by = .(broad_sector)]
  setorder(d, -sum_trade_value)
  d[, sum_trade_value := NULL]

  sections <- unique(d[["broad_sector"]])
  if (length(sections) == 0L) return(d)
  d <- rbindlist(lapply(sections, function(s) {
    setorder(d[broad_sector == s], -trade_value)
  }))

  return(d)
}

#' @title Colorize Products
#' @param d input dataset
#' @param con SQL connection
p_colors <- function(d, con) {
  d <- setDT(copy(d))
  if (!("broad_sector" %in% names(d)) || nrow(d) == 0L) return(data.table())
  sectors <- unique(d[["broad_sector"]])
  colors_ref <- setDT(pool::dbGetQuery(con,
    "SELECT s.broad_sector AS broad_sector, c.colour AS sector_color
     FROM itpd_sectors s JOIN itpd_colours c ON s.broad_sector_id = c.broad_sector_id"
  ))
  colors_ref[broad_sector %in% sectors]
}

#' @title Aggregate Products
#' @param d input dataset
#' @param con SQL connection
p_aggregate_products <- function(d, con) {
  industries_ref <- setDT(pool::dbGetQuery(con,
    "SELECT industry_id, industry_descr AS commodity_name FROM itpd_industries"
  ))
  sectors_ref <- setDT(pool::dbGetQuery(con,
    "SELECT broad_sector_id, broad_sector AS broad_sector FROM itpd_sectors"
  ))
  colours_ref <- setDT(pool::dbGetQuery(con,
    "SELECT broad_sector_id, colour AS sector_color FROM itpd_colours"
  ))

  d <- setDT(copy(d))
  d <- merge(d, industries_ref, by = "industry_id")
  d <- merge(d, sectors_ref, by = "broad_sector_id")
  d <- merge(d, colours_ref, by = "broad_sector_id")
  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)),
         by = .(industry_id, broad_sector_id, sector_color, broad_sector, commodity_name)]
  return(d)
}

#' @title Product Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
p_treemap <- function(d, d2, title = NULL) {
  d <- setDT(copy(d))
  d2 <- setDT(copy(d2))

  if (nrow(d) == 0L || nrow(d2) == 0L || !("broad_sector" %in% names(d))) return(NULL)

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

  d3po(dd) %>%
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$broad_sector,
        subgroup = .data$commodity_name,
        color = .data$color,
        tiling = "binary"
      )
    ) %>%
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
    ) %>%
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

#' @title Typing reactiveValues is too long
#' @param ... elements to pass to the function
#' @rdname reactives
rv <- function(...) shiny::reactiveValues(...)

#' @rdname reactives
rvtl <- function(...) shiny::reactiveValuesToList(...)
