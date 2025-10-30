#' @title Available Server parameters expressed as functions
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

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
  dd <- d %>%
    mutate(
      continent_name = factor(!!sym("continent_name"),
        levels = d2$continent_name
      )
    ) %>%
    arrange(!!sym("continent_name"))

  new_lvls <- dd %>%
    group_by(!!sym("continent_name"), !!sym("country_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop") %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("continent_name")) %>%
    pull(!!sym("continent_name"))

  new_colors <- d2 %>%
    mutate(
      continent_name = factor(!!sym("continent_name"),
        levels = new_lvls
      )
    ) %>%
    arrange(!!sym("continent_name")) %>%
    pull(!!sym("country_color"))

  dd <- dd %>%
    group_by(!!sym("continent_name"), !!sym("country_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = TRUE), .groups = "drop") %>%
    left_join(
      d2 %>%
        rename(color = !!sym("country_color")),
      by = "continent_name"
    )

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
    )) %>%
    po_background("transparent")
}

# PRODUCT TREEMAPS ----

#' @title Add Percentages to Sections
#' @param d input dataset
#' @param col column to collapse
#' @param con SQL connection
p_aggregate_by_section <- function(d, col, con) {
  d <- d %>%
    select(!!sym("commodity_code"), !!sym("section_code"), !!sym(col)) %>%
    rename(trade_value = !!sym(col))

  d <- p_aggregate_products(d, con = con)

  d <- d %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T), .groups = "drop") %>%
    ungroup()

  d <- d %>%
    select(!!sym("section_name"), !!sym("commodity_name"), !!sym("trade_value")) %>%
    group_by(!!sym("section_name")) %>%
    mutate(sum_trade_value = sum(!!sym("trade_value"), na.rm = T)) %>%
    ungroup() %>%
    arrange(-!!sym("sum_trade_value")) %>%
    select(-!!sym("sum_trade_value"))

  d <- map_df(
    d %>%
      select(!!sym("section_name")) %>%
      distinct() %>%
      pull(),
    function(s) {
      d %>%
        filter(!!sym("section_name") == s) %>%
        arrange(-!!sym("trade_value"))
    }
  )

  return(d)
}

#' @title Colorize Products
#' @param d input dataset
#' @param con SQL connection
p_colors <- function(d, con) {
  d %>%
    distinct(!!sym("section_name")) %>%
    inner_join(
      tbl(con, "commodities") %>%
        select(
          !!sym("section_code"),
          !!sym("section_name"),
          !!sym("section_color")
        ) %>%
        distinct() %>%
        collect(),
      by = "section_name"
    )
}

#' @title Aggregate Products
#' @param d input dataset
#' @param con SQL connection
p_aggregate_products <- function(d, con) {
  # Get all commodity data in one query to avoid multiple database hits
  commodities_data <- tbl(con, "commodities") %>%
    select(
      !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"),
      !!sym("section_color"), !!sym("section_name")
    ) %>%
    left_join(
      tbl(con, "commodities_short") %>%
        select(!!sym("commodity_code"),
          commodity_name = !!sym("commodity_name")
        ),
      by = c("commodity_code_short" = "commodity_code")
    ) %>%
    collect()

  d %>%
    inner_join(commodities_data, by = c("commodity_code", "section_code")) %>%
    group_by(
      !!sym("commodity_code"), !!sym("commodity_code_short"), !!sym("section_code"),
      !!sym("section_color"), !!sym("section_name"), !!sym("commodity_name")
    ) %>%
    summarise(trade_value = sum(!!sym("trade_value"), na.rm = T), .groups = "drop") %>%
    ungroup()
}

#' @title Product Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
p_treemap <- function(d, d2, title = NULL) {
  dd <- d %>%
    mutate(section_name = factor(!!sym("section_name"), levels = d2$section_name)) %>%
    arrange(!!sym("section_name"))

  new_lvls <- dd %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(
      trade_value = sum(!!sym("trade_value"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate_if(is.factor, as.character) %>%
    arrange(desc(!!sym("trade_value"))) %>%
    distinct(!!sym("section_name")) %>%
    pull(!!sym("section_name"))

  new_colors <- d2 %>%
    mutate(section_name = factor(!!sym("section_name"), levels = new_lvls)) %>%
    arrange(!!sym("section_name")) %>%
    pull(!!sym("section_color"))

  dd <- dd %>%
    group_by(!!sym("section_name"), !!sym("commodity_name")) %>%
    summarise(
      trade_value = sum(!!sym("trade_value"), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(d2 %>% rename(color = !!sym("section_color")), by = "section_name") %>%
    ungroup()

  d3po(dd) %>%
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$section_name,
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
            return 'Displaying Commodities';
          } else {
            return 'Displaying Sections';
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
          var section = (row && (row.group || row.section_name || row.name)) ? (row.group || row.section_name || row.name) : '';
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
        var section = (row && (row.group || row.section_name || row.name)) ? (row.group || row.section_name || row.name) : '';
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
    )) %>%
    po_background("transparent")
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
