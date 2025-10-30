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
    inp_d <- reactive({
      input$d
    }) # adjust dollar
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

      # evaluate reactives locally to avoid unquoting closures inside tbl() pipelines
      yrs <- inp_y()
      scode <- inp_s()

      d <- tbl(con, tbl_agg) %>%
        filter(!!sym("year") %in% yrs)

      # Apply section/commodity filter if specified (operate on local df)
      if (nchar(scode) == 4) {
        d <- d %>%
          filter(substr(!!sym("commodity_code"), 1, 4) == scode)
      } else if (nchar(scode) == 2) {
        d <- d %>%
          filter(!!sym("section_code") == scode)
      }

      # Use import data as measure of trade (more accurate from importer's perspective)
      d <- d %>%
        group_by(!!sym("year")) %>%
        summarise(
          trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
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
      # Evaluate reactives locally to avoid using closures in tbl operations
      yrs <- inp_y()
      scode <- inp_s()

      # Base data from yrpc table - use evaluated yrs variable
      d <- tbl(con, tbl_dtl) %>%
        filter(!!sym("year") %in% yrs)

      # Apply section/commodity filter if specified (operate on local df)
      if (nchar(scode) == 4) {
        d <- d %>%
          filter(substr(!!sym("commodity_code"), 1, 4) == scode)
      } else if (nchar(scode) == 2) {
        d <- d %>%
          filter(!!sym("section_code") == scode)
      }

      # Get commodities reference data and collect early to avoid lazy evaluation issues

      commodities_ref <- tbl(con, "commodities") %>%
        distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("commodity_code_short"))

      # For imports: use direct import data (more accurate)
      d_imp <- d %>%
        inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
        select(-!!sym("trade_value_usd_exp"))

      if (nchar(scode) == 4) {
        d_imp <- d_imp %>%
          group_by(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"), commodity_code = !!sym("commodity_code_short")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          )
      } else if (nchar(scode) == 2) {
        d_imp <- d_imp %>%
          group_by(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"), commodity_code = !!sym("section_code")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          )
      }

      # For exports: use imports but swap reporter/partner perspective
      # If country A reports importing product X from country B,
      # then country B exported product X to country A
      d_exp <- d %>%
        inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
        mutate(
          original_reporter = !!sym("reporter_iso"),
          reporter_iso = !!sym("partner_iso"), # Swap perspective: partner becomes reporter
          partner_iso = !!sym("original_reporter"), # Original reporter becomes partner
          trade_value_usd_exp = !!sym("trade_value_usd_imp") # Imports = exports from partners
        ) %>%
        select(-!!sym("trade_value_usd_imp"))

      if (nchar(scode) == 4) {
        d_exp <- d_exp %>%
          group_by(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"), commodity_code = !!sym("commodity_code_short")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          )
      } else if (nchar(scode) == 2) {
        d_exp <- d_exp %>%
          group_by(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"), commodity_code = !!sym("section_code")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          )
      }

      d <- d_imp %>%
        left_join(
          d_exp,
          by = c(
            "year", "reporter_iso", "partner_iso", "commodity_code"
          )
        ) %>%
        mutate(
          trade_value_usd_exp = ifelse(is.na(!!sym("trade_value_usd_exp")), 0, !!sym("trade_value_usd_exp")),
          trade_value_usd_imp = ifelse(is.na(!!sym("trade_value_usd_imp")), 0, !!sym("trade_value_usd_imp"))
        )

      if (nchar(scode) == 4L) {
        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(commodity_code = !!sym("commodity_code_short"), !!sym("section_color")),
            by = "commodity_code"
          )
      } else if (nchar(scode) == 2) {
        d <- d %>%
          inner_join(
            tbl(con, "commodities") %>%
              distinct(commodity_code = !!sym("section_code"), !!sym("section_color")),
            by = "commodity_code"
          )
      }

      d <- collect(d)

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        message(glue::glue("[mod_products_server] df_dtl after deflator: rows={nrow(d)}"))
      }

      return(d)
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d(), "yrpc") %>%
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      d <- df_agg() %>%
        select(!!sym("year"), !!sym("trade_value_usd_exp"), !!sym("trade_value_usd_imp")) %>%
        filter(!!sym("year") %in% c(min(inp_y()), max(inp_y())))
    })

    exp_val_min_yr <- eventReactive(input$go, {
      trade_values() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("trade_value_usd_exp"))
    })

    exp_val_max_yr <- eventReactive(input$go, {
      trade_values() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("trade_value_usd_exp"))
    })

    imp_val_min_yr <- eventReactive(input$go, {
      trade_values() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("trade_value_usd_imp"))
    })

    imp_val_max_yr <- eventReactive(input$go, {
      trade_values() %>%
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
      ) %>%
        mutate(
          year = as.character(!!sym("year")),
          color = ifelse(!!sym("flow") == "Exports", "#67c090", "#26667f")
        )

      # convert to billions for display
      d <- d %>%
        arrange(!!sym("year")) %>%
        mutate(trade_billion = .data$trade / 1e9)

      d3po(d) %>%
        po_bar(
          daes(
            x = .data$year,
            y = .data$trade_billion,
            group = .data$flow,
            color = .data$color,
            stack = FALSE
          )
        ) %>%
        po_labels(
          x = "Year",
          y = "Trade Value (USD billion)",
          title = trd_exc_columns_title()
        ) %>%
        po_format(
          y = format(.data$trade_billion, big.mark = " ", scientific = FALSE, digits = 2)
        ) %>%
        po_tooltip(JS(
          "function(value, row) {
            if (!row) return '';
            var grp = (row.flow != null) ? row.flow : (row.group != null ? row.group : '');
            var val = (value != null && !isNaN(value)) ? Number(value) : (row.trade_billion != null && !isNaN(row.trade_billion) ? Number(row.trade_billion) : 0);
            var groupPrefix = grp ? (grp + ': ') : '';
            return groupPrefix + (val || 0) + ' billion';
          }"
        )) %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
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
      # Evaluate year locally to avoid reactive closure issues
      min_year <- min(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name")) %>%
        collect()

      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min_year) %>%
        inner_join(countries_data, by = c("partner_iso" = "country_iso")) %>%
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

      # Convert values to billions for display and number top 4 + Rest of the world as 5
      d <- d %>%
        arrange(desc(!!sym("trade_value_usd_imp"))) %>%
        mutate(rank = row_number())

      d <- d %>% mutate(color = "#85cca6")

      # Ensure Rest of the world is always present and set to position 5
      d <- d %>%
        filter(!!sym("country_name") == "Rest of the world") %>%
        mutate(n = 5L) %>%
        bind_rows(
          d %>%
            filter(!!sym("country_name") != "Rest of the world") %>%
            arrange(desc(!!sym("trade_value_usd_imp"))) %>%
            mutate(n = row_number())
        ) %>%
        mutate(
          country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
          trade_value_usd_imp = round(!!sym("trade_value_usd_imp") / 1e9, 2)
        ) %>%
        select(-!!sym("n"))

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(
          title = exp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) %>%
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) %>%
        po_tooltip("{country_name}: {trade_value_usd_imp} billion") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name")) %>%
        collect()

      d <- df_dtl() %>%
        filter(!!sym("year") == max_year) %>%
        inner_join(countries_data, by = c("partner_iso" = "country_iso")) %>%
        group_by(!!sym("country_name")) %>%
        summarise(
          trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
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

      d <- d %>%
        arrange(desc(!!sym("trade_value_usd_imp"))) %>%
        mutate(rank = row_number())

      d <- d %>% mutate(color = "#67c090")

      # Ensure Rest of the world is always present and set to position 5
      d <- d %>%
        filter(!!sym("country_name") == "Rest of the world") %>%
        mutate(n = 5L) %>%
        bind_rows(
          d %>%
            filter(!!sym("country_name") != "Rest of the world") %>%
            arrange(desc(!!sym("trade_value_usd_imp"))) %>%
            mutate(n = row_number())
        ) %>%
        mutate(
          country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
          trade_value_usd_imp = round(!!sym("trade_value_usd_imp") / 1e9, 2)
        ) %>%
        select(-!!sym("n"))

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(
          title = exp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) %>%
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) %>%
        po_tooltip("{country_name}: {trade_value_usd_imp} billion") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Exporters in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      # Evaluate year locally to avoid reactive closure issues
      min_year <- min(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
        collect()

      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min_year) %>%
        group_by(!!sym("partner_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(countries_data, by = c("partner_iso" = "country_iso"))

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_treemap(d, d2, title = exp_tt_min_yr())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Top Exporters in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      # Evaluate year locally to avoid reactive closure issues
      max_year <- max(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
        collect()

      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and partner_iso to identify exporters (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == max_year) %>%
        group_by(!!sym("partner_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(countries_data, by = c("partner_iso" = "country_iso"))

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_treemap(d, d2, title = exp_tt_max_yr())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
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
      # Evaluate year locally to avoid reactive closure issues
      min_year <- min(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name")) %>%
        collect()

      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min_year) %>%
        inner_join(countries_data, by = c("reporter_iso" = "country_iso")) %>%
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

      d <- d %>%
        arrange(desc(!!sym("trade_value_usd_imp"))) %>%
        mutate(rank = row_number())

      d <- d %>% mutate(color = "#518498")

      # Ensure Rest of the world is always present and set to position 5
      d <- d %>%
        filter(!!sym("country_name") == "Rest of the world") %>%
        mutate(n = 5L) %>%
        bind_rows(
          d %>%
            filter(!!sym("country_name") != "Rest of the world") %>%
            arrange(desc(!!sym("trade_value_usd_imp"))) %>%
            mutate(n = row_number())
        ) %>%
        mutate(
          country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
          trade_value_usd_imp = round(!!sym("trade_value_usd_imp") / 1e9, 2)
        ) %>%
        select(-!!sym("n"))

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(
          title = imp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) %>%
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE, digits = 2)) %>%
        po_tooltip("{country_name}: {trade_value_usd_imp} billion") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      # Evaluate year locally to avoid reactive closure issues
      max_year <- max(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name")) %>%
        collect()

      d <- df_dtl() %>%
        filter(!!sym("year") == max_year) %>%
        inner_join(countries_data, by = c("reporter_iso" = "country_iso")) %>%
        group_by(!!sym("country_name")) %>%
        summarise(
          trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!!sym("trade_value_usd_imp") > 0) %>%
        mutate(country_name = fct_lump_n(
          f = !!sym("country_name"),
          n = 4,
          w = !!sym("trade_value_usd_imp"),
          other_level = "Rest of the world"
        )) %>%
        group_by(!!sym("country_name")) %>%
        summarise(
          trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          country_name = factor(!!sym("country_name"),
            levels = c(setdiff(unique(!!sym("country_name")), "Rest of the world"), "Rest of the world")
          )
        )

      d <- d %>%
        arrange(desc(!!sym("trade_value_usd_imp"))) %>%
        mutate(rank = row_number())

      d <- d %>% mutate(color = "#26667f")

      # Ensure Rest of the world is always present and set to position 5
      d <- d %>%
        filter(!!sym("country_name") == "Rest of the world") %>%
        mutate(n = 5L) %>%
        bind_rows(
          d %>%
            filter(!!sym("country_name") != "Rest of the world") %>%
            arrange(desc(!!sym("trade_value_usd_imp"))) %>%
            mutate(n = row_number())
        ) %>%
        mutate(
          country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
          trade_value_billion = round(!!sym("trade_value_usd_imp") / 1e9, 2)
        ) %>%
        select(-!!sym("n"))

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_billion,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(
          title = imp_col_max_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD billion)"
        ) %>%
        po_format(x = format(.data$trade_value_billion, big.mark = " ", scientific = FALSE, digits = 2)) %>%
        po_tooltip("{country_name}: {trade_value_billion} billion") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Top Importers in { min(inp_y()) }")
    })

    imp_tm_dtl_min_yr <- reactive({
      # Evaluate year locally to avoid reactive closure issues
      min_year <- min(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
        collect()

      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == min_year) %>%
        group_by(!!sym("reporter_iso")) %>%
        summarise(trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), .groups = "drop") %>%
        # Join with countries table to get country names and continent info
        inner_join(countries_data, by = c("reporter_iso" = "country_iso"))

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      od_treemap(d, d2, title = imp_tt_min_yr())
    }) %>%
      bindCache(inp_y(), inp_s(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Top Importers in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      # Evaluate year locally to avoid reactive closure issues
      max_year <- max(inp_y())

      # Get countries reference data
      countries_data <- tbl(con, "countries") %>%
        select(!!sym("country_iso"), !!sym("country_name"), !!sym("continent_name"), !!sym("continent_color")) %>%
        collect()

      # Aggregate by countries instead of products for country treemap
      # Use import flow (trade_value_usd_imp) and reporter_iso to identify importers (more reliable)
      d <- df_dtl() %>%
        filter(!!sym("year") == max_year) %>%
        group_by(!!sym("reporter_iso")) %>%
        summarise(
          trade_value = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        # Join with countries table to get country names and continent info
        inner_join(countries_data, by = c("reporter_iso" = "country_iso"))

      # Create continent colors dataset
      d2 <- d %>%
        select(!!sym("continent_name"), country_color = !!sym("continent_color")) %>%
        distinct() %>%
        arrange(!!sym("continent_name"))

      out <- od_treemap(d, d2, title = imp_tt_max_yr())

      session$sendCustomMessage("hideProgress", list())

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
