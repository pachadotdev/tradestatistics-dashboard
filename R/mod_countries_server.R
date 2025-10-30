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

    inp_r <- reactive({
      input$r
    }) # reporter
    inp_p <- reactive({
      input$p
    }) # partner
    inp_d <- reactive({
      input$d
    }) # adjust dollar
    inp_fmt <- reactive({
      input$fmt
    }) # format

    tbl_agg <- "yrp"
    tbl_dtl <- "yrpc"

    # Human-readable reporter/partner names for glue templates. Fallback to
    # the code when no display name is available.
    rname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::reporters_display[tradestatisticsshiny::reporters_display == inp_r()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_r())
      }
      out
    })

    pname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::reporters_display[tradestatisticsshiny::reporters_display == inp_p()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_p())
      }
      out
    })

    title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Multilateral trade { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    # Visualize ----

    ## Data ----

    df_agg <- reactive({
      session$sendCustomMessage("showProgress", list(text = "Loading data..."))

      d_base <- tbl(con, tbl_agg)

      if (inp_p() == "ALL") {
        # For imports: use direct data (reporter's own records are more accurate)
        d_imp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r()
          ) %>%
          group_by(!!sym("year"), !!sym("reporter_iso")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          )

        # For exports: use partners' import data (more accurate than direct export records)
        # Get imports from all countries that report trading WITH our selected reporter
        d_exp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("partner_iso") == !!inp_r() # Countries that import FROM our reporter
          ) %>%
          group_by(!!sym("year"), !!sym("partner_iso")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE), # Use their imports as our exports
            .groups = "drop"
          ) %>%
          rename(!!sym("reporter_iso") := !!sym("partner_iso")) # Rename to match the import data structure

        # Combine the two datasets
        d <- d_imp %>%
          left_join(d_exp, by = c("year", "reporter_iso")) %>%
          mutate(
            !!sym("trade_value_usd_exp") := ifelse(is.na(!!sym("trade_value_usd_exp")), 0, !!sym("trade_value_usd_exp")),
            !!sym("trade_value_usd_imp") := ifelse(is.na(!!sym("trade_value_usd_imp")), 0, !!sym("trade_value_usd_imp"))
          )
      } else {
        # For bilateral trade, we still need to apply the same logic
        # Imports: direct data from reporter
        d_imp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r() &
              !!sym("partner_iso") == !!inp_p()
          ) %>%
          select(!!sym("year"), !!sym("reporter_iso"), !!sym("partner_iso"), !!sym("trade_value_usd_imp"))

        # Exports: partner's import data (swap reporter and partner)
        d_exp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_p() & # Partner as reporter
              !!sym("partner_iso") == !!inp_r() # Our country as partner
          ) %>%
          select(!!sym("year"),
            reporter_iso = !!sym("partner_iso"), # Swap back to our perspective
            partner_iso = !!sym("reporter_iso"),
            trade_value_usd_exp = !!sym("trade_value_usd_imp")
          ) # Their imports = our exports

        # Combine bilateral data
        d <- d_imp %>%
          left_join(d_exp, by = c("year", "reporter_iso", "partner_iso")) %>%
          mutate(
            !!sym("trade_value_usd_exp") := ifelse(is.na(!!sym("trade_value_usd_exp")), 0, !!sym("trade_value_usd_exp")),
            !!sym("trade_value_usd_imp") := ifelse(is.na(!!sym("trade_value_usd_imp")), 0, !!sym("trade_value_usd_imp"))
          )
      }

      d <- d %>% collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      return(d)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    df_dtl <- reactive({
      d_base <- tbl(con, tbl_dtl)

      # Get commodities reference data
      commodities_ref <- tbl(con, "commodities") %>%
        distinct(!!sym("commodity_code"), !!sym("section_code"), !!sym("section_name"), !!sym("section_color"), !!sym("commodity_code_short"), !!sym("commodity_name"))

      if (inp_p() == "ALL") {
        # For imports: use direct data (reporter's own records are more accurate)
        d_imp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r()
          ) %>%
          inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
          select(-!!sym("trade_value_usd_exp")) # Remove exports, we'll get them from partners

        # For exports: use partners' import data (more accurate than direct export records)
        # Get imports from all countries that report trading WITH our selected reporter
        d_exp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("partner_iso") == !!inp_r() # Countries that import FROM our reporter
          ) %>%
          inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
          select(-!!sym("trade_value_usd_exp")) %>% # Remove exports, we don't need them here
          mutate(
            !!sym("original_reporter") := !!sym("reporter_iso"), # Store original reporter
            !!sym("reporter_iso") := !!sym("partner_iso"), # Swap perspective
            !!sym("partner_iso") := !!sym("original_reporter"), # Complete the swap
            !!sym("trade_value_usd_exp") := !!sym("trade_value_usd_imp") # Their imports = our exports
          ) %>%
          select(-!!sym("original_reporter"), -!!sym("trade_value_usd_imp")) # Remove temporary column and original imports

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
          )
      } else {
        # For bilateral trade, apply the same logic but for specific partner
        # Imports: direct data from reporter
        d_imp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_r() &
              !!sym("partner_iso") == !!inp_p()
          ) %>%
          inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
          select(-!!sym("trade_value_usd_exp")) # Remove exports, we'll get them from partner

        # Exports: partner's import data (swap reporter and partner)
        d_exp <- d_base %>%
          filter(
            !!sym("year") %in% !!inp_y() &
              !!sym("reporter_iso") == !!inp_p() & # Partner as reporter
              !!sym("partner_iso") == !!inp_r() # Our country as partner
          ) %>%
          inner_join(commodities_ref, by = c("commodity_code", "section_code")) %>%
          select(-!!sym("trade_value_usd_exp")) %>% # Remove exports, we don't need them
          mutate(
            !!sym("original_reporter") := !!sym("reporter_iso"), # Store original reporter
            !!sym("reporter_iso") := !!sym("partner_iso"), # Swap back to our perspective
            !!sym("partner_iso") := !!sym("original_reporter"), # Complete the swap
            !!sym("trade_value_usd_exp") := !!sym("trade_value_usd_imp") # Their imports = our exports
          ) %>%
          select(-!!sym("original_reporter"), -!!sym("trade_value_usd_imp")) # Remove temporary column and original imports

        # Combine bilateral data
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
          )
      }

      d <- collect(d)

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      return(d)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      df_agg() %>%
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

      # Always get ALL partners for consistent ranking calculation
      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") %in% min_max_y &
            !!sym("reporter_iso") == !!inp_r()
        ) %>%
        collect()

      if (inp_d() != "No") {
        d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
      }

      d <- d %>%
        # Keep all partners including unspecified for accurate share calculation
        mutate(
          trd_value_usd_bal = !!sym("trade_value_usd_exp") + !!sym("trade_value_usd_imp")
        ) %>%
        group_by(!!sym("year")) %>%
        mutate(
          bal_rank = dense_rank(desc(!!sym("trd_value_usd_bal"))),
          exp_share = !!sym("trade_value_usd_exp") / sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
          imp_share = !!sym("trade_value_usd_imp") / sum(!!sym("trade_value_usd_imp"), na.rm = TRUE)
        ) %>%
        ungroup()

      return(d)
    })

    # Helper function to get ranking with tie information
    get_ranking_with_ties <- function(year_val) {
      if (inp_p() == "ALL") {
        return("N/A") # No ranking for multilateral trade
      }

      rankings_data <- trd_rankings() %>%
        filter(!!sym("year") == year_val, !!sym("reporter_iso") == !!inp_r())

      partner_rank <- rankings_data %>%
        filter(!!sym("partner_iso") == !!inp_p()) %>%
        pull(!!sym("bal_rank"))

      if (length(partner_rank) == 0 || is.na(partner_rank)) {
        return("N/A")
      }

      # Check for ties
      tied_partners <- rankings_data %>%
        filter(!!sym("bal_rank") == partner_rank, !!sym("partner_iso") != !!inp_p()) %>%
        nrow()

      if (tied_partners > 0) {
        return(paste0(
          partner_rank, " (tied with ", tied_partners, " other",
          ifelse(tied_partners == 1, "", "s"), ")"
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
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == min(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("exp_share")) %>%
        as.numeric()

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
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == max(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("exp_share")) %>%
        as.numeric()

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
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == min(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("imp_share")) %>%
        as.numeric()

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
      result <- trd_rankings() %>%
        ungroup() %>%
        filter(
          !!sym("year") == max(!!inp_y()),
          !!sym("reporter_iso") == !!inp_r(),
          !!sym("partner_iso") == !!inp_p()
        ) %>%
        select(!!sym("imp_share")) %>%
        as.numeric()

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

    # Get GDP data for the reporter country
    gdp_data <- eventReactive(input$go, {
      reporter <- inp_r()
      years <- inp_y()

      tbl(con, "gdp") %>%
        filter(
          !!sym("country_iso") == !!reporter &
            !!sym("year") %in% !!years
        ) %>%
        collect()
    })

    # Calculate trade as percentage of GDP for exports
    exp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      exp_val <- exp_val_min_yr()
      if (is.na(exp_val) || exp_val <= 0) {
        return(NA)
      }

      return(round((exp_val / gdp_val) * 100, 2))
    })

    exp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      exp_val <- exp_val_max_yr()
      if (is.na(exp_val) || exp_val <= 0) {
        return(NA)
      }

      return(round((exp_val / gdp_val) * 100, 2))
    })

    # Calculate trade as percentage of GDP for imports
    imp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      imp_val <- imp_val_min_yr()
      if (is.na(imp_val) || imp_val <= 0) {
        return(NA)
      }

      return(round((imp_val / gdp_val) * 100, 2))
    })

    imp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      imp_val <- imp_val_max_yr()
      if (is.na(imp_val) || imp_val <= 0) {
        return(NA)
      }

      return(round((imp_val / gdp_val) * 100, 2))
    })

    # Get total exports for bilateral context (when partner != "ALL")
    total_exp_val_min_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(exp_val_min_yr())
      }

      # Get total exports for the year (all partners)
      min_year <- min(inp_y())
      reporter <- inp_r()

      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!reporter
        ) %>%
        summarise(total_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE)) %>%
        collect()

      total_val <- d$total_exp
      if (inp_d() != "No" && !is.na(total_val)) {
        # Apply GDP deflator adjustment if needed
        temp_df <- tibble(
          year = min_year,
          trade_value_usd_exp = total_val,
          trade_value_usd_imp = 0
        )
        adjusted_df <- gdp_deflator_adjustment(temp_df, as.integer(inp_d()), con = con)
        total_val <- adjusted_df$trade_value_usd_exp
      }

      return(total_val)
    })

    total_exp_val_max_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(exp_val_max_yr())
      }

      # Get total exports for the year (all partners)
      max_year <- max(inp_y())
      reporter <- inp_r()

      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!reporter
        ) %>%
        summarise(total_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE)) %>%
        collect()

      total_val <- d$total_exp
      if (inp_d() != "No" && !is.na(total_val)) {
        # Apply GDP deflator adjustment if needed
        temp_df <- tibble(
          year = max_year,
          trade_value_usd_exp = total_val,
          trade_value_usd_imp = 0
        )
        adjusted_df <- gdp_deflator_adjustment(temp_df, as.integer(inp_d()), con = con)
        total_val <- adjusted_df$trade_value_usd_exp
      }

      return(total_val)
    })

    # Calculate total exports as percentage of GDP for bilateral context
    total_exp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_exp_val <- total_exp_val_min_yr()
      if (is.na(total_exp_val) || total_exp_val <= 0) {
        return(NA)
      }

      return(round((total_exp_val / gdp_val) * 100, 2))
    })

    total_exp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_exp_val <- total_exp_val_max_yr()
      if (is.na(total_exp_val) || total_exp_val <= 0) {
        return(NA)
      }

      return(round((total_exp_val / gdp_val) * 100, 2))
    })

    # Get total imports for bilateral context (when partner != "ALL")
    total_imp_val_min_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(imp_val_min_yr())
      }

      # Get total imports for the year (all partners)
      min_year <- min(inp_y())
      reporter <- inp_r()

      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") == !!min_year &
            !!sym("reporter_iso") == !!reporter
        ) %>%
        summarise(total_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE)) %>%
        collect()

      total_val <- d$total_imp
      if (inp_d() != "No" && !is.na(total_val)) {
        # Apply GDP deflator adjustment if needed
        temp_df <- tibble(
          year = min_year,
          trade_value_usd_exp = 0,
          trade_value_usd_imp = total_val
        )
        adjusted_df <- gdp_deflator_adjustment(temp_df, as.integer(inp_d()), con = con)
        total_val <- adjusted_df$trade_value_usd_imp
      }

      return(total_val)
    })

    total_imp_val_max_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(imp_val_max_yr())
      }

      # Get total imports for the year (all partners)
      max_year <- max(inp_y())
      reporter <- inp_r()

      d <- tbl(con, "yrp") %>%
        filter(
          !!sym("year") == !!max_year &
            !!sym("reporter_iso") == !!reporter
        ) %>%
        summarise(total_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE)) %>%
        collect()

      total_val <- d$total_imp
      if (inp_d() != "No" && !is.na(total_val)) {
        # Apply GDP deflator adjustment if needed
        temp_df <- tibble(
          year = max_year,
          trade_value_usd_exp = 0,
          trade_value_usd_imp = total_val
        )
        adjusted_df <- gdp_deflator_adjustment(temp_df, as.integer(inp_d()), con = con)
        total_val <- adjusted_df$trade_value_usd_imp
      }

      return(total_val)
    })

    # Calculate total imports as percentage of GDP for bilateral context
    total_imp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_imp_val <- total_imp_val_min_yr()
      if (is.na(total_imp_val) || total_imp_val <= 0) {
        return(NA)
      }

      return(round((total_imp_val / gdp_val) * 100, 2))
    })

    total_imp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        pull(!!sym("gdp"))

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_imp_val <- total_imp_val_max_yr()
      if (is.na(total_imp_val) || total_imp_val <= 0) {
        return(NA)
      }

      return(round((total_imp_val / gdp_val) * 100, 2))
    })

    ### Text/Visual elements ----

    trd_smr_txt_exp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s exports to the world { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s exports to { r_add_the(pname()) } { pname() } { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_exp_share_min_yr_2() } of exports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_exp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      min_pct <- exp_gdp_pct_min_yr()
      max_pct <- exp_gdp_pct_max_yr()

      if (!is.na(min_pct) && !is.na(max_pct) && min_pct > 0 && max_pct > 0) {
        if (inp_p() == "ALL") {
          # Multilateral: concise GDP context
          gdp_context <- glue(" These exports were { min_pct }% of { r_add_the(rname()) } { rname() }'s GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) }.")
        } else {
          # Bilateral: show bilateral exports as % of GDP, and total if significantly different
          total_min_pct <- total_exp_gdp_pct_min_yr()
          total_max_pct <- total_exp_gdp_pct_max_yr()

          if (!is.na(total_min_pct) && !is.na(total_max_pct) && total_min_pct > 0 && total_max_pct > 0) {
            gdp_context <- glue(" This trade was { min_pct }% of { r_add_the(rname()) } { rname() }'s GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) } (total exports: { total_min_pct }% and { total_max_pct }%).")
          }
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_smr_txt_imp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s imports from the world { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s imports from { r_add_the(pname()) } { pname() } { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_imp_share_min_yr_2() } of imports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_imp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      min_pct <- imp_gdp_pct_min_yr()
      max_pct <- imp_gdp_pct_max_yr()

      if (!is.na(min_pct) && !is.na(max_pct) && min_pct > 0 && max_pct > 0) {
        if (inp_p() == "ALL") {
          # Multilateral: concise GDP context
          gdp_context <- glue(" These imports were { min_pct }% of GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) }.")
        } else {
          # Bilateral: show bilateral imports as % of GDP, and total if significantly different
          total_min_pct <- total_imp_gdp_pct_min_yr()
          total_max_pct <- total_imp_gdp_pct_max_yr()

          if (!is.na(total_min_pct) && !is.na(total_max_pct) && total_min_pct > 0 && total_max_pct > 0) {
            gdp_context <- glue(" This trade was { min_pct }% of GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) } (total imports: { total_min_pct }% and { total_max_pct }%).")
          }
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Imports and Exports { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    trd_exc_columns_agg <- reactive({
      d <- trade_values()

      d <- tibble(
        year = d$year,
        trade = round(d$trade_value_usd_exp / 1e9, 2),
        flow = "Exports"
      ) %>%
        bind_rows(
          tibble(
            year = d$year,
            trade = round(d$trade_value_usd_imp / 1e9, 2),
            flow = "Imports"
          )
        ) %>%
        mutate(
          year = as.character(!!sym("year")),
          color = ifelse(!!sym("flow") == "Exports", "#67c090", "#26667f")
        )

      d3po(d) %>%
        po_bar(
          daes(
            x = .data$year,
            y = .data$trade,
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
          y = format(.data$trade, big.mark = " ", scientific = FALSE)
        ) %>%
        # po_tooltip("{flow}: {trade} B") %>%
        po_tooltip(JS(
          "function(value, row) {
            if (!row) return '';
            var grp = (row.flow != null) ? row.flow : (row.group != null ? row.group : '');
            var val = (value != null && !isNaN(value)) ? Number(value) : (row.trade != null && !isNaN(row.trade) ? Number(row.trade) : 0);
            var groupPrefix = grp ? (grp + ': ') : '';
            return groupPrefix + (val || 0) + ' B' ;
          }"
        )) %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Exports of { r_add_the(rname()) } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Exports of { r_add_the(rname()) } { rname() } to { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    # Export column chart titles
    exp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Export Destinations in { min(inp_y()) }")
    })

    exp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Export Destinations in { max(inp_y()) }")
    })

    # Export column charts
    exp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(
            f = !!sym("country_name"),
            n = 4,
            w = !!sym("trade_value_usd_exp"),
            other_level = "Rest of the world"
          )) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(color = "#67c090")

        d <- d %>%
          filter(!!sym("country_name") == "Rest of the world") %>%
          mutate(n = 5L) %>%
          bind_rows(
            d %>%
              filter(!!sym("country_name") != "Rest of the world") %>%
              arrange(desc(!!sym("trade_value_usd_exp"))) %>%
              mutate(n = row_number())
          ) %>%
          mutate(
            country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
            trade_value_usd_exp = round(!!sym("trade_value_usd_exp") / 1e9, 2),
            color = "#67c090"
          ) %>%
          select(-!!sym("n"))
      } else {
        # Show top 4 destinations + selected partner + "Rest of the world" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          arrange(
            desc(!!sym("trade_value_usd_exp"))
          ) %>%
          mutate(
            n = row_number(),
            country_name = case_when(
              !!sym("n") <= 4L ~ !!sym("country_name"),
              !!sym("country_name") == !!pname() ~ !!sym("country_name"),
              TRUE ~ "Rest of the world"
            )
          ) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            color = case_when(
              !!sym("country_name") == !!pname() ~ "#d04e66",
              TRUE ~ "#67c090"
            )
          )

        d <- d %>%
          filter(!!sym("country_name") == "Rest of the world") %>%
          mutate(n = 5L) %>%
          bind_rows(
            d %>%
              filter(!!sym("country_name") != "Rest of the world") %>%
              arrange(desc(!!sym("trade_value_usd_exp"))) %>%
              mutate(n = row_number())
          ) %>%
          mutate(
            country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
            trade_value_usd_exp = round(!!sym("trade_value_usd_exp") / 1e9, 2)
          ) %>%
          select(-!!sym("n"))
      }

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_exp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(
          title = exp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD Billion)"
        ) %>%
        po_format(x = format(.data$trade_value_usd_exp, big.mark = " ", scientific = FALSE)) %>%
        po_tooltip("{country_name}: {trade_value_usd_exp} B") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          mutate(country_name = fct_lump_n(
            f = !!sym("country_name"),
            n = 4,
            w = !!sym("trade_value_usd_exp"),
            other_level = "Rest of the world"
          )) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(color = "#67c090")

        d <- d %>%
          filter(!!sym("country_name") == "Rest of the world") %>%
          mutate(n = 5L) %>%
          bind_rows(
            d %>%
              filter(!!sym("country_name") != "Rest of the world") %>%
              arrange(desc(!!sym("trade_value_usd_exp"))) %>%
              mutate(n = row_number())
          ) %>%
          mutate(
            country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
            trade_value_usd_exp = round(!!sym("trade_value_usd_exp") / 1e9, 2),
            color = "#67c090"
          ) %>%
          select(-!!sym("n"))
      } else {
        # Show top 4 destinations + selected partner + "Rest of the world" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_exp") > 0) %>%
          arrange(
            desc(!!sym("trade_value_usd_exp"))
          ) %>%
          mutate(
            n = row_number(),
            country_name = case_when(
              !!sym("n") <= 4L ~ !!sym("country_name"),
              !!sym("country_name") == !!pname() ~ !!sym("country_name"),
              TRUE ~ "Rest of the world"
            )
          ) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_exp = sum(!!sym("trade_value_usd_exp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            color = case_when(
              !!sym("country_name") == !!pname() ~ "#d04e66",
              TRUE ~ "#67c090"
            )
          )

        d <- d %>%
          filter(!!sym("country_name") == "Rest of the world") %>%
          mutate(n = 5L) %>%
          bind_rows(
            d %>%
              filter(!!sym("country_name") != "Rest of the world") %>%
              arrange(desc(!!sym("trade_value_usd_exp"))) %>%
              mutate(n = row_number())
          ) %>%
          mutate(
            country_name = paste(!!sym("n"), !!sym("country_name"), sep = " - "),
            trade_value_usd_exp = round(!!sym("trade_value_usd_exp") / 1e9, 2)
          ) %>%
          select(-!!sym("n"))
      }

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_exp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(title = exp_col_max_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") %>%
        po_format(x = format(.data$trade_value_usd_exp, big.mark = " ", scientific = FALSE)) %>%
        po_tooltip("{country_name}: {trade_value_usd_exp} B") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Export Composition in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_exp", con = con)

      d2 <- p_colors(d, con = con)

      p_treemap(d, d2, title = exp_tt_min_yr())
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Export Composition in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_exp", con = con)

      d2 <- p_colors(d, con = con)

      p_treemap(d, d2, title = exp_tt_max_yr())
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Imports of { r_add_the(rname()) } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Imports of { r_add_the(rname()) } { rname() } from { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Import Composition in { min(inp_y()) }")
    })

    # Import column chart titles
    imp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Import Origins in { min(inp_y()) }")
    })

    imp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Import Origins in { max(inp_y()) }")
    })

    # Import column charts
    imp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
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
          mutate(color = "#26667f")

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
            trade_value_usd_imp = round(!!sym("trade_value_usd_imp") / 1e9, 2),
            color = "#26667f"
          ) %>%
          select(-!!sym("n"))
      } else {
        # Show top 4 destinations + selected partner + "Rest of the world" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!min_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          arrange(
            desc(!!sym("trade_value_usd_imp"))
          ) %>%
          mutate(
            n = row_number(),
            country_name = case_when(
              !!sym("n") <= 4L ~ !!sym("country_name"),
              !!sym("country_name") == !!pname() ~ !!sym("country_name"),
              TRUE ~ "Rest of the world"
            )
          ) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            color = case_when(
              !!sym("country_name") == !!pname() ~ "#d04e66",
              TRUE ~ "#26667f"
            )
          )

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
      }

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(title = imp_col_min_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") %>%
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE)) %>%
        po_tooltip("{country_name}: {trade_value_usd_imp} B") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      d <- tbl(con, tbl_dtl)

      if (inp_p() == "ALL") {
        # Show top 4 partners + "Rest of the world" for multilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
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
          mutate(color = "#26667f")

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
            trade_value_usd_imp = round(!!sym("trade_value_usd_imp") / 1e9, 2),
            color = "#26667f"
          ) %>%
          select(-!!sym("n"))
      } else {
        # Show top 4 destinations + selected partner + "Rest of the world" for bilateral trade
        d <- d %>%
          filter(
            !!sym("year") == !!max_year &
              !!sym("reporter_iso") == !!inp_r()
          )

        d <- d %>% collect()

        d <- d %>%
          inner_join(
            tbl(con, "countries") %>%
              select(!!sym("country_iso"), !!sym("country_name")) %>%
              collect(),
            by = c("partner_iso" = "country_iso")
          )

        if (inp_d() != "No") {
          d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con = con)
        }

        d <- d %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(!!sym("trade_value_usd_imp") > 0) %>%
          arrange(
            desc(!!sym("trade_value_usd_imp"))
          ) %>%
          mutate(
            n = row_number(),
            country_name = case_when(
              !!sym("n") <= 4L ~ !!sym("country_name"),
              !!sym("country_name") == !!pname() ~ !!sym("country_name"),
              TRUE ~ "Rest of the world"
            )
          ) %>%
          group_by(!!sym("country_name")) %>%
          summarise(
            trade_value_usd_imp = sum(!!sym("trade_value_usd_imp"), na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            color = case_when(
              !!sym("country_name") == !!pname() ~ "#d04e66",
              TRUE ~ "#26667f"
            )
          )

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
      }

      d3po(d) %>%
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) %>%
        po_labels(title = imp_col_max_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") %>%
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE)) %>%
        po_tooltip("{country_name}: {trade_value_usd_imp} B") %>%
        po_background("transparent")
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_tm_dtl_min_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == min(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_imp", con = con)

      d2 <- p_colors(d, con = con)

      p_treemap(d, d2, title = imp_tt_min_yr())
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Import Composition in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      d <- df_dtl() %>%
        filter(!!sym("year") == max(inp_y())) %>%
        p_aggregate_by_section(col = "trade_value_usd_imp", con = con)

      d2 <- p_colors(d, con = con)

      out <- p_treemap(d, d2, title = imp_tt_max_yr())

      session$sendCustomMessage("hideProgress", list())
      return(out)
    }) %>%
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) %>%
      bindEvent(input$go)

    ## Dynamic / server side selectors ----

    observeEvent(input$r, {
      updateSelectizeInput(session, "p",
        choices = sort(tradestatisticsshiny::reporters_display[
          tradestatisticsshiny::reporters_display != input$r
        ]),
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
      if (inp_p() == "ALL") {
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

    # Export column chart outputs
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

    ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    # Import column chart outputs
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

    output$dwn_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_dtl(), filename)
      }
    )

    output$dwn_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
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
