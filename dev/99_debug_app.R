devtools::load_all()

con = tradestatisticsshiny::sql_con()

tbl_dtl = function() "yrc"
inp_r = function() "can"
inp_p = function() "usa"
inp_y = function() c(2010,2015)
inp_d = function() 2000

df_dtl = function() {
  d <- tbl(sql_con, tbl_dtl())

  if (inp_p() == "all") {
    d <- d %>%
      filter(
        !!sym("year") %in% !!inp_y() &
          !!sym("reporter_iso") == !!inp_r()
      )
  } else {
    d <- d %>%
      filter(
        !!sym("year") %in% !!inp_y() &
          !!sym("reporter_iso") == !!inp_r() &
          !!sym("partner_iso") == !!inp_p()
      )
  }

  d <- d %>% collect()

  if (inp_d() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_d()), con)
  }

  return(d)
}

exp_tm_dtl_min_yr <- function() {
  d <- df_dtl() %>%
    filter(!!sym("year") == min(inp_y()))

  d <- p_fix_section_and_aggregate(d, col = "trade_value_usd_exp", sql_con = con)

  d2 <- p_colors(d, sql_con = con)

  p_to_highcharts(d, d2)
}

exp_tm_dtl_min_yr()
