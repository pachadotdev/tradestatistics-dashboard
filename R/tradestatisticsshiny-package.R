#' @keywords internal
#' @import tabler
#' @import d3po
#' @importFrom cachem cache_disk
#' @importFrom data.table `:=` .N .I .SD copy data.table fifelse frankv rbindlist setDT setnames setorder
#' @importFrom DBI dbConnect dbDisconnect dbIsValid dbGetQuery
#' @importFrom glue glue
#' @importFrom golem add_resource_path activate_js favicon bundle_resources with_golem_options
#' @importFrom htmlwidgets JS
#' @importFrom jsonlite toJSON
#' @importFrom rio export
#' @importFrom RPostgres Postgres
#' @importFrom stats setNames
"_PACKAGE"

utils::globalVariables(c(
  ".", ".data",
  "bal_rank", "broad_sector", "broad_sector_id",
  "color", "commodity_name", "continent_name", "country", "country_color", "country_name",
  "exp_pct", "exp_share", "exporter", "exporter_iso3_dynamic",
  "flow",
  "imp_pct", "imp_share", "importer", "industry_id",
  "n",
  "region_colour",
  "sector_color", "sum_trade_value",
  "trade", "trade_exp", "trade_imp", "trade_value", "trd_value_usd_bal",
  "year"
))

tablerOptions(
  cache = cache_disk(
    dir = "/tradestatistics/cache"
  )
)

#' countries
#'
#' Internal dataset for country codes.
#'
#' @docType data
#' @keywords datasets
#' @name countries
NULL

#' sectors
#'
#' Internal dataset for section/commodity codes.
#'
#' @docType data
#' @keywords datasets
#' @name sectors
NULL

#' industries
#'
#' Internal dataset for commodity codes (6,898 codes).
#'
#' @docType data
#' @keywords datasets
#' @name industries
NULL
