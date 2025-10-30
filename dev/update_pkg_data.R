library(dplyr)
library(DBI)
library(RPostgres)
library(glue)

readRenviron("/tradestatistics/visualization-with-shiny")

con <- dbConnect(
  drv = Postgres(),
  dbname = "tradestatistics",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

# reporters ----

reporters_display <- tbl(con, "countries") %>%
  select(!!sym("country_iso"), !!sym("country_name")) %>%
  collect()

reporters_out <- reporters_display %>%
  select(!!sym("country_iso")) %>%
  pull()

reporters_names_out <- reporters_display %>%
  select(!!sym("country_name")) %>%
  pull()

names(reporters_out) <- reporters_names_out

reporters_display <- reporters_out

# sections ----

sections_display <- tbl(con, "commodities") %>%
  collect()

sections_out <- sections_display %>%
  distinct(!!sym("section_code")) %>%
  pull()

sections_names_out <- sections_display %>%
  distinct(!!sym("section_name")) %>%
  pull()

names(sections_out) <- glue("{ sections_out } - { sections_names_out }")

sections_display <- sections_out

# commodities short ----

commodities_short_display <- tbl(con, "commodities_short") %>%
  select(commodity_code, commodity_name) %>%
  collect()

commodities_short_out <- commodities_short_display %>%
  select(!!sym("commodity_code")) %>%
  pull()

commodities_short_names_out <- commodities_short_display %>%
  select(!!sym("commodity_name")) %>%
  pull()

names(commodities_short_out) <- glue("{ commodities_short_out } - { commodities_short_names_out }")

commodities_short_display <- commodities_short_out

# commodities ----

commodities <- tbl(con, "commodities") %>%
  select(section_code, commodity_code) %>%
  collect()

commodities_display <- tbl(con, "commodities") %>%
  select(commodity_code, commodity_name) %>%
  collect()

commodities_out <- commodities_display %>%
  select(!!sym("commodity_code")) %>%
  pull()

commodities_names_out <- commodities_display %>%
  select(!!sym("commodity_name")) %>%
  pull()

names(commodities_out) <- glue("{ commodities_out } - { commodities_names_out }")

commodities_display <- commodities_out

# output ----

usethis::use_data(reporters_display, overwrite = T)

usethis::use_data(sections_display, overwrite = T)

usethis::use_data(commodities, overwrite = T)

usethis::use_data(commodities_short_display, overwrite = T)

usethis::use_data(commodities_display, overwrite = T)

DBI::dbDisconnect(con)
