library(data.table)
library(RPostgres)
library(glue)

readRenviron("/tradestatistics/credentials.txt")

con <- dbConnect(
  drv = Postgres(),
  dbname = "tradestatistics",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

# countries ----

countries <- setDT(dbGetQuery(con, "select country, dynamic_code from dgd_countries"))

regions <- setDT(dbGetQuery(con, "select * from dgd_regions"))
regions2 <- setDT(dbGetQuery(con, "select iso3_dynamic, region_id from dgd_colours"))
regions <- merge(regions, regions2)
setnames(regions, "iso3_dynamic", "dynamic_code")

countries <- merge(countries, regions)
setorder(countries, region, country)

countries[, region := gsub("_", " ", region)]
countries[, region := tools::toTitleCase(region)]

countries_table <- countries

regions <- unique(countries_table$region)

countries <- list()

for (i in seq_along(regions)) {
  # i = 1
  chunk <- countries_table[region == regions[[i]]]
  codes <- chunk$dynamic_code
  names(codes) <- chunk$country
  countries[[i]] <- codes
}

names(countries) <- regions

usethis::use_data(countries, overwrite = T)

# sectors ----

sectors <- dbGetQuery(con, "select * from itpd_sectors")

sectors_out <- sectors$broad_sector_id

sectors_names_out <- sectors$broad_sector

names(sectors_out) <- paste(sectors_out, sectors_names_out, sep = " - ")

sectors <- sectors_out

usethis::use_data(sectors, overwrite = T)

# industries ----

industries <- dbGetQuery(con, "select * from itpd_industries")

industries_out <- industries$industry_id

industries_names_out <- industries$industry_descr

names(industries_out) <- paste(industries_out, industries_names_out, sep = " - ")

industries <- industries_out

usethis::use_data(industries, overwrite = T)

dbDisconnect(con)
