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

countries <- dbGetQuery(con, "select distinct country, dynamic_code from dgd_countries") 

countries_out <- countries$dynamic_code

countries_names_out <- countries$country

names(countries_out) <- countries_names_out

countries_out <- sort(countries_out)

countries <- countries_out

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
