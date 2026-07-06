# The United Kingdom's exports to the world decreased from 3912.22B in 2012 to 3905.07B in 2017 (0.04% annual decrease). On average, total exports represented 165.04% of the United Kingdom's GDP over the period.

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

# GBR total trade
trade <- dbGetQuery(con, "select * from itpde_imp where importer_iso3_dynamic = 'GBR' and year in (2012,2013,2014,2015,2016,2017)")

gdp <- dbGetQuery(con, "select distinct year, iso3_dynamic_d, gdp_pwt_const_d from dgd where iso3_dynamic_d = 'GBR' and year in (2012,2013,2014,2015,2016,2017)")

tradegdp <- merge(trade, gdp)

# gdp in million
mean(tradegdp$trade / tradegdp$gdp_pwt_const_d)
