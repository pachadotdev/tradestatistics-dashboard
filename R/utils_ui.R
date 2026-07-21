available_yrs <- function() {
  1988L:2022L
}

available_yrs_deflator <- function() {
  available_yrs()
}

available_yrs_min <- function() {
  min(available_yrs())
}

available_yrs_max <- function() {
  max(available_yrs())
}
