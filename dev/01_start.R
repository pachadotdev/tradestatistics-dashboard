golem::fill_desc(
  pkg_name = "tradestatisticsshiny",
  pkg_title = "Open Trade Statistics Dashboard",
  pkg_description = "This is meant as a production grade Shiny app and not as end user app.",
  authors = person("Mauricio", "Vargas", , "mavargas11@uc.cl", role = c("aut", "cre", "cph"),
    comment = c(ORCID = "0000-0003-1017-7574")),
  repo_url = "https://github.com/tradestatistics/visualization-with-shiny"
)

golem::set_golem_options()

usethis::use_apache_license()
usethis::use_readme_rmd(open = FALSE)

usethis::use_code_of_conduct(contact = "Mauricio Vargas")
usethis::use_lifecycle_badge("stable")
usethis::use_news_md(open = FALSE)

usethis::use_git()

golem::use_recommended_tests()

golem::use_favicon("https://shiny.tradestatistics.io/images/favicon.ico")
golem::remove_favicon()

golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
