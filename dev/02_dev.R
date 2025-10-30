attachment::att_amend_desc()

golem::add_module(name = "countries", with_test = TRUE)
golem::add_module(name = "products", with_test = TRUE)

golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)

golem::add_css_file("custom")

usethis::use_test("app")

usethis::use_vignette("model")
devtools::build_vignettes()

usethis::use_coverage()
