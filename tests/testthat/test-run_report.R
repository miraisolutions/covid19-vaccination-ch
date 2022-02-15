test_that("Rendering Rmd Article works", {
  expect_error(run_report(quiet = TRUE, run_pandoc = FALSE), regexp = NA)
})
