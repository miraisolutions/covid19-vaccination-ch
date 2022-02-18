test_that("Running the Rmd report works", {
  # adapted from golem::expect_running()
  r_ <- if (tolower(.Platform$OS.type) == "windows") {
     normalizePath(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"))
  } else {
    normalizePath(file.path(Sys.getenv("R_HOME"), "bin", "R"))
  }
  run_expression <- sprintf(
    ".libPaths(%s); covid19vaccinationch::run_report()",
    paste(deparse(.libPaths()), collapse = "\n")
  )
  run_process <- processx::process$new(
    echo_cmd = TRUE,
    command = r_,
    c("-e", run_expression),
    stdout = "|", stderr = "|"
  )
  Sys.sleep(5)
  expect_true(run_process$is_alive())
  run_process$kill()
})
