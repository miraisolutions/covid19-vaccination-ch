#' Path of the Rmd file containing the article
#'
#' @noRd
report_path <- function() {
  system.file("report", package = "covid19vaccinationch")
}
#' Data path
#'
#' @noRd
#' @export
data_path <- function() {
  system.file("bag_data", package = "covid19vaccinationch")
}

#' Run the Rmd report
#'
#' @param ... Additional arguments to [rmarkdown::run()].
#' @export
run_report <- function(...) {
  rmarkdown::run(file.path(report_path(), "index.Rmd"), ...)
}
