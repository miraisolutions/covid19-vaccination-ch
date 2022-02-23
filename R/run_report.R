#' Path of the Rmd file containing the article
#'
#' @noRd
report_path <- function() {
  system.file("report", package = "covid19vaccinationch")
}
#' Data path
#'
#' @export
data_path <- function() {
  system.file("bag_data", package = "covid19vaccinationch")
}

#' Run the Rmd report
#'
#' @param rds logical, if FALSE the data are read from source, if TRUE from RDS files in `bag_data` folder. Default = TRUE, FALSE to be used in local run.
#' @param ... Additional arguments to [rmarkdown::run()], different from `render_args`.
#' @export
run_report <- function(rds = TRUE, ...) {
  rmarkdown::run(file.path(report_path(), "index.Rmd"), render_args = list(params = list(rds = rds)), ...)
}
