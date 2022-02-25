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
#' @param fetch_latest_data logical, if `TRUE` the data re-fetched from the BAG
#'   source, otherwise data are read from the stored RDS files in
#'   `inst/bag_data`. Defaults to `TRUE`, `FALSE` can be used in local runs.
#' @param render_args list, additional arguments to pass to
#'   [rmarkdown::render()]
#' @param shiny_args list, additional arguments to pass to [shiny::runApp()]
#' @param auto_reload logical, additional argument to pass to
#'   [rmarkdown::run()], If `TRUE` (the default), automatically reload the Shiny
#'   application when the file currently being viewed is changed on disk.
#' @export
run_report <- function(fetch_latest_data = FALSE, render_args = NULL, auto_reload = TRUE,
                       shiny_args = NULL) {
  rmd_params <- list(fetch_latest_data = fetch_latest_data, use_pkgload = FALSE)
  render_args$params <- c(render_args$params, rmd_params)
  rmarkdown::run(file.path(report_path(), "index.Rmd"),
                 render_args = render_args, auto_reload = auto_reload,
                 shiny_args = shiny_args)
}

