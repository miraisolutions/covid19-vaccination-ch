#' Path of the Rmd file containing the article
#' 
#' @noRd
report_path <- function() {
  system.file("report", package = "covid19vaccinationch")  
}
#' Data path
#' 
#' @noRd
data_path <- function() {
  system.file("bag_data", package = "covid19vaccinationch")  
}


#' It renders the Article Rmd report
#' 
#' @param rmdname character name of the report
#' @param quiet logical suppress printing
#' @param run_pandoc logical run_pandoc argument of render
#' 
#' @importFrom rmarkdown render 
#' @export
run_report <- function(rmdname = "Index.Rmd", quiet = TRUE, run_pandoc = TRUE) {
  rmarkdown::render(file.path(report_path(), rmdname), 
                    quiet = quiet, run_pandoc = run_pandoc)
}