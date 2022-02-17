#' Path of the Rmd file containing the article
#' 
#' @noRd
report_path <- function() {
  system.file("report", package = "covid19vaccinationch")  
  #"."
}
#' Data path
#' 
#' @noRd
data_path <- function() {
  system.file("bag_data", package = "covid19vaccinationch")  
  #"inst/bag_data"
  
}


#' It renders the Article Rmd report
#'
#' @param rmdname character name of the input Rmd report (Index.Rmd)
#' @param out.html character name of the output html report (Index.html)
#' @param quiet logical suppress printing
#' @param run_pandoc logical run_pandoc argument of render
#'
#' @importFrom rmarkdown render
#' @export
run_report <- function(rmdname = "Index.Rmd", out.html = "Index.html", quiet = TRUE, run_pandoc = TRUE) {
  if (!grepl(".Rmd$", rmdname))
    stop("rmdname must be a .Rmd file")
  rmarkdown::render(file.path(report_path(), rmdname), output_file = out.html,
                    quiet = quiet, run_pandoc = run_pandoc)
  browseURL(out.html)
}