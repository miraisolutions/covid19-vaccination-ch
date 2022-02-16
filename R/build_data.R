#' It renders the Article Rmd report
#' 
#' @param url character bag_api_url
#' @param agemap data.frame ageclassMap Age mapping
#' @param writerds logical write RDS files
#' 
#' @importFrom jsonlite fromJSON 
build_data <- function(url = bag_api_url, agemap = ageclassMap, writerds = TRUE) {
  message("BUILD DATA")
  bag_sources = jsonlite::fromJSON(bag_api_url)
  
  DATA = read_bag_data_vac(bag_api_url, ageclassMap)
  
  # read Cases, they are not available anymore
  CASES = read_bag_data_cases(bag_api_url, ageclassMap, dateweek = unique(DATA$Week))
  
  message("SAVING RDS DATA")
  
  if (writerds) {
    saveRDS(DATA, "inst/bag_data/DATA.rds")
    saveRDS(CASES, "inst/bag_data/CASES.rds") 
  }
  invisible()
}



