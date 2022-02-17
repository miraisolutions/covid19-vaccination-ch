#' URL BAG website where data are available
bag_api_url <- "https://www.covid19.admin.ch/api/data/context/"


#' Mapping Age Classes data.frame
#'
#' @noRd
ageclassMap <- data.frame(
  # Exclude unknown
  ageclass = c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+" ),
  AgeClass = c("0-19",  "0-19",    "20-39",   "20-39",   "40-59",   "40-59",   "60-79",   "60-79",   "80+" ),
  AgeClass2 = c("0-39", "0-39",    "0-39",    "0-39",    "40-69",   "40-69",   "40-69",   "70+",  "70+"),
  # AgeClass = c("0 - 29", "0 - 29", "0 - 29", "30 - 49", "30 - 49", "50 - 69", "50 - 69", "60 - 79", "80+" ),
  stringsAsFactors = FALSE
)

#' Read Vaccination data from BAG source
#'
#' @param bag.admin.url character vector, BAG data url
#' @param ageclassMap data.frame with mapping age
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
read_bag_data_vac <- function(bag.admin.url, ageclassMap) {
  # define variables:
  BAGSOURCES <- jsonlite::fromJSON(bag.admin.url)
  sourceDate <- BAGSOURCES$sourceDate
  
  # confirmed Vaccinated are not updated anymore
  
  #TODO: exclusion of 2020 done by hand
  Hosp_vaccpersons <- BAGSOURCES$sources$individual$json$weekly$byAge$hospVaccPersons
  HOSP.VAC.J <- jsonlite::fromJSON(Hosp_vaccpersons) %>%
    filter(!grepl("2020",date)) %>%
    mutate(Week = .makeweek(date))
  
  Death_vaccpersons <- BAGSOURCES$sources$individual$json$weekly$byAge$deathVaccPersons
  DEATH.VAC.J <- jsonlite::fromJSON(Death_vaccpersons) %>%
    filter(!grepl("2020",date)) %>%
    mutate(Week = .makeweek(date))
  
  dateweek <- 
    intersect(HOSP.VAC.J$Week, DEATH.VAC.J$Week)
  
  message("dateweek: ", paste(dateweek, collapse = ", "))
  
  .clean_cases_vac <- function(data, dateweek, region = "CHFL", var = "confirmed") {
    # data in ascending order
    
    var_tot <- paste(var, "tot", sep = "_")
    var_vax <- paste(var, sep = "_")
    
    data %>% filter(Week %in% dateweek & geoRegion == region & vaccination_status != "fully_vaccinated") %>%
      rename(ageclass = altersklasse_covid19, {{var_vax}} := entries, {{var_tot}} := sumTotal) %>%
      mutate(AgeClass = ageclassMap$AgeClass[match(ageclass, ageclassMap$ageclass)],
             AgeClass2 = ageclassMap$AgeClass2[match(ageclass, ageclassMap$ageclass)],
             AsOfDate = substring(version, 1, 10)) %>%
      mutate(AgeClass = ifelse(ageclass == "all", "All", AgeClass), 
             AgeClass2 = ifelse(ageclass == "all", "All", AgeClass2)) %>%
      mutate(AgeClass = ifelse(is.na(AgeClass), "unknown", AgeClass),
             AgeClass2 = ifelse(is.na(AgeClass2), "unknown", AgeClass2)) %>%
      mutate(vaccination_status = factor(
        vaccination_status,
        levels = c(vac_levels()),
        labels = c(names(vac_levels()))
        
      )) %>%
      select(Week, AsOfDate, geoRegion, ageclass, AgeClass, AgeClass2, vaccination_status, pop, !!var_vax, !!var_tot)
  }
  
  HOSP.VAC <- .clean_cases_vac(HOSP.VAC.J, dateweek, var = "hosp")
  DEATH.VAC <- .clean_cases_vac(DEATH.VAC.J, dateweek, var = "deaths")
  
  # realign the as of dates
  
  AsOfDate = min(HOSP.VAC$AsOfDate, DEATH.VAC$AsOfDate)
  DEATH.VAC$AsOfDate <- HOSP.VAC$AsOfDate <- AsOfDate
  
  RES <- 
    merge(HOSP.VAC, DEATH.VAC %>% select(-pop),
          by = c("AsOfDate", "Week", "geoRegion", "ageclass", "AgeClass", "AgeClass2", "vaccination_status"), sort = FALSE)
  
  RES
}


#' Read Infection cases data from BAG source
#'
#' @param bag.admin.url character vector, BAG data url
#' @param ageclassMap data.frame with mapping age
#' @param dateweek dates in Weeks format to be used to filter the data
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
read_bag_data_cases <- function(bag.admin.url, ageclassMap, dateweek = NULL) {
  # define variables:
  BAGSOURCES <- jsonlite::fromJSON(bag.admin.url)
  sourceDate <- BAGSOURCES$sourceDate
  
  Cases = BAGSOURCES$sources$individual$json$weekly$byAge$cases
  CASES.J <- jsonlite::fromJSON(Cases) %>%
    filter(!grepl("2020",datum)) %>%
    mutate(Week = .makeweek(datum))
  
  .clean_cases = function(data, dateweek, region = "CHFL", var = "confirmed") {
    # data in ascending order
    var_tot = paste(var, "tot", sep = "_")
    
    if (!is.null(dateweek))
      data <- data %>% filter(Week %in% dateweek)
    
    res <- data %>% filter(geoRegion == region ) %>%
      rename(ageclass = altersklasse_covid19, {{var}} := entries, {{var_tot}} := sumTotal
             # {{growth_fact}} := entries_diff_pct, {{diff}} := entries_diff_abs)
      ) %>%
      mutate(AgeClass = ageclassMap$AgeClass[match(ageclass, ageclassMap$ageclass)],
             AgeClass2 = ageclassMap$AgeClass2[match(ageclass, ageclassMap$ageclass)],
             AsOfDate = substring(version, 1, 10)) %>%
      mutate(AgeClass = ifelse(is.na(AgeClass),"unknown",AgeClass),
             AgeClass2 = ifelse(is.na(AgeClass2),"unknown",AgeClass2)) %>%
      select(Week, AsOfDate, geoRegion, ageclass, AgeClass, AgeClass2, !!var, !!var_tot, pop) #, !!growth_fact, !!diff)
    
    res_age_tot <-  res %>%
      group_by(Week,AsOfDate,geoRegion) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(ageclass = "All", AgeClass = "All", AgeClass2 = "All")
    
    res <- bind_rows(res, res_age_tot) %>%
      arrange(desc(Week),desc(ageclass))
    res
  }
  
  CASES = .clean_cases(CASES.J, dateweek)
  
  
  CASES
}
