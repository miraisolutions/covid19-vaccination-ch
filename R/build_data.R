#' It builds the data input of the Rmd article and places them in inst/bag_data folder is writerds = TRUE
#' 
#' @param url character bag_api_url
#' @param agemap data.frame ageclassMap Age mapping
#' @param writerds logical write RDS files
#' 
#' @importFrom jsonlite fromJSON 
#' @import dplyr
build_data <- function(url = bag_api_url, agemap = ageclassMap, writerds = TRUE) {
  message("BUILD DATA")
  bag_sources = jsonlite::fromJSON(bag_api_url)
  
  message("Read vaccination")
  
  DATA = read_bag_data_vac(bag_api_url, ageclassMap)
  
  message("Read infections")
  
  # read Cases, they are not available anymore
  CASES = read_bag_data_cases(bag_api_url, ageclassMap, dateweek = unique(DATA$Week))
  
  message("Roll Monthly data from start")
  
  # All data scaled for line plots
  #ageclasses2 <- c("0-39", "40-69", "70+")
  ageclassMap2 <- ageclassMap %>% #mutate(AgeClass2 = c(rep(ageclasses2[1],4), rep(ageclasses2[2], 3), rep(ageclasses2[3], 2))) %>%
    select(-ageclass) %>%
    rbind(c("All", "All")) %>% rbind(c("unknown","All"))
  
  DATA.AGG2 = DATA %>%
    group_by(Week,AgeClass2, vaccination_status ) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    ungroup() %>%
    rename(AgeClass = AgeClass2) %>%
    mutate(AgeClass = factor(AgeClass, levels = unique(ageclassMap2$AgeClass2), labels = unique(ageclassMap2$AgeClass2)))
  # 
  
  DataRoll4W100k <- NULL
  # starting week: "21-W-20"
  start_week <- "21-W-20"
  
  week_to_consider <- unique(DATA.AGG2$Week[DATA.AGG2$Week >= start_week])
  
  message("Weeks for lineplot: ", paste(week_to_consider, collapse = ","))
  
  aggrvars = names(DATA.AGG2) [sapply(DATA.AGG2, is.numeric)] %>%
    grep(pattern = "_tot$", value = TRUE, invert = TRUE)  %>%
    grep(pattern = "^pop", value = TRUE, invert = TRUE)
  
  cumvars = setdiff(names(DATA.AGG2)[sapply(DATA.AGG2, is.numeric)], aggrvars)
  
  
  for (week in week_to_consider) {
    last4weeks = rev(unique(DATA.AGG2$Week)[which(unique(DATA.AGG2$Week) == week) - 0:3])
    DataMonth = DATA.AGG2 %>% filter(Week %in% last4weeks)
    
    DataMonth <- aggregate_to_month(DataMonth, week, aggv = aggrvars, cumv = cumvars)
    
    DataMonth <- DataMonth %>%
      select(Week, AgeClass, vaccination_status, hosp, deaths, pop) %>%
      filter(!AgeClass %in% c("unknown")) %>%
      rename(AsOfDate = Week, population = pop) %>% bind_cols() %>%
      pivot_longer(cols = c("population","hosp", "deaths"),
                   names_to = "Case", values_to = "Value") %>%
      mutate(Case = factor(Case, levels = c("population","hosp","deaths"),  
                           labels = c("Population", "Hospitalizations", "Deaths"))) %>%
      rename(Status = vaccination_status, 
             value = Value)
    
    DataMonthScale <- rescale_unknown(DataMonth) %>%
      rename(Value = value)
    
    DataMonthScale100k <- make_100k(DataMonthScale, by = c("AsOfDate","AgeClass", "Status"), 
                                    status = "Status")
    
    DataRoll4W100k <- bind_rows(DataRoll4W100k, DataMonthScale100k)  #%>%
    #   filter(Status != "Partially vac.")
    
  }
  
  
  if (writerds) {
    message("SAVING RDS DATA")
    
    saveRDS(DATA, "inst/bag_data/DATA.rds")
    saveRDS(CASES, "inst/bag_data/CASES.rds") 
    saveRDS(DataRoll4W100k, "inst/bag_data/DataRoll4W100k.rds") 
    
  }
  invisible()
}



