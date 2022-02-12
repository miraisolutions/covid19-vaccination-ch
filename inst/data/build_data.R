# ? needed
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# 
# 
# library(covid19vaccinationch)
# library(jsonlite)
# library(RCurl)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(ggplot2)
# library(plotly)
# library(tidyselect)
# library(scales)
# library(htmlTable)
# library(lubridate)

print("BUILD DATA")
bag_sources = jsonlite::fromJSON(bag_api_url)

DATA = read_bag_data_vac(bag_api_url, ageclassMap)
# aggregation to new age classes
# DATA.AGG = DATA %>%
#   group_by(Week,AgeClass, vaccination_status ) %>%
#   summarise_if(is.numeric, sum, na.rm = TRUE)
# weeks4 = tail(unique(DATA.AGG$Week), 4)
# period = paste(range(weeks4), collapse = "-")

# DATA.AG4W <- DATA.AGG %>% filter(Week %in% weeks4)

# read Cases, they are not available anymore
CASES = read_bag_data_cases(bag_api_url, ageclassMap, dateweek = unique(DATA$Week))

# CASES.AGG = CASES %>%
#   group_by(Week,AgeClass ) %>%
#   summarise_if(is.numeric, sum, na.rm = TRUE)

# CASES.AG4W <- CASES.AGG %>% filter(Week %in% weeks4)

message("SAVING RDS DATA")
saveRDS(DATA, "inst/data/DATA.rds")
saveRDS(CASES, "inst/data/CASES.rds")
