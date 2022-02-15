
# we use a font size relative to the responsive body font size (em)
table_css <- "border-collapse: separate; border-spacing: 0.25em; width: 100%; font-size: 0.7em; margin-bottom: 14.5px;"
table_cell_css <- "white-space: nowrap; font-size: .95em;" # relative to the table_css

#' Vac status vector with labels as names
#'
#' @noRd
vac_levels <- function(){
  levs <- c("unknown", #"fully_vaccinated",
                  "fully_vaccinated_first_booster", "fully_vaccinated_no_booster", "partially_vaccinated", "not_vaccinated")
  names(levs) <- c("Unknown",  #"Fully vac.", 
                         "Fully vac. Booster", "Fully vac. No Booster", "Partially vac.", "Unvac.")
  levs
}


# VaxNoBoCol = "#17a2ff"
# #VaxBoCol = "darkblue"
# VaxBoCol = "royalblue3"
# VaxCol = c(VaxNoBoCol, VaxBoCol)
# NoVaxCol = "#dd4b39"
# ParVaxCol = "burlywood2"
# UknVaxCol = "white"

#' Color for Current Status
ProjCol = "tomato4"

#' Vac status color vector
#'
#' @noRd
vac_levels_colors <- function(){
  VaxNoBoCol = "#17a2ff"
  #VaxBoCol = "darkblue"
  VaxBoCol = "royalblue3"
  #VaxCol = c(VaxNoBoCol, VaxBoCol)
  NoVaxCol = "#dd4b39"
  ParVaxCol = "burlywood2"
  UknVaxCol = "white"
  levcols <- c(UknVaxCol, VaxBoCol, VaxNoBoCol, ParVaxCol, NoVaxCol)
  names(levcols) <- names(vac_levels())
  levcols
}
#' pick Label given vac status
#' 
#' @param x character vaccination status from `vac_levels()`
#' @noRd
pick_vac_lev <- function(x) {
  if (!all(x %in% vac_levels()))
    stop("wrong argument x")
  levs <- vac_levels()[match(x, vac_levels())]
  as.character(names(levs))
}

#' pick color given vac status
#' 
#' @param x character vaccination status from `vac_levels()`
#' @noRd
pick_vac_lev_col <- function(x) {
  if (!all(x %in% vac_levels()))
    stop("wrong argument x")
  levs <- vac_levels()[match(x, vac_levels())]
  as.character(vac_levels_colors()[names(levs)])
}



#' Extract week name from date, Weeks in format W-[0-9]
#'
#' @param dat date vector
#'
#' @noRd
.makeweek <- function(dat) {
  paste(substring(dat, 3, 4), "W", substring(dat, 5, 6), sep = "-")
}

#'Converts weeks in form W-[0-9] into real dates in form yy/mm/dd
#'
#' @param w date vector
#' @param range logical if TRUE then only 1st and last values are returned
#'
#' @noRd
weeks_to_date <- function(w, range = TRUE){
  weeksN = sapply(strsplit(w, "-"), function(x) as.numeric(x[3]))
  yearN = sapply(strsplit(w, "-"), function(x) as.numeric(x[1]))
  # W-1
  yearD = rep(as.Date("2021/01/04"), length(yearN)) 
  # W-1 22
  yearD[yearN == 22] = as.Date("2022/01/01")
  # to be updated
  if (range)
    res <- yearD[c(1,length(weeksN))] + 7 * weeksN[c(1,length(weeksN))] - c(7,0) -1
  else
    res <- yearD + 7 * weeksN -1
  res
}   
# week_to_date <- function(w){
#   # if (length(w)>1)
#   #   stop("week_to_date supports only one week")
#   weeksN = sapply(strsplit(w, "-"), function(x) as.numeric(x[3]))
#   as.Date("2021/01/04") + 7 * weeksN -1
# }



#' Compute Totals over a given time period
#'
#' @param data data.frame data
#' @param pd character period label to mutate in Week column
#' @param by character vector of column names to group_by
#' @param aggv character vector of columns that can be aggregated (daily record)
#' @param cumv character vector of columns that are cumulative (daily record) and should not be aggregated
#' 
#' @import dplyr
aggregate_to_month <- function(data, pd, by = c("AgeClass","vaccination_status"), 
                               aggv, cumv) {
  data %>%
    group_by(across(all_of(by))) %>%
    summarise_at(aggv, sum, na.rm = TRUE) %>%
    left_join(data %>%
                group_by(across(all_of(by))) %>%
                summarise_at(cumv, mean, na.rm = TRUE), by = c(by)) %>%
    ungroup() %>%
    mutate(Week = pd)
}


# .bayes_prob <- function(data, aVar = "confirmed_vax", bVar = "confirmed",
#                         aVarTot = "vaccinated_tot", bVarTot = "pop") {
#   # P(xvar / conditional) = P(conditional / xvar) * P(xvar) / P (conditional)
#   PrA <- data[[aVarTot]] / data[[bVarTot]] # P(Vaccinated)
#   PrBA <- data[[aVar ]] / data[[aVarTot]] # P(Case / Vaccinated)
#   #BayesTheorem(PrA, PrBA) # does not work with vector
#   PrB <- data[[bVar]] / data[[bVarTot]]
#   # prpb of confirmed cases given no vax
#   PrB1mA <- (data[[bVar ]] - data[[aVar ]]) / (data[[bVarTot]] - data[[aVarTot]]) # P(Case / Vaccinated)
#   
#   PrAB <- PrBA * PrA / (PrA * PrBA + (1 - PrA) * PrB1mA)
#   PrAstartB <- 1 - PrAB
#   #PrAB = (PrBA * PrA) / sum(PrBA * PrA)
#   data[[paste("prob", aVar, "given", bVar, sep = "-")]] <- PrAB
#   data[[paste("prob-not", aVar, "given", bVar, sep = "-")]] <- PrAstartB
#   data
# }

#' Allocates proportionally values of Unknown vaccination status to the other Vaccination Categories
#'
#' @param data data.frame data
#' @param by character vector of column names to group_by
#' 
#' @import dplyr
rescale_unknown <- function(data, by = c("AsOfDate","AgeClass", "Case")) {
  # Sum all classes but Unknown 
  dataTotNoUkn = data  %>%
    filter(Status != "Unknown") %>%
    #group_by(.dots = by) %>%
    group_by(across(all_of(by))) %>%
    summarize(Total = sum(value)) %>%
    ungroup()
  # Unknown Values
  dataTotUkn = data  %>%
    filter(Status == "Unknown") %>%
    select(-Status) %>%
    rename(TotUnkn = value)
  
  dataScale = data %>%
    filter(!Case %in% c("Population") & (Status != "Unknown")) %>%
    left_join(dataTotNoUkn, by = by) %>%
    left_join(dataTotUkn, by = by) %>%
    mutate(Ratio = value/Total ) %>%
    mutate(Ratio = ifelse(is.nan(Ratio), NA, Ratio)) %>%
    # add to value the Ratio of the Unknown CAT
    mutate(value = value + TotUnkn * Ratio) %>%
    mutate(value = replace_na(value, 0))
  
  # recompute All
  dataScaleAll = dataScale %>% 
    filter(AgeClass != "All") %>% arrange(Case) %>% 
    group_by(.dots = c(setdiff(by, "AgeClass"), "Status")) %>% 
    #group_by(across(all_of(setdiff(by, "AgeClass"), "Status"))) %>%  
    #this should work, to be testes
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(AgeClass = "All")
  
  dataScale <- bind_rows(dataScale %>% filter(AgeClass != "All"), 
                         dataScaleAll ) %>%
    select(-Total,- TotUnkn,- Ratio)
  
  dataScale <- bind_rows(dataScale, 
                         data %>% filter(Case == "Population" & Status != "Unknown")) 
  
  dataScale
}

#' Re-scales values to 100'000 people
#'
#' @param data data.frame data
#' @param by character vector of column names to group_by
#' @param status character vector of Vaccination Status column name
#' 
#' @import dplyr
make_100k <- function(data, by = c("AsOfDate", "AgeClass", "vaccination_status"), status) {
  Pop <- data %>%
    filter(Case == "Population") %>%
    rename(pop = Value) %>%
    select(-Case)
  Pop <- left_join(Pop, Pop %>% group_by(AgeClass) %>%
                     summarize(totpopage = sum(pop)),
                   by = "AgeClass")
  # compute age adjusted measure
  data100k <- data %>%
    left_join(Pop, by = by) %>%
    filter(!(!!sym(status) %in% c("Unknown")) & (!Case %in% c("Population","Infections"))) %>%
    #filter(Case != c("Population","Infections")) %>%
    mutate(Value100k = Value/pop * 100000) %>%
    mutate(Value100k = ifelse(!is.finite(Value100k), NA, Value100k)) %>%
    mutate(ValueAdj = Value100k / 100000 * totpopage)
    
  # calculate age adjusted for all
  data100k$Value100k[data100k$AgeClass == "All"] = 
    data100k %>% 
      filter(AgeClass != "All") %>% 
      group_by(across(c("Case",by[by!="AgeClass"]))) %>% 
      summarize(ValueAdj = sum(ValueAdj), .groups = "drop")  %>% 
    left_join(Pop %>% filter(AgeClass == "All"), by = c("AsOfDate","Status"))  %>%
      mutate(Value100knew = ValueAdj / totpopage * 100000) %>%
      mutate(Value100knew = ifelse(!is.finite(Value100knew), NA, Value100knew)) %>%
        as.data.frame() %>% .[, "Value100knew"]
    
  data100k <- data100k %>% select(-Value,-totpopage ,-ValueAdj) %>% 
    rename(Value = Value100k)
  
  data100k
}

#' Calculates ratio of hosp and death between Unvaccinated and Vaccinated 
#'
#' @param data data.frame data
#' @param ageclassmap data.frame Age class mapping, used for factor levels
#' @param refstatus character vector of reference vaccination status for the computation
#' 
#' @import dplyr
calc_ratio_fullyvac <- function(data, ageclassmap = ageclassMap, refstatus = "not_vaccinated") {
  refstatus_label = names(vac_levels())[vac_levels() == refstatus]
  data %>% left_join(data %>%
                       filter(Status == refstatus_label) %>%
                       #select(-Status,-pop) %>%
                       select(-Status, - pop) %>%
                       rename(RefValue = Value),
                     #by = c("AsOfDate", "AgeClass","Case")) %>%
                     by = c( "AgeClass","Case", "AsOfDate")) %>%
    filter(Case != "Population") %>% # new
    mutate(Value = round(RefValue / Value,1), Variable = "Ratio") %>%
    mutate(Value = ifelse(is.infinite(Value), NA, Value)) %>%
    mutate(Value = ifelse(is.nan(Value), NA, Value)) %>%
    bind_rows(data %>% filter(Case != "Population") %>%
                mutate(Variable = "Value")) %>%
    mutate(Value = ifelse(Variable == "Ratio" & Status == refstatus_label, NA, Value)) %>%
    mutate(Value = round(Value,1),
           Variable = factor(Variable, levels = c("Value","Ratio"), labels = c("Over 100k", paste("Ratio over", refstatus_label))),
           Case = factor(Case, levels = setdiff(levels(Case), "Population"), labels = setdiff(levels(Case), "Population")),
           AgeClass = factor(AgeClass, levels = c(unique(ageclassmap$AgeClass), "All"))) %>%
    #select(-pop, AsOfDate) %>%
    #select(AsOfDate) %>%
    rename(value = Value)  %>%
    select(-RefValue) %>%
    arrange(AsOfDate, Case,Variable , Status) #%>%
  #mutate(AgeClass = as.character(AgeClass))
}

