test_that("reading vaccination data works", {
  bag_sources = fromJSON(bag_api_url)
  sourceDate <- bag_sources$sourceDate

  Hosp_vaccpersons <- bag_sources$sources$individual$json$weekly$byAge$hospVaccPersons
  expect_false(is.null(Hosp_vaccpersons), label = "sources$individual$json$weekly$byAge$hospVaccPersons is NULL")

  HOSP.VAC.J <- fromJSON(Hosp_vaccpersons) %>%
    filter(!grepl("2020",date)) %>%
    mutate(Week = .makeweek(date))

  expect_equal(sort(unique(HOSP.VAC.J$vaccination_status)[unique(HOSP.VAC.J$vaccination_status) != "fully_vaccinated"]),
               as.character(sort(vac_levels())),
               label = "Vaccination Levels do notcorrespond to expected in HOSP.VAC.J")
  # expect_equal(sort(unique(HOSP.VAC.J$altersklasse_covid19)[!unique(HOSP.VAC.J$altersklasse_covid19) %in% c("Unbekannt", "all")]),
  #              as.character(sort(ageclassMap$ageclass)),
  #              label = "altersklasse_covid19 do not correspond to expected of ageclassMap in HOSP.VAC.J")
  #
  expect_true(all(as.character(sort(ageclassMap$ageclass)) %in% unique(HOSP.VAC.J$altersklasse_covid19)),
              label = "altersklasse_covid19 do not correspond to expected of ageclassMap in HOSP.VAC.J")

  Death_vaccpersons <- bag_sources$sources$individual$json$weekly$byAge$deathVaccPersons
  expect_false(is.null(Death_vaccpersons), label = "sources$individual$json$weekly$byAge$deathVaccPersons is NULL")

  DEATH.VAC.J <- fromJSON(Death_vaccpersons) %>%
    filter(!grepl("2020",date)) %>%
    mutate(Week = .makeweek(date))

  expect_equal(sort(unique(DEATH.VAC.J$vaccination_status)[unique(DEATH.VAC.J$vaccination_status) != "fully_vaccinated"]),
               as.character(sort(vac_levels())),
               label = "Vaccination Levels do notcorrespond to expected in HOSP.VAC.J")
  # expect_equal(sort(unique(DEATH.VAC.J$altersklasse_covid19)[!unique(DEATH.VAC.J$altersklasse_covid19) %in% c("Unbekannt", "all")]),
  #              as.character(sort(ageclassMap$ageclass)),
  #              label = "altersklasse_covid19 do not correspond to expected of ageclassMap in HOSP.VAC.J")

  expect_true(all(as.character(sort(ageclassMap$ageclass)) %in% unique(DEATH.VAC.J$altersklasse_covid19)),
              label = "altersklasse_covid19 do not correspond to expected of ageclassMap in DEATH.VAC.J")


  DATA = read_bag_data_vac(bag_api_url, ageclassMap)

  expect_true(nrow(DATA)>0,
              label = "read_bag_data_vac dows not return rows")
  expect_equal(levels(DATA$vaccination_status), names(vac_levels()),
               label = "Vaccination Levels do not correspond to expected")
  expect_equal(names(DATA),
               c("AsOfDate","Week","geoRegion","ageclass","AgeClass","AgeClass2","vaccination_status","pop","hosp","hosp_tot",
"deaths","deaths_tot"),
               label = "Expected columns are not returned")
  expect_true(sum(is.na(DATA[, c("hosp", "deaths", "hosp_tot", "deaths_tot")])) == 0,
               label = "NAs present in 'hosp' or 'deaths' variables")

})
test_that("reading case data works", {
  bag_sources = fromJSON(bag_api_url)
  CASES = read_bag_data_cases(bag_api_url, ageclassMap)

  expect_true(nrow(CASES)>0,
              label = "read_bag_data_cases dows not return rows")

  expect_equal(names(CASES),
               c("Week","AsOfDate","geoRegion","ageclass","AgeClass","AgeClass2","confirmed","confirmed_tot","pop"),
               label = "Expected columns are not returned")
  expect_true(sum(is.na(CASES[, c("confirmed" , "confirmed_tot")])) == 0,
              label = "NAs present in 'confirmed' variables")

  expect_equal(sort(unique(CASES$ageclass )[!unique(CASES$ageclass ) %in% c("Unbekannt", "All")]),
               as.character(sort(ageclassMap$ageclass)),
               label = "altersklasse_covid19 do not correspond to expected of ageclassMap in read_bag_data_cases")
  expect_true(all(as.character(sort(ageclassMap$ageclass)) %in% unique(CASES$ageclass)),
              label = "altersklasse_covid19 do not correspond to expected of ageclassMap in CASES")


})
