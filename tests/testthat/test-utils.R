test_that("rescale_unknown works", {
  data <- readRDS("data/DataTab.rds")

  data_scale <- rescale_unknown(data)
  expect_true(nrow(data_scale)>0, label = "rescale_unknown does not return rows")
  expect_true(sum(is.na(data_scale$value)) == 0, label = "rescale_unknown returns NAs")
  expect_equal(names(data_scale), c("AsOfDate","AgeClass","Status", "Case","value"),
               label = "Column names of the result of rescale_unknown are incorrect")
  expect_true(sum(data_scale$value) == 17425798, label = "rescale_unknown value sum must be 17425798")

})

test_that("make_100k works", {
  data <- readRDS("data/DataTabScale.rds")

  data_100k <- make_100k(data %>% rename(Value = value),
            by = c("AsOfDate","AgeClass", "Status"), status = "Status")
  expect_true(nrow(data_100k)>0, label = "make_100k does not return rows")
  expect_true(sum(is.na(data_100k$Value)) == 0, label = "make_100k returns NAs")
  expect_equal(names(data_100k), c("AsOfDate","AgeClass","Status", "Case","pop","Value"),
               label = "Column names of the result of make_100k are incorrect")
  expect_true(round(sum(data_100k$Value)) == 4273, label = "make_100k value sum must be 4273")

})

test_that("aggregate_to_month works", {
  data <- readRDS(file.path(data_path(), "CASES.rds"))

  data <- data %>%
    group_by(Week,AgeClass ) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)

  weeks4 <- tail(unique(data$Week), 4)
  period <- paste(range(weeks4), collapse = "-")

  data <- data %>% filter(Week %in% weeks4)

  # Aggregate to last month figures
  data.AG1M <- aggregate_to_month(data, period, by = "AgeClass",
                                   aggv = "confirmed", cumv = c("pop", "confirmed_tot"))

  expect_true(nrow(data.AG1M)>0, label = "aggregate_to_month does not return rows")
  expect_true(sum(is.na(data.AG1M[, sapply(data.AG1M, is.numeric)])) == 0, label = "aggregate_to_month returns NAs")
  expect_equal(names(data.AG1M), c("AgeClass","confirmed","pop","confirmed_tot","Week"),
               label = "Column names of the result of aggregate_to_month are incorrect")
})

