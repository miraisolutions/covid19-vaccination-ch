test_that("make_100k works", {
  #data <- readRDS("tests/testthat/data/DataTabScale.rds")
  data <- readRDS("data/DataTabScale.rds")
  
  data_100k <- make_100k(data %>% rename(Value = value), 
            by = c("AsOfDate","AgeClass", "Status"), status = "Status")
  expect_true(nrow(data_100k)>0, label = "make_100k does not return rows")
  expect_true(sum(is.na(data_100k$Value)) == 0, label = "make_100k returns NAs")
  expect_equal(names(data_100k), c("AsOfDate","AgeClass","Status", "Case","pop","Value"), 
               label = "Column names of the result of make_100k are incorrect")
  
})
