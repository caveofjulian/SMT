source("../io/InputService.R",chdir = TRUE)

library(testthat)

testthat::test_that("mean", {
  expect_equal(round(getMean("Nep02"), digits = 6), 3.539017)
})

