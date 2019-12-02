library(examMarking)
library(testthat)
library(tidyverse)

test_that("McCann spike works", {
  x  <- 1:100
  y  <- 1:100
  y[49] <- 50
  y[64] <- 65
  y[84] <- 85
  y[74] <- 75
  expect_equal(y, add_mccann_spike(x))
  expect_equal(add_mccann_spike(NA), NA)
})