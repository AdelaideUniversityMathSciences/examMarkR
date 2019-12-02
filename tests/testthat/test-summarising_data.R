library(tidyverse)
library(examMarking)
library(testthat)


test_that("Proportion works",{
  df  <- tibble(
    A = 1:10
  )
  df  <- 
    df %>% 
    get_prop_df("A", total_row = 10)
  expect_equal(df$A, 1:10/10)
})

test_that("Totals work", {
  df  <- tibble(
    A1 = 1:10, 
    A2 = 10:1
  )
  total  <- df %>% get_total("A")
  expect_equal(total, rep(11, 10))
})

test_that("Mean work", {
  df  <- tibble(
    A1 = 1:10, 
    A2 = 10:1
  )
  total  <- df %>% get_mean("A")
  expect_equal(total, rep(5.5, 10))
  df$A1[1]  <- NA
  total  <- df %>% get_mean("A")
  expect_equal(total, c(10, rep(5.5, 9)))
})
  