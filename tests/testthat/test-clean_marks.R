library(examMarking)
library(testthat)
library(tidyverse)

df  <- tibble(
  A = c("EX", 2, NA)
)

df  <- 
  df %>% 
  clean_marks_df("A")

test_that("Cleaning works",
          expect_equal(df$A, c(NA, 2, 0))
          )