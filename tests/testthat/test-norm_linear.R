pacman::p_load(tidyverse, testthat)
test_that("norm linear works", {
  expect_error(norm_linear_inter(x, boundary_pts = tibble(x = 1, z = 2)))
  expect_error(norm_linear_inter(x, boundary_pts = tibble(y = 1, z = 2)))
})

test_that("positive slope detection works", {
  boundary_pts  <- tribble(
    ~x, ~y, 
    45, 50, 
    60, 45
  )
  
  expect_error(norm_linear_inter(0:100, boundary_pts = boundary_pts))
})