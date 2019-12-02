pacman::p_load(testthat, examMarking)


test_that("norm compare arguments", {
  expect_error(
    norm_compare(1:10, 1:9)
  )
})
