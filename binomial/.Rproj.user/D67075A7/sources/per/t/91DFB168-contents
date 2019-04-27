source("../../R/binomial.R")
context("Test for auxiliary functions")

test_that("aux_mean work as expected", {
  t1 <- 4.5
  p1 <- 0.4
  expect_error(aux_mean(t1, p1))
  t2 <- 4
  p2 <- 1.1
  expect_error(aux_mean(t2, p2))
  t3 <- 10
  p3 <- 0.3
  expect_equal(aux_mean(t3, p3), 3)
  expect_length(aux_mean(t3, p3), 1)
})

test_that("aux_variance work as expected", {
  t1 <- 4.6
  p1 <- 0.1
  expect_error(aux_variance(t1, p1))
  t2 <- 4
  p2 <- 1.3
  expect_error(aux_variance(t2, p2))
  t3 <- 10
  p3 <- 0.3
  expect_equal(aux_variance(t3, p3), 2.1)
  expect_length(aux_variance(t3, p3), 1)
})

test_that("aux_mode work as expected", {
  t1 <- 4.6
  p1 <- 0.1
  expect_error(aux_mode(t1, p1))
  t2 <- 4
  p2 <- 1.3
  expect_error(aux_mode(t2, p2))
  t3 <- 10
  p3 <- 0.3
  expect_equal(aux_mode(t3, p3), 3)
  expect_length(aux_mode(t3, p3), 1)
  t4 <- 9
  p4 <- 0.7
  expect_equal(aux_mode(t4, p4), c(7,6))
  expect_length(aux_mode(t4, p4), 2)
})

test_that("aux_skewness work as expected", {
  t1 <- 4.6
  p1 <- 0.1
  expect_error(aux_skewness(t1, p1))
  t2 <- 4
  p2 <- 1.3
  expect_error(aux_skewness(t2, p2))
  t3 <- 10
  p3 <- 0.3
  expect_lt(abs(aux_skewness(t3, p3) - 0.2760262), 0.00000005)
  expect_length(aux_skewness(t3, p3), 1)
})

test_that("aux_kurtosis work as expected", {
  t1 <- 4.6
  p1 <- 0.1
  expect_error(aux_kurtosis(t1, p1))
  t2 <- 4
  p2 <- 1.3
  expect_error(aux_kurtosis(t2, p2))
  t3 <- 10
  p3 <- 0.3
  expect_lt(abs(aux_kurtosis(t3, p3) - (-0.1238095)), 0.00000005)
  expect_length(aux_kurtosis(t3, p3), 1)
})
