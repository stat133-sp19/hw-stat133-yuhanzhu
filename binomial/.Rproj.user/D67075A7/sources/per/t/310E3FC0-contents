source("../../R/binomial.R")
context("Test for check functions")

test_that("check_prob function works as expected",{
  x <- 0.6
  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  y <- c(1.1)
  expect_error(check_prob(y))
})

test_that("check_prob works as expected",{
  x <- 0.6
  expect_equal(check_prob(x), TRUE)
  expect_length(check_prob(x), 1)
  y <- c(1.1)
  expect_error(check_prob(y))
})

test_that("check_success works as expected",{
  s1 <- c(1.1, 3)
  t1 <- 3
  expect_error(check_success(s1, t1))
  s2 <- c(1,3,4,5)
  expect_error(check_success(s2, t1))
  s3 <- c(2,4)
  expect_error(check_success(s3, t1))
  s4 <- c(1,3,4)
  t4 <- 5
  expect_equal(check_success(s4, t4), TRUE)
  expect_length(check_success(s4, t4), 1)
})

test_that("check_trials work as expected",{
  x <- 5
  expect_equal(check_trials(x), TRUE)
  expect_length(check_trials(x), 1)
  y <- 4.1
  expect_error(check_trials(y))
})

