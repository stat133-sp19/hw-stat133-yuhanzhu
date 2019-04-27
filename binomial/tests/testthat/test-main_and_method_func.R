source("../../R/binomial.R")
context("Test for main and method functions")

test_that("bin_choose work as expected",{
  n1 <- 3
  k1 <- 4
  expect_error(bin_choose(n1, k1))
  n2 <- 3
  k2 <- 0:5
  expect_error(bin_choose(n2, k1))
  n3 <- 5
  k3 <- 2
  expect_equal(bin_choose(n3, k3), 10)
  expect_length(bin_choose(n3, k3), 1)
  n4 <- 5
  k4 <- 0
  expect_equal(bin_choose(n4, k4), 1)
  expect_length(bin_choose(n4, k4), 1)
  n5 <- 5
  k5 <- 0:2
  expect_equal(bin_choose(n5, k5), c(1, 5, 10))
  expect_length(bin_choose(n5, k5), 3)
})

test_that("bin_probability work as expected",{
  k1 <- 4
  n1 <- 3
  p1 <- 0.4
  expect_error(bin_probability(k1, n1, p1))
  k2 <- 0:2
  n2 <- 3
  p2 <- 1.1
  expect_error(bin_probability(k2, n2, p2))
  k3 <- 2
  n3 <- 5
  p3 <- 0.5
  expect_equal(bin_probability(k3, n3, p3), 0.3125)
  expect_length(bin_probability(k3, n3, p3), 1)
  k4 <- 0:2
  n4 <- 5
  p4 <- 0.5
  expect_equal(bin_probability(k4, n4, p4), c(0.03125, 0.15625, 0.31250))
  expect_length(bin_probability(k4, n4, p4), 3)
  k5 <- 55
  n5 <- 100
  p5 <- 0.45
  expect_equal(bin_probability(k5, n5, p5), 0.01075277)
  expect_length(bin_probability(k5, n5, p5), 1)
})

test_that("bin_distribution work as expected",{
  n1 <- 3
  p1 <- 1.1
  expect_error(bin_distribution(n1, p1))
  n2 <- 3.1
  p2 <- 1
  expect_error(bin_distribution(n2, p2))
  n3 <- 5
  p3 <- 0.5
  success <- 0:5
  df3 <- as.data.frame(success)
  df3$probability <- c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125)
  class(df3) <- c("bindis", "data.frame")
  expect_equal(class(bin_distribution(n3, p3)), c("bindis", "data.frame"))
  expect_equal(bin_distribution(n3, p3), df3)
})

test_that("bin_cumulative work as expected",{
  n1 <- 3
  p1 <- 1.1
  expect_error(bin_cumulative(n1, p1))
  n2 <- 3.1
  p2 <- 1
  expect_error(bin_cumulative(n2, p2))
  n3 <- 5
  p3 <- 0.5
  success <- 0:5
  cdf3 <- as.data.frame(success)
  cdf3$probability <- c(0.03125, 0.15625, 0.31250, 0.31250, 0.15625, 0.03125)
  cdf3$cumulative <- c(0.03125,  0.18750, 0.50000, 0.81250, 0.96875, 1.00000)
  class(cdf3) <- c("bincum", "data.frame")
  expect_equal(class(bin_cumulative(n3, p3)), c("bincum", "data.frame"))
  expect_equal(bin_cumulative(n3, p3), cdf3)
})
