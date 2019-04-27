# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(ggplot2)

# an auxiliary function to test if an input prob is a valid probability value
check_prob <- function(prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' values must be between 0 and 1")
  }
  TRUE
}

# an auxiliary function to test if an input trials is a valid value
check_trials <- function(trials) {
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' values must be non-negative integer")
  }
  TRUE
}

# an auxiliary function check_success() to test if an input success is a valid value for number of successes
check_success <- function(success, trials) {
  if (any(round(success) != success)) {
    stop("\ninvalid success value")}
  if (any(success > trials) ){
    stop("\nsuccess cannot be greater than trials")
  }
  TRUE
}


# an auxiliary function to calculate the mean value of a binomial distribution
aux_mean <- function(trials, prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' is invalid")
  }
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' is invalid")
  }

  trials * prob
}

# an auxiliary function to calculate the variance value of a binomial distribution
aux_variance <- function(trials, prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' is invalid")
  }
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' is invalid")
  }
  trials * prob * (1 - prob)
}

# an auxiliary function to calculate the mode value of a binomial distribution
aux_mode <- function(trials, prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' is invalid")
  }
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' is invalid")
  }
  if(round(trials * prob + prob) != trials * prob + prob){
    return(floor(trials * prob + prob))
  }else{
    return(c(floor(trials * prob + prob), floor(trials * prob + prob) - 1))
  }
}

# an auxiliary function to calculate the skewness value of a binomial distribution
aux_skewness <- function(trials, prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' is invalid")
  }
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' is invalid")
  }
  (1 - 2 * prob) / sqrt(trials * prob * (1 - prob))
}

# an auxiliary function to calculate the kurtosis value of a binomial distribution
aux_kurtosis <- function(trials, prob) {
  if (prob < 0 | prob > 1) {
    stop("\n'prob' is invalid")
  }
  if (trials < 0 | round(trials) != trials) {
    stop("\n'trials' is invalid")
  }
  (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob))
}

#' @title bin_choose
#' @description calculate the number of combinations in which k successes can occur in n trials
#' @param n value of trials
#' @param k value of successes
#' @result the number of combinations
#' @export
#' @examples
#' bin_choose(5, 2)
#' 10
bin_choose <- function(n = 5, k = 2){
  check_trials(n)
  check_success(k, n)
  factorial(n) / (factorial(k) * factorial(n - k))
}

#' @title bin_probability
#' @description calculate the probability of k successes occurring in n trials
#' @param success value of successes
#' @param trials value of trials
#' @param prob probability of one independent success
#' @result probability of k successes occurring in n trials
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' 0.3125
bin_probability <- function(success = 2, trials = 5, prob = 0.5){
  check_success(success, trials)
  check_trials(trials)
  check_prob(prob)
  (bin_choose(trials,success)) * (prob ^ success) * ((1 - prob) ^ (trials - success))
}

#' @title bin_distribution
#' @description obtain a data frame with the probability distribution
#' @param trials value of trials
#' @param prob probability of one independent success
#' @result the probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' success probability
## 1 0 0.03125
## 2 1 0.15625
## 3 2 0.31250
## 4 3 0.31250
## 5 4 0.15625
## 6 5 0.03125
bin_distribution <- function(trials = 5, prob = 0.5){
  check_trials(trials)
  check_prob(prob)
  success <- 0:trials
  df <- as.data.frame(success)
  df$probability <- bin_probability(0:trials, trials, prob)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

#' @export
plot.bindis <- function(df){
  bpt <- ggplot(df,aes(success, probability)) + geom_bar(stat = "identity")
  bpt
}
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)

#' @title bin_cumulative
#' @description obtain a data frame with both the probability distribution and the cumulative probabilities
#' @param trials value of trials
#' @param prob probability of one independent success
#' @result the probability distribution and the cumulative probabilities
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#'
## success probability cumulative
## 1 0 0.03125 0.03125
## 2 1 0.15625 0.18750
## 3 2 0.31250 0.50000
## 4 3 0.31250 0.81250
## 5 4 0.15625 0.96875
## 6 5 0.03125 1.00000
bin_cumulative <- function(trials = 5, prob = 0.5){
  check_trials(trials)
  check_prob(prob)
  cdf <- bin_distribution(trials, prob)
  cdf$cumulative <- cumsum(cdf$probability)
  class(cdf) <- c("bincum", "data.frame")
  return(cdf)
}

#' @export
plot.bincum <- function(cdf){
  df <- as.data.frame(cdf)
  lpt <- ggplot(df,aes(success, cumulative)) + geom_line(stat = "identity") + geom_point()
  lpt
}

#' @title bin_variable
#' @description obtain a binomial random variable object.(class "binvar")
#' @param trials value of trials
#' @param prob probability of one independent success
#' @result a list with probability and success
#' @export
#' @examples
#' bin_var(trials = 5, prob = 0.5)
#'
# > bin_var()
# $trials
# [1] 5
#
# $prob
# [1] 0.5
#
# attr(,"class")
# [1] "binvar"
bin_variable<- function(trials = 5, prob = 0.5){
  check_trials(trials)
  check_prob(prob)
  var <- list(trials = trials, prob = prob)
  class(var) <- "binvar"
  return (var)
}

#' @export
print.binvar <- function(bv){
  prbv <- cat(paste('""Binomial variable"\n\nParameters\n- number of trials: ', bv$trials, '\n- prob of success: ', bv$prob))
  invisible(bv)
  prbv
}

#' @export
summary.binvar <- function(bv){
  if(class(bv) != "binvar"){
    stop("input should be a 'binvar' object")
  }
  n = bv$trials
  p = bv$prob
  smbv <- list(trials = n , probs = p, mean = aux_mean(n, p), variance = aux_variance(n, p), mode = aux_mode(n, p), skewness = aux_skewness(n,p), kurtosis = aux_kurtosis(n, p))
  class(smbv) <- "summary.binvar"
  return (smbv)
}

#' @export
print.summary.binvar <- function(smbv){
  prsmbv <- cat(paste('"Summary Binomial"\n\nParameters\n- number of trials: ', smbv$trials, "\n- prob of success : ", smbv$probs, "\n\nMeasures\n- mean    : ", smbv$mean, "\n- variance: ", smbv$variance, "\n- mode    : ", smbv$mode, "\n- skewness: ", smbv$skewness,  "\n- kurtosis: ", smbv$kurtosis))
  invisible(smbv)
  prsmbv
}

#' @title bin_mean
#' @description to calculate the mean value of a binomial distribution
#' @param n value of trials
#' @param k value of successes
#' @result the mean value
#' @export
#' @examples
#' bin_mean(10, 0.3)
## [1] 3
bin_mean <- function(n = 10, p = 0.3){
  check_trials(n)
  check_prob(p)
  mean <- aux_mean(n, p)
  return(mean)
}

#' @title bin_variance
#' @description to calculate the variance value of a binomial distribution
#' @param n value of trials
#' @param k value of successes
#' @result the variance value
#' @export
#' @examples
#' bin_variance(10, 0.3)
## [1] 2.1
bin_variance <- function(n = 10, p = 0.3){
  check_trials(n)
  check_prob(p)
  variance <- aux_variance(n, p)
  return(variance)
}

#' @title bin_mode
#' @description to calculate the mode value of a binomial distribution
#' @param n value of trials
#' @param k value of successes
#' @result the mode value
#' @export
#' @examples
#' bin_mode(10, 0.3)
## [1] 2.1
bin_mode <- function(n = 10, p = 0.3){
  check_trials(n)
  check_prob(p)
  mode <- aux_mode(n, p)
  return(mode)
}

#' @title bin_skewness
#' @description to calculate the skewness value of a binomial distribution
#' @param n value of trials
#' @param k value of successes
#' @result the skewness value
#' @export
#' @examples
#' bin_skewness(10, 0.3)
## [1] 2.1
bin_skewness <- function(n = 10, p = 0.3){
  check_trials(n)
  check_prob(p)
  skewness <- aux_skewness(n, p)
  return(skewness)
}

#' @title bin_kurtosis
#' @description to calculate the kurtosis value of a binomial distribution
#' @param n value of trials
#' @param k value of successes
#' @result the kurtosis value
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
## [1] 2.1
bin_kurtosis <- function(n = 10, p = 0.3){
  check_trials(n)
  check_prob(p)
  kurtosis <- aux_kurtosis(n, p)
  return(kurtosis)
}






















