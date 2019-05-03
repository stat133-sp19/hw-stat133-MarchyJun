library(ggplot2)
library(roxygen2)
library(devtools)

## Function bin_choose()
#' @title: bin_choose()
#' @description: calculate the number of combinations in which k successes can occur in n trials
#' @param n: the number of trials
#' @param k: the number of successes
#' @return: calculated number of combinations when n and k are given
#' @example: bin_choose(n = 5, k = 2)
#' @example: bin_choose()
#' @example: bin_choose(5, 1:3)
#' @export

bin_choose <- function(n, k){
  if (length(n) == length(k)) {
    if (n >= k) { return(factorial(n) / (factorial(k) * factorial(n - k))) }
    else { stop('k cannot be greater than n')}
  }
  else{
    n = rep(n, length(k))
    if (sum(n >= k) == length(n)) { return(factorial(n) / (factorial(k) * factorial(n - k))) }
    else { stop('k cannot be greater than n') }
  }
}

## Function bin_probability()
#' @title: bin_probability()
#' @description: calculate the probability of binomial distribution
#' @param success the number of success out of trials
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return calcualted probability of binomial when success, trials, and probability of success are given
#' @example: bin_probability(success = 2, trials = 5, prob = 0.5)
#' @example: bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' @example: bin_probability(success = 55, trials = 100, prob = 0.45)
#' @export

bin_probability <- function(success, trials, prob){
  if (!check_prob(prob)){stop('invalid prob value')}
  if (!check_trials(trials)){stop('invalid trials value')}
  if (!check_success(success, trials)){stop('invalid success value')}
  return(bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success))
}

## Function bin_distribution()
#' @title: bin_distribution()
#' @description: get data frame with the binomial probability distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: data frame with the binomial probability distribution
#' @example: bin_distribution(trials = 5, prob = 0.5)
#' @export

bin_distribution <- function(trials, prob){
  success = seq(0, trials, 1)
  probability = bin_probability(success, trials, prob)
  result_df = data.frame(success = success,
                         probability = probability)
  class(result_df) <- c('bindis','data.frame')
  result_df
}

#' @export
plot.bindis <- function(x){
    ggplot(data = x, aes(x = success, y = probability)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(breaks = seq(0,length(x$success)))
}

## Function bin_cumulative()
#' @title: bin_cumulative()
#' @description: get data frame with both the probability distribution and the cummulative probabilities
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: data frame with both the probability distribution and the cummulative probabilities
#' @example: bin_cumulative(trials = 5, prob = 0.5)
#' @export

bin_cumulative <- function(trials, prob){
  success = seq(0, trials, 1)
  probability = bin_probability(success, trials, prob)
  cumulative = c()
  total = 0
  i = 1
  for (p in probability){
    total = p + total
    cumulative[i] = total
    i = i + 1
  }
  result_df = data.frame(success = success,
                         probability = probability,
                         cumulative = cumulative)

  class(result_df) <- c('bincum', 'data.frame')
  result_df
}


#' @export
plot.bincum <- function(x){
  ggplot(data = x, aes(x = success, y = cumulative)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(0,length(x$success))) +
    scale_y_continuous(breaks = seq(0,1,0.2)) +
    labs(y = 'probability')
}

## Function bin_variable()
#' @title: bin_variable()
#' @description: check numbers of input trials and probability and shows input trials and prob
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: input trials and prob
#' @example: bin_variable(trials = 10, p = 0.3)
#' @export

bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  res <- list(trials = trials,
              prob = prob)
  class(res) <- 'binvar'
  res
}

#' @export
print.binvar <- function(x){
  cat('"Binomial variable"\n\n')
  cat('Paramaters\n')
  cat('-number of trials:', x$trials,'\n')
  cat('-prob of success:', x$prob)
  invisible(x)
}

#' @export
summary.binvar <- function(x){
  trials <-  x$trials
  p <-  x$prob
  mean_result <- aux_mean(trials, p)
  variance_result <- aux_variance(trials, p)
  mode_result <- aux_mode(trials, p)
  skewness_result <- aux_skewness(trials, p)
  kurtosis_result <- aux_kurtosis(trials, p)
  result <- list(trials = trials,
                 prob = p,
                 mean = mean_result,
                 variance = variance_result,
                 mode = mode_result,
                 skewness = skewness_result,
                 kurtosis = kurtosis_result)
  class(result) <- 'summary.binvar'
  result
}

#' @export
print.summary.binvar <- function(x){
  cat('"Summary Binomial"\n\n')
  cat('Paramaters\n')
  cat('- number of trials:', x$trials,'\n')
  cat('- prob of success:', x$prob, '\n\n')
  cat('Measures\n')
  cat('- mean    :', x$mean, '\n')
  cat('- variance:', x$variance, '\n')
  cat('- mode    :', x$mode, '\n')
  cat('- skewness:', x$skewness, '\n')
  cat('- kurtosis:', x$kurtosis)
  invisible(x)
}

## Functions bin_mean()
#' @title: bin_mean()
#' @description: mean of binomial distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: calculated mean of binomial distribution
#' @example: bin_mean(trials = 10, p = 0.3)
#' @export
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

## Functions bin_variance()
#' @title: bin_variance()
#' @description: variance of binomial distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: calculated variance of binomial distribution
#' @example: bin_variance(trials = 10, p = 0.3)
#' @export
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

## Functions bin_mode()
#' @title: bin_mode()
#' @description: mode of binomial distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: calculated mode of binomial distribution
#' @example: bin_mode(trials = 10, p = 0.3)
#' @export
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

## Functions bin_skewness()
#' @title: bin_skewness()
#' @description: skewness of binomial distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: calculated skewness of binomial distribution
#' @example: bin_skewness(trials = 10, p = 0.3)
#' @export
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

## Functions bin_kurtosis()
#' @title: bin_kurtosis()
#' @description: kurtosis of binomial distribution
#' @param trials: the number of trials
#' @param prob: the probability of success
#' @return: calculated kurtosis of binomial distribution
#' @example: bin_kurtosis(trials = 10, p = 0.3)
#' @export
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}





