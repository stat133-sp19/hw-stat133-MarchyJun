library(ggplot2)

### 1 R Functions ###
## 1.1 Private Checker Fuctions
# Function check_prob()
# check whether prob is valid number between 0 and 1
check_prob <- function(prob){
  if ((0 <= prob) & (prob <= 1)){
    return(TRUE)
  }
  else{
    stop('invalid prob value: p has to be a number: p should be the number between 0 and 1')
  }
}

# Function check_trials()
# check whether the number of trials is valid number: trials should be non-negative integer
check_trials <- function(trials){
  if (trials >=0){
    return(TRUE)
  }
  else{
    stop('invalid trials value: n should be non-negative integer')
  }
}

# Function check_success()
# check whether the number of success is valid number: success should be the number between 0 and trials
check_success <- function(success, trials){
  if ((sum(success <= trials) == length(success)) & (sum(0 <= success) == length(success))){
    return(TRUE)
  }
  else {
    stop('invalid success value')
  }
}

## 1.2 Private Auxiliary Functions
# function aux_mean : calculate mean of binomial distribution when trials and prob is given
aux_mean <- function(trials, prob){
  return(trials * prob)
}

# function aux_variance : calculate variance of binomial distribution when trials and prob is given
aux_variance <- function(trials, prob){
  return(trials * prob * (1 - prob))
}

# function aux_mode : calculate mode of binomial distribution when trials and prob is given
aux_mode <- function(trials, prob){
  return(trunc(trials * prob + prob))
}

# function aux_skewness : calculate skewness of binomial distribution when trials and prob is given
aux_skewness <- function(trials, prob){
  return((1 - 2*prob) / sqrt(trials * prob * (1 - prob)))
}

# function aux_kurtosis : calculate kurtosis of binomial distribution when trials and prob is given
aux_kurtosis <- function(trials, prob){
  return((1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))
}

## 1.3 Function bin_choose()
#'@title: bin_choose()
#'@description: calculate the number of combinations in which k successes can occur in n trials
#'@param n the number of trials
#'@param k the number of successes
#'@return: calculated number of combinations when n and k are given

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

## 1.4 Function bin_probability()
#'@title: bin_probability()
#'@description: calculate the probability of binomial distribution
#'@param success the number of success out of trials
#'@param trials the number of trials
#'@param prob the probability of success
#'@return calcualted probability of binomial when success, trials, and probability of success are given
#'@export

bin_probability <- function(success, trials, prob){
  if (!check_prob(prob)){stop('invalid prob value')}
  if (!check_trials(trials)){stop('invalid trials value')}
  if (!check_success(success, trials)){stop('invalid success value')}
  return(bin_choose(trials, success) * prob^success * (1 - prob)^(trials - success))
}

## 1.5 Function bin_distribution()
#'@title: bin_distribution()
#'@description: get data frame with the binomial probability distribution
#'@param trials
#'@param prob
#'@return: data frame with the binomial probability distribution
#'@export

bin_distribution <- function(trials, prob){
  success = seq(0, trials, 1)
  probability = bin_probability(success, trials, prob)
  result_df = data.frame(success = success,
                         probability = probability)
  class(result_df) <- c('bindis','data.frame')
  result_df
}

#'@export plot.binds()
plot.bindis <- function(x){
    ggplot(data = x, aes(x = success, y = probability)) +
    geom_bar(stat = 'identity') +
    scale_x_continuous(breaks = seq(0,length(x$success)))
}

## 1.6 Function bin_cumulative()
#'@title: bin_cumulative()
#'@description: get data frame with both the probability distribution and the cummulative probabilities
#'@param trials
#'@param prob
#'@return: data frame with both the probability distribution and the cummulative probabilities
#'@export

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


#'@export plot.bincum()
plot.bincum <- function(x){
  ggplot(data = x, aes(x = success, y = cumulative)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(0,length(x$success))) +
    scale_y_continuous(breaks = seq(0,1,0.2)) +
    labs(y = 'probability')
}

## 1.7 Function bin_variable()
#'@title: bin_variable()
#'@description: check numbers of input trials and probability and shows input trials and prob
#'@param trials
#'@param prob
#'@return: input trials and prob
#'@export

bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  res <- list(trials = trials,
              prob = prob)
  class(res) <- 'binvar'
  res
}

#'@export
print.binvar <- function(x){
  cat('"Binomial variable"\n\n')
  cat('Paramaters\n')
  cat('-number of trials:', x$trials,'\n')
  cat('-prob of success:', x$prob)
  invisible(x)
}

#'@export
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

#'@export
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

## 1.8 Functions of measures()
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}





