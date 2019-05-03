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
