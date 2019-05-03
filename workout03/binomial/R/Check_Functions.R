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
