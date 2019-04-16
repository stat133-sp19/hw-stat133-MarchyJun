### Basic Settings ###
library(shiny)
library(ggplot2)
library(reshape2)

### FV, FVA, FVGA functions ###
# FV function
#' @title: Future Value Function
#' @description: Computes the futuer value of an investment
#' @param Initial_Amount: initial invested amount
#' @param Return_Rate: annual rate of return
#' @param Years: number of years
#' @return: Computed future value of an investment with given rate at the end of years

FV <- function(Initial_Amount, Return_Rate, Years){
  return(Initial_Amount*(1 + (Return_Rate/100))^Years)
}

# FVA function
#' @title: Future Value of Annuity
#' @description: Computes the future value of annuity when there is some kind of contribution
#' @param Annual_Contribution: contribution(i.e how much you deposit at the end of each year)
#' @param Return_rate: annual rate of return
#' @param Years: time(in years)
#' @return: Computed future value of annuity with given rate at the end of years when there is some kind of contribution

FVA <- function(Annual_Contribution, Return_Rate, Years){
  return( Annual_Contribution*((1 + Return_Rate/100)^Years - 1)/(Return_Rate/100)) 
}


# FVGA function
#' @title: Future Value of Growing Annuity
#' @description: Computes the future value of annuity when there is some kind of contribution that grows each year
#' @param Annual_Contribution: first contribution(i.e. how much you deposit at the end of year 1) 
#' @param Return_Rate: annual rate of return
#' @param Growth_Rate: growth rate
#' @param Years: time(in years)
#' @return: Computed future value of annuity with given rate at the end of years when there is some kind of contribution that grows each year

FVGA <- function(Annual_Contribution, Return_Rate, Growth_Rate, Years){
  return( Annual_Contribution*((1 + Return_Rate/100)^Years-(1 + Growth_Rate/100)^Years)/((Return_Rate/100)-(Growth_Rate/100)))
}

### get_balances_table function ###

get_balances_timelines <- function(Initial_Amount, Annual_Contribution, Return_Rate,Growth_Rate,Years,Facet){
  # get no_contrib, fixed_contrib, growing_contrib
  no_contrib <- c()
  fixed_contrib <- c()
  growing_contrib <- c()
  
  for (n in seq(0,Years,1)){
    FV_value <- FV(Initial_Amount, Return_Rate, n)
    FVA_value <- FVA(Annual_Contribution, Return_Rate, n)
    FVGA_value <- FVGA(Annual_Contribution, Return_Rate, Growth_Rate, n)
    no_contrib[n+1] <-  FV_value
    fixed_contrib[n+1] <-  FV_value + FVA_value
    growing_contrib[n+1] <-  FV_value + FVGA_value
  }
  
  # get balances_table
  balances <- data.frame('year' = seq(0,Years,1),
                         'no_contrib' = no_contrib,
                         'fixed_contrib' = fixed_contrib,
                         'growing_contrib' = growing_contrib)
  # get balances_melted_table
  balances_melted <- reshape2::melt(balances, id.var='year')
  
  # set graphic margin
  
  
  # get timlines_graph
  if (Facet == 'No') {
    timelines <- ggplot(data = balances_melted, aes(x = year, y = value, col = variable)) +
      geom_line()+
      geom_point()+
      scale_x_continuous(breaks = seq(0,length(balances_melted$year))) +
      labs(x = 'year', y = 'value', title = 'Three modes of investing') +
      theme(legend.position = 'right')
  }
  
  else {
    timelines <- ggplot(data = balances_melted, aes(x = year, y = value, fill = variable, col = variable)) +
      geom_area(aes(alpha = 0.5))+
      geom_line()+
      geom_point()+
      scale_x_continuous(breaks = seq(0,length(balances_melted$year))) +
      labs(x = 'year', y = 'value', title = 'Three modes of investing') +
      facet_grid( ~ variable)
    theme(legend.position = 'right')
  }
  
  # return
  return(list(balances, timelines))
}