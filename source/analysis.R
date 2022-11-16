library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#USE DPLYR

#call function get_data to view data

# Explain how they relate to patterns of inequality
#value 1 function

#value 2 function

#value 3 function 

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
# This data wrangling function should return a data frame that is suitable for 
# visualization. This function takes no parameters. 
get_year_jail_pop <- function() {
  # TODO: Implement this function 
return()   
}

# This function ... <todo:  update comment>
# This plotting function should return the chart. This function: (1) Takes no 
# parameters; and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
# Use DPLYR and ggplot2 to produce a line chart that shows the growth of the U.S. 
# prison population from 1970 to 2018 by one or more states. 
# You should write two functions: 

# 1. This data wrangling function should return a data frame that is suitable 
# for visualization. The parameter states should be a vector of states
get_jail_pop_by_states <- function(states) {
  
}

# 2. This plotting function should return the chart. The parameter states should
# be a vector of states. This function should call the data wrangling function.
plot_jail_pop_by_states <- function(states) {
  
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


