library(tidyverse)
library("ggplot2")

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
library("dplyr")

# call function get_data to view data
data <- get_data(num_records = -1)
View(data)

# drop NA values as well as select relevant columns 
# to values of interest
prison_data <- data %>%
  drop_na() %>%
  select(year, 
         state,
         county_name, 
         aapi_prison_pop, 
         black_prison_pop, 
         latinx_prison_pop, 
         native_prison_pop, 
         white_prison_pop)
View(prison_data)

#value 1 function 
# Which race has the highest cumulative prison population count?
high_prison_count <- data %>%
  summarize(
    aapi_prison_pop = sum(aapi_prison_pop), 
    black_prison_pop = sum(black_prison_pop), 
    latinx_prison_pop = sum(latinx_prison_pop), 
    native_prison_pop = sum(native_prison_pop), 
    white_prison_pop = sum(white_prison_pop)
  ) %>%
  pivot_longer("aapi_prison_pop":"white_prison_pop", 
               names_to = "race_prison_pop", 
               values_to = "total_race_pop") %>%
  filter(total_race_pop == max(total_race_pop)) %>%
  pull(race_prison_pop)
high_prison_count

#value 2 function
# Which race has the lowest cumulative prison population count?
low_prison_count <- data %>%
  summarize(
    aapi_prison_pop = sum(aapi_prison_pop), 
    black_prison_pop = sum(black_prison_pop), 
    latinx_prison_pop = sum(latinx_prison_pop), 
    native_prison_pop = sum(native_prison_pop), 
    white_prison_pop = sum(white_prison_pop)
  ) %>%
  pivot_longer("aapi_prison_pop":"white_prison_pop", 
               names_to = "race_prison_pop", 
               values_to = "total_race_pop") %>%
  filter(total_race_pop == min(total_race_pop)) %>%
  pull(race_prison_pop)
low_prison_count

#value 3 function
# What race currently has the highest prison count?
current_high_prison_count <- data %>%
  filter(year == max(year)) %>%
  summarize(
    aapi_prison_pop = sum(aapi_prison_pop), 
    black_prison_pop = sum(black_prison_pop), 
    latinx_prison_pop = sum(latinx_prison_pop), 
    native_prison_pop = sum(native_prison_pop), 
    white_prison_pop = sum(white_prison_pop)
    ) %>%
  pivot_longer("aapi_prison_pop":"white_prison_pop", 
               names_to = "race_prison_pop", 
               values_to = "total_race_pop") %>%
  filter(total_race_pop == max(total_race_pop)) %>%
  pull(race_prison_pop)
current_high_prison_count

# value 4
# What race currently has the lowest prison count?
current_low_prison_count <- data %>%
  filter(year == max(year)) %>%
  summarize(
    aapi_prison_pop = sum(aapi_prison_pop), 
    black_prison_pop = sum(black_prison_pop), 
    latinx_prison_pop = sum(latinx_prison_pop), 
    native_prison_pop = sum(native_prison_pop), 
    white_prison_pop = sum(white_prison_pop)
  ) %>%
  pivot_longer("aapi_prison_pop":"white_prison_pop", 
               names_to = "race_prison_pop", 
               values_to = "total_race_pop") %>%
  filter(total_race_pop == min(total_race_pop)) %>%
  pull(race_prison_pop)
current_low_prison_count

# 
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
# Use DPLYR and ggplot2 to replicate Figure 1 (above).  That is, produce a bar 
# chart that shows the growth of the U.S. prison population from 1970 to 2018.  
# To organize your code, you should create two functions: 

# This data wrangling function should return a data frame that is suitable for 
# visualization. This function takes no parameters. 
get_year_jail_pop <- function() {
  bar_chart_data <- data %>% 
    arrange(year) %>%
    filter(between (year, 1970, 2018)) %>%
    select(year, total_jail_pop) %>%
    # prettyNum(total_jail_pop, big.mark = ",", scientific = FALSE) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
    # prettyNum(total_jail_pop, big.mark = ",", scientific = FALSE)
  return(bar_chart_data)   
}

# This function ... <todo:  update comment>
# This plotting function should return the chart. This function: (1) Takes no 
# parameters; and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(data = bar_chart_data) +
    geom_col(
      mapping = aes(
        x = year, 
        y = total_jail_pop,
      )
    ) +
    labs(
      x = "Year", 
      y = "Total Jail Population", 
      title = "Increase of Jail Population in U.S. (1970 - 2018)"
    ) 
  #format(total_jail_pop, big.mark = ",", scientific = FALSE)
  #options(scipen = 999)
  return(plot)   
} 

chart <- plot_jail_pop_for_us()
chart
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

#line_chart_data <- data %>% 
#  arrange(year) %>%
#  filter(between (year, 1970, 2018)) %>%
#  select(state, year, total_jail_pop) %>%
#  drop_na() %>%
#  group_by(year) %>%
#  summarize(total_jail_pop = sum(total_jail_pop))
#View(line_chart_data)  

get_jail_pop_by_states <- function(states) {
  line_chart_data <- data %>%
    arrange(year) %>%
    filter(between(year, 1970, 2018)) %>%
    # filter(state == states) %>%
    select(state, year, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
}

test <- get_jail_pop_by_states(c("WA", "OR", "CA"))
test

# 2. This plotting function should return the chart. The parameter states should
# be a vector of states. This function should call the data wrangling function.
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(data = line_chart_data) +
    geom_line(
      mapping = aes(
        x = year, 
        y = total_jail_pop, 
        group = states, 
        color = states, 
      )
    ) + 
    labs(
      x = "Year", 
      y = "Total Jail Population", 
      title = "Increase of Jail Population in U.S. States (1970 - 2018)"
    )
  return(plot)
}

test <- plot_jail_pop_by_states(c("WA", "OR", "CA"))
test

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
# two different continuous varibable representing a trend in the dataset
# Scatter plot
# black men in prison versus total in prison?



#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas

# one coninuous varibale by state
# Mapping ration of black_prison_pop total to total_prison_pop
# by current year







#----------------------------------------------------------------------------#

## Load data frame ---- 


