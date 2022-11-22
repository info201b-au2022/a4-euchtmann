library(tidyverse)
library("ggplot2")
library(usmap)

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
# Data Summary

# data wrangling for race populations in prison
data <- get_data(num_records = -1)
# View(data)

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

# Value 1 function 
# Which race has the highest cumulative prison population count?
high_prison_count <- prison_data %>%
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
#black_prison_pop

# Value 2 function
# Which race has the lowest cumulative prison population count?
low_prison_count <- prison_data %>%
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
#aapi_prison_pop

# Value 3 function
# What race currently has the highest prison count?
current_high_prison_count <- prison_data %>%
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
#black_prison_pop

# Value 4
# What race currently has the lowest prison count?
current_low_prison_count <- prison_data %>%
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
#aapi_prison_pop

#----------------------------------------------------------------------------#
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# This function wrangles data to show to increase in jail populations from 1970 to 2018
get_year_jail_pop <- function() {
  bar_chart_data <- data %>% 
    arrange(year) %>%
    filter(between (year, 1970, 2018)) %>%
    select(year, total_jail_pop) %>%
    # prettyNum(total_jail_pop, big.mark = ",", scientific = FALSE) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
  return(bar_chart_data)
}


# This function creates the bar chart of jail populations from 1970 to 2018
# (1) Takes no parameters
# (2) Should call the data wrangling function.
# DO NOT KNOW WHERE TO CALL DATA WRANGLING FUNCTION
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
    ) +
  #format(total_jail_pop, big.mark = ",", scientific = FALSE)
  options(scipen = 999) # only way I've found that works, but still no commas
  return(plot)   
} 

# odd error but chart still works - 
  # Error in `ggplot_add()`: 
  # Can't add `o` to a ggplot object
chart <- plot_jail_pop_for_us()
chart



#----------------------------------------------------------------------------#
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function 
# parameter states : vector of states
states = c("WA", "OR", "CA")

get_jail_pop_by_states <- function(states) {
  line_chart_data <- data %>%
    drop_na() %>%
    filter(between (year, 1970, 2018) &
             (state %in% states)) %>%
    select(state, year, total_jail_pop) %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
  return(line_chart_data)
}
test <- get_jail_pop_by_states(c("WA", "OR", "CA"))
test

# This function creates a line chart of prison population growth for multiple states from 1970 to 2018
# 1.) parameter states : vector of states
# 2.) This function should call the data wrangling function.
# DO NOT KNOW WHERE TO CALL DATA WRANGLING FUNCTION
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(data = line_chart_data) +
    geom_line(
      mapping = aes(
        x = year, 
        y = total_jail_pop, 
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
# Black v. White Prison Population Trend 

# This function wrangles data comparing the prison populations of Black and White people
# from 2000 to 2013
scatter_data_wrangling <- function(data) {
  scatter_plot_data <- data %>%
    drop_na() %>%
    group_by(year) %>%
    select(year, black_prison_pop, white_prison_pop) %>%
    summarize(black_prison_pop = sum(black_prison_pop), 
              white_prison_pop = sum(white_prison_pop)
    ) %>%
    pivot_longer("black_prison_pop":"white_prison_pop", 
                 names_to = "race", 
                 values_to = "population")
  return(scatter_plot_data)
  }

# This function creates a scatter plot comparing the prison populations of Black and 
# White people from 2000 to 2013
scatter_plot <- function() {
  plot <- ggplot(data = scatter_plot_data) +
    geom_point(
      mapping = aes(
        x = year, 
        y = population, 
        color = race )
      ) + 
    labs(
      x = "Year", 
      y = "Prison Population", 
      title = "Black Prison Population versus White Prison Population (2000 - 2013)"
    )
  return(plot)
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Racial Minority Prison Population Percentage by State

# This function wrangles data to show the current percentage of racial minorities
# in the prison population.  
map_data_wrangling <- function() {
  map_data <- data %>%
    drop_na() %>%
    filter(year == max(year)) %>%
    group_by(state) %>%
    summarize(
      black_prison_pop = sum(black_prison_pop), 
      white_prison_pop = sum(white_prison_pop),
      aapi_prison_pop = sum(aapi_prison_pop), 
      latinx_prison_pop = sum(latinx_prison_pop), 
      native_prison_pop = sum(native_prison_pop), 
      minority_prison_pop = black_prison_pop +
        aapi_prison_pop +
        latinx_prison_pop +
        native_prison_pop,
      total_prison_pop = black_prison_pop +
        aapi_prison_pop +
        latinx_prison_pop +
        native_prison_pop +
        white_prison_pop,
      percent = (minority_prison_pop / total_prison_pop) * 100
    ) %>%
    select(state, percent)
return(map_data)

}

# This function creates a map that shows the current percentage of racial minorities 
# in the prison population
map_plot <- function() {
  plot <- plot_usmap(
    regions = c("state"), 
    data = map_data, 
    values = "percent", 
    label = TRUE, 
    label_color = "black"
  ) +
    scale_fill_continuous(
      low = "white", 
      high = "blue", 
      name = "Percent"
    ) +
    labs(title = "Percentage of People of Color in the Prison Population in Each State")
  return(plot)  
}

#----------------------------------------------------------------------------#



