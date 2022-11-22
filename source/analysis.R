library(tidyverse)
library("ggplot2")
library(usmap)
source("../source/a4-helpers.R")
#----------------------------------------------------------------------------#
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Data Summary

# data wrangling for race populations in prison
data <- get_data(num_records = -1)

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
 plot_jail_pop_for_us <- function()  {
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(
      mapping = aes(
        x = year, 
        y = total_jail_pop,
      )
    ) +
    labs(
      x = "Year", 
      y = "Total Jail Population", 
      title = "Increase of Jail Population in U.S. (1970 - 2018)", 
      caption = "Bar Chart Showing the Growth of the U.S. Prison Population from 1970 to 2018"
    )
  return(plot)   
} 

test <- plot_jail_pop_for_us()
test
 

#----------------------------------------------------------------------------#
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function wrangles data of prison population growth for multiple states from 1970 to 2018
get_jail_pop_by_states <- function(states) {
  line_chart_data <- data %>%
    filter(between (year, 1970, 2018) &
             (state %in% states)) %>%
    select(state, year, total_jail_pop) %>%
    group_by(year, state) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(line_chart_data)
}

# This function creates a line chart of prison population growth for multiple states from 1970 to 2018
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(data = get_jail_pop_by_states()) +
    geom_line(
      mapping = aes(
        x = year, 
        y = total_jail_pop, 
        color = state, 
      )
    ) + 
    labs(
      x = "Year", 
      y = "Total Jail Population", 
      title = "Increase of Jail Population in U.S. States (1970 - 2018)", 
      caption = "Line Chart Showing the Growth of the U.S. Prison Population by States from 1970 to 2018"
    )
return(plot) 
}

test <- plot_jail_pop_by_states(c("WA", "OR", "CA"))
test

#----------------------------------------------------------------------------#
## Section 5  ---- 
#----------------------------------------------------------------------------#
# Black v. White Prison Population Trend 

# This function wrangles data comparing the prison populations of Black and White people from 2000 to 2013
scatter_data_wrangling <- function(data) {
  scatter_plot_data <- data %>%
    drop_na() %>%
    group_by(year) %>%
    select(year, black_prison_pop, white_prison_pop) %>%
    summarize(black_prison_pop = sum(black_prison_pop, na.rm = T), 
              white_prison_pop = sum(white_prison_pop, na.rm = T)
    ) %>%
    pivot_longer("black_prison_pop":"white_prison_pop", 
                 names_to = "race", 
                 values_to = "population")
  return(scatter_plot_data)
}

# This function creates a scatter plot comparing the prison populations of Black and 
# White people from 2000 to 2013
scatter_plot <- function() {
  plot <- ggplot(data = scatter_data_wrangling(data)) +
    geom_point(
      mapping = aes(
        x = year, 
        y = population, 
        color = race )
    ) + 
    labs(
      x = "Year", 
      y = "Prison Population", 
      title = "Black Prison Population versus White Prison Population (2000 - 2013)", 
      caption = "Scatter Plot Showing the Difference in White and Black Prison Populations from 2000 - 2013"
    )
  return(plot)
}

test <- scatter_plot()
test


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Racial Minority Prison Population Percentage by State

# This function wrangles data to show the current percentage of racial minorities
# in the prison population.  

# NOW PRINTING NaN VALUES
map_data <- data %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(
    black_prison_pop = sum(black_prison_pop, na.rm = T), 
    white_prison_pop = sum(white_prison_pop, na.rm = T),
    aapi_prison_pop = sum(aapi_prison_pop, na.rm = T), 
    latinx_prison_pop = sum(latinx_prison_pop, na.rm = T), 
    native_prison_pop = sum(native_prison_pop, na.rm = T),
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
View(map_data)


map_data_wrangling <- function() {
  return()

}

# This function creates a map that shows the current percentage of racial minorities 
# in the prison population
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
  labs(title = "Percentage of People of Color in the Prison Population in Each State",
       caption = "U.S. Map Showing Most Recent Data of the Percentage of POC Peoples in Prison")
plot

map_plot <- function() {
  return(plot)
}

#----------------------------------------------------------------------------#



