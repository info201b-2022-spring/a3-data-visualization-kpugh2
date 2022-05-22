# Load libraries

library(dplyr)
library(ggplot2)

# Load in data

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# INTRODUCTION
# 
# 

# Select columns and rows
prison_race <- incarceration_trends %>%
  filter(state == "TX") %>%
  select(year, county_name, aapi_prison_pop, black_prison_pop, 
         latinx_prison_pop, native_prison_pop, white_prison_pop,
         total_prison_pop, aapi_pop_15to64, black_pop_15to64, 
         latinx_pop_15to64, native_pop_15to64, white_pop_15to64)

prison_race <- na.omit(prison_race)

# SUMMARY INFORMATION

# What is the mean prison population for each race?

prison_pop_means <- prison_race %>%
  summarise_at(vars("aapi_prison_pop", "black_prison_pop", "latinx_prison_pop", "native_prison_pop", "white_prison_pop"), mean)
aapi_mean <- prison_pop_means[1, "aapi_prison_pop"]
black_mean <- prison_pop_means[1, "black_prison_pop"]
latinx_mean <- prison_pop_means[1, "latinx_prison_pop"]
native_mean <- prison_pop_means[1, "native_prison_pop"]
white_mean <- prison_pop_means[1, "white_prison_pop"]

# What proportion of people from each race make up the the prison population?

prison_pop_prop <- prison_race %>%
  mutate(aapi_pop_prop = aapi_prison_pop/total_prison_pop) %>%
  mutate(black_pop_prop = black_prison_pop/total_prison_pop) %>%
  mutate(latinx_pop_prop = latinx_prison_pop/total_prison_pop) %>%
  mutate(native_pop_prop = native_prison_pop/total_prison_pop) %>%
  mutate(white_pop_prop = white_prison_pop/total_prison_pop) %>%
  select(year, county_name, aapi_pop_prop, black_pop_prop, latinx_pop_prop, native_pop_prop, white_pop_prop)

# What is the mean proportion of the prison population for each race?

prison_pop_prop[is.na(prison_pop_prop)] <- 0
prison_pop_prop_means <- prison_pop_prop %>%
  summarise_at(vars("aapi_pop_prop", "black_pop_prop", "latinx_pop_prop", "native_pop_prop", "native_pop_prop", "white_pop_prop"), mean)
aapi_pop_prop_mean <- prison_pop_prop_means[1, "aapi_pop_prop"]
black_pop_prop_mean <- prison_pop_prop_means[1, "black_pop_prop"]
latinx_pop_prop_mean <- prison_pop_prop_means[1, "latinx_pop_prop"]
native_pop_prop_mean <- prison_pop_prop_means[1, "native_pop_prop"]
white_pop_prop_mean <- prison_pop_prop_means[1, "white_pop_prop"]

# What is the proportion of people in prison for each race?

prison_proportions <- data.frame(prison_race)

prison_proportions <- prison_race %>%
  mutate(aapi_prop = aapi_prison_pop/aapi_pop_15to64) %>%
  mutate(black_prop = black_prison_pop/black_pop_15to64) %>%
  mutate(latinx_prop = latinx_prison_pop/latinx_pop_15to64) %>%
  mutate(native_prop = native_prison_pop/native_pop_15to64) %>%
  mutate(white_prop = white_prison_pop/white_pop_15to64) %>%
  select(year, county_name, aapi_prop, black_prop, latinx_prop, native_prop, white_prop)

# What is the maximum proportion for each race?

prison_proportions[is.na(prison_proportions)] <- 0
aapi_max_prop <- max(prison_proportions$aapi_prop)
black_max_prop <- max(prison_proportions$black_prop)
latinx_max_prop <- max(prison_proportions$latinx_prop)
native_max_prop <- max(prison_proportions$native_prop)
white_max_prop <- max(prison_proportions$white_prop)

# Which county has the most black prisons? The highest black proportion?

most_black_prisoners <- prison_race %>%
  filter(black_prison_pop == max(prison_race$black_prison_pop)) %>%
  pull(county_name)
highest_black_prop <- prison_proportions %>%
  filter(black_prop == black_max_prop) %>%
  pull(county_name)

