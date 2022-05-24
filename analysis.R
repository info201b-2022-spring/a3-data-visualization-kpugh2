# Load libraries

library(dplyr)
library(ggplot2)
library(plotly)
library(usmap)

# Load in data

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# INTRODUCTION

# Select columns and rows
prison_race <- incarceration_trends %>%
  filter(state == "TX") %>%
  select(year, county_name, fips, aapi_prison_pop, black_prison_pop, 
         latinx_prison_pop, native_prison_pop, white_prison_pop,
         total_prison_pop, aapi_pop_15to64, black_pop_15to64, 
         latinx_pop_15to64, native_pop_15to64, white_pop_15to64)

prison_race <- na.omit(prison_race)

# SUMMARY INFORMATION

# What is the mean prison population for each race?

prison_pop_means <- prison_race %>%
  summarise_at(vars("native_prison_pop", "aapi_prison_pop", "black_prison_pop", "latinx_prison_pop", "white_prison_pop"), mean)
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

# What is the maximum proportion of people in prison for each race?

prison_proportions[is.na(prison_proportions)] <- 0
aapi_max_prop <- max(prison_proportions$aapi_prop)
black_max_prop <- max(prison_proportions$black_prop)
latinx_max_prop <- max(prison_proportions$latinx_prop)
native_max_prop <- max(prison_proportions$native_prop)
white_max_prop <- max(prison_proportions$white_prop)

# Which county has the most black prisoners? The highest black proportion?

most_black_prisoners <- prison_race %>%
  filter(black_prison_pop == max(prison_race$black_prison_pop)) %>%
  pull(county_name, black_prison_pop)
highest_black_prop <- prison_proportions %>%
  filter(black_prop == black_max_prop) %>%
  pull(county_name, black_prop)

# Which county has the most prisoners?
most_prisoners <- prison_race %>%
  filter(total_prison_pop == max(prison_race$total_prison_pop)) %>%
  pull(county_name)

# CHARTS

# In Harris County (the county with the most prisoners) how has the number of black 
# prisoners changed each year?

harris_county <- prison_race %>%
  filter(county_name == "Harris County") %>%
  select(year, county_name, black_prison_pop, white_prison_pop)

# Chart displaying the number of black prisoners and the number of white prisoners in 
# Harris county prisons each year
# I could not figure out how to include a legend but the teal circles are black prisoners
# and the yellow triangles are white prisoners.
harris_county %>%
  tail(14) %>%
  ggplot( aes(x=year)) +
  geom_line(aes(y=black_prison_pop), color="blue") +
  geom_line(aes(y=white_prison_pop), color="red") +
  ylab("Prison Population") +
  geom_point(aes(y=black_prison_pop), shape=21, color="black", fill="#69b3a2", size=6) +
  geom_point(aes(y=white_prison_pop), shape=24, color="black", fill="#E69F00", size=6) +
  theme(legend.position = "right") +
  ggtitle("Black and White Prisoners in Harris County")

# filtering prison proportions to just Anderson County

anderson_county <- prison_proportions %>%
  filter(county_name == "Anderson County") %>%
  select(year, county_name, black_prop, white_prop)

# chart comparing the proportion of black people in prison and white people in prison

ggplot(anderson_county, aes(x=black_prop, y=white_prop)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.5,
    size=6,
    stroke = 2
  ) +
  xlab("Proportion of Black People in Prison") +
  ylab("Proportion of White People in Prison")


# MAP

# Map of Texas counties with the population of black prisoners

# New dataframe with the most recent population of black prisoners for each county
counties_prison_pop <- prison_race %>%
  select(year, county_name, fips, black_prison_pop) %>%
  group_by(county_name) %>%
  filter(year == max(year))

plot_usmap(data = counties_prison_pop, region = "counties", values = "black_prison_pop", include = "TX", color = "blue") +
  scale_fill_continuous(low = "white", high = "blue", name = "Black Prison Population", label = scales::comma) + 
  labs(title = "Texas Black Prison Populations per County", subtitle = "using the most recent year available for each county") +
  theme(legend.position = "right")

