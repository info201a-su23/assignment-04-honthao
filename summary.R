# SUMMARY INFORMATION

# Load packages
library("dplyr")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                   stringsAsFactors = FALSE)

# Calculate total jail pop across states in the most recent year to find avg
state_pop <- us_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarize(total_pop = sum(total_jail_pop, na.rm = TRUE))
avg_pop <- mean(state_pop$total_pop)
median_pop <- median(state_pop$total_pop)

# Compute the jail incarceration rate per year for different racial groups
yearly_rate <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(
    aapi_pct = sum(aapi_jail_pop, na.rm = TRUE) /
      sum(aapi_pop_15to64, na.rm = TRUE) * 100,
    black_pct = sum(black_jail_pop, na.rm = TRUE) /
      sum(black_pop_15to64, na.rm = TRUE) * 100,
    latinx_pct = sum(latinx_jail_pop, na.rm = TRUE) /
      sum(latinx_pop_15to64, na.rm = TRUE) * 100,
    native_pct = sum(native_jail_pop, na.rm = TRUE) /
      sum(native_pop_15to64, na.rm = TRUE) * 100,
    white_pct = sum(white_jail_pop, na.rm = TRUE) /
      sum(white_pop_15to64, na.rm = TRUE) * 100
  )

# Compute mean jail rate of each racial group & get the highest & lowest
mean_per_race <- yearly_rate %>%
  summarize(
    aapi_rate = mean(aapi_pct),
    black_rate = mean(black_pct),
    latinx_rate = mean(latinx_pct),
    native_rate = mean(native_pct),
    white_rate = mean(white_pct))

# Get max jail rate - Black
max_rate = mean_per_race$black_rate

# Get min jail rate - AAPI
min_rate = mean_per_race$aapi_rate

# Compute the incarceration ratio between males & females
gender_rate <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(
    female_rate = sum(female_jail_pop, na.rm = TRUE) /
      sum(female_pop_15to64, na.rm = TRUE) * 100,
    male_rate = sum(male_jail_pop, na.rm = TRUE) / 
      sum(male_pop_15to64, na.rm = TRUE) * 100
  )
male_mean_rate <- mean(gender_rate$male_rate)
female_mean_rate <- mean(gender_rate$female_rate)
gender_ratio <- male_mean_rate / female_mean_rate

# Compute the state w/ max Black incarceration rate in the most recent year
max_rate_state <- us_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarize(total_jail_pop =
              sum(black_jail_pop, na.rm = TRUE) /
              sum(black_pop_15to64, na.rm = TRUE) * 100
  ) %>%
  filter(total_jail_pop == max(total_jail_pop)) %>%
  pull(state)
max_rate_state <- state.name[match(max_rate_state, state.abb)]
