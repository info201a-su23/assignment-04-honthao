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
avg_pop <- floor(mean(state_pop$total_pop))
median_pop <- floor(median(state_pop$total_pop))

# Compute the jail incarceration % per year for different racial groups 15-64
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

# Compute mean jail % of each racial group & get the highest + lowest
mean_per_race <- yearly_rate %>%
  summarize(
    aapi_rate = mean(aapi_pct),
    black_rate = mean(black_pct),
    latinx_rate = mean(latinx_pct),
    native_rate = mean(native_pct),
    white_rate = mean(white_pct)
  )

# Get max jail % - Black
max_rate <- mean_per_race$black_rate

# Get min jail % - AAPI
min_rate <- mean_per_race$aapi_rate

# Compute the % of males & females 15-64 in jail each year
gender_pct <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(
    female_pct = sum(female_jail_pop, na.rm = TRUE) /
      sum(female_pop_15to64, na.rm = TRUE) * 100,
    male_pct = sum(male_jail_pop, na.rm = TRUE) /
      sum(male_pop_15to64, na.rm = TRUE) * 100
  )
male_mean_pct <- mean(gender_pct$male_pct)
female_mean_pct <- mean(gender_pct$female_pct)
gender_ratio <- male_mean_pct / female_mean_pct

# Get the state w/ the highest percentage of the Black population 15-64 that are
# in jail in the most recent year
max_pct_state <- us_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarize(
    pct_in_jail =
      sum(black_jail_pop, na.rm = TRUE) /
      sum(black_pop_15to64, na.rm = TRUE) * 100
  ) %>%
  filter(pct_in_jail == max(pct_in_jail, na.rm = TRUE)) %>%
  pull(state)
max_pct_state <- state.name[match(max_pct_state, state.abb)]
