# VARIABLE COMPARISON CHART

# Load packages
library("dplyr")
library("ggplot2")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                    stringsAsFactors = FALSE)

# Compute the percentage of males & females 15-64 in jail each year
gender_pct <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(
    female_pct = sum(female_jail_pop, na.rm = TRUE) /
      sum(female_pop_15to64, na.rm = TRUE) * 100,
    male_pct = sum(male_jail_pop, na.rm = TRUE) /
      sum(male_pop_15to64, na.rm = TRUE) * 100
  )

# Scatter plot of the percentage data
scatter_chart <- ggplot(data = gender_pct) +
  geom_point(mapping = aes(
    x = female_pct,
    y = male_pct,
    color = year
  )) +
  labs(
    title = "Percentage of Females vs Males Age 15-64 in Jail, 1990-2018",
    x = "Female Incarceration Percentage",
    y = "Male Incarceration Percentage",
  )
