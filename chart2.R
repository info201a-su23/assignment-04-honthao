# Load packages
library("dplyr")
library("ggplot2")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                    stringsAsFactors = FALSE)

# Compute the incarceration rate between males & females
gender_rate <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(
    female_rate = sum(female_jail_pop, na.rm = TRUE) /
      sum(female_pop_15to64, na.rm = TRUE) * 100,
    male_rate = sum(male_jail_pop, na.rm = TRUE) / 
      sum(male_pop_15to64, na.rm = TRUE) * 100
  )

# Scatter plot of female vs male incarceration rate
scatter_chart <- ggplot(data = gender_rate) +
  geom_point(mapping = aes(
    x = female_rate,
    y = male_rate,
    color = year
  )) +
  labs(
    title = "Relationship Between Female and Male Jail Incarceration Rate 1990-2018",
    x = "Female Incarceration Rate",
    y = "Male Incarceration Rate",
  )
