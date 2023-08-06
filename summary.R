# SUMMARY INFORMATION

# Load packages
library("dplyr")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                   stringsAsFactors = FALSE)

# Calculate total jail population per year to find the mean & median
yearly_pop <- us_jail %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(total_pop = sum(total_jail_pop, na.rm = TRUE))

avg_pop <- mean(yearly_pop$total_pop)
median_pop <- median(yearly_pop$total_pop)
