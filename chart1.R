# Load packages
library("dplyr")
library("ggplot2")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                    stringsAsFactors = FALSE)

# Compute the incarceration rate per year for different racial groups
jail_pop <- us_jail %>%
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

# Plot the data using line graph
line_chart <- ggplot(data = jail_pop, aes(x = year)) +
  geom_line(mapping = aes(y = aapi_pct, color = "AAPI")) +
  geom_line(mapping = aes(y = black_pct, color = "Black")) +
  geom_line(mapping = aes(y = latinx_pct, color = "Latinx")) +
  geom_line(mapping = aes(y = native_pct, color = "Native")) +
  geom_line(mapping = aes(y = white_pct, color = "White")) +
  labs(
    title = "Jail Incarceration Percentage 1990-2018",
    x = "Year",
    y = "Jail Incarceration Percentage",
  ) +
  scale_colour_manual(name = "Race",
                      breaks = c("AAPI", "Black", "Latinx", "Native", "White"),
                      values = c("orange", "red", "green", "blue", "purple"))
