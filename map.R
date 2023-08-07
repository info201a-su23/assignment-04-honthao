# MAP

# Load packages
library("dplyr")
library("ggplot2")
library("maps")
library("mapproj")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                    stringsAsFactors = FALSE)

# Compute the percentage of the Black population 15-64 that are in jail for each
# state in the most recent year
jail_state_pct <- us_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarize(
    pct_in_jail =
      sum(black_jail_pop, na.rm = TRUE) /
      sum(black_pop_15to64, na.rm = TRUE) * 100
  )

# Load state shapefile into `state_shape`
state_shape <- map_data("state")

# Convert state in `jail_state_pct` from abbr to full name & lowercase to be
# consistent w/ `state_shape`
jail_state_pct <- jail_state_pct %>%
  mutate(state_full_name = tolower(state.name[match(state, state.abb)]))

# Join `jail_state_pct` and `state_shape`
jail_state_pct_shape <- left_join(
  jail_state_pct,
  state_shape,
  by = c("state_full_name" = "region")
)

# Plot Black incarceration % geographically
pop_map <- ggplot(data = jail_state_pct_shape) +
  geom_polygon(
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = pct_in_jail
    )
  ) +
  scale_fill_continuous(
    low = "grey",
    high = "red",
  ) +
  labs(
    title = "Percentage of Black Population Age 15-64 in Jail in the USA, 2018",
    fill = "Incarceration %"
  ) +
  coord_map()
