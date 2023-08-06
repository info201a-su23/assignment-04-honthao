# Load packages
library("dplyr")
library("ggplot2")
library("maps")
library("mapproj")

# Load data about jail population per county/state
us_jail <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-jail-pop.csv?raw=true",
                    stringsAsFactors = FALSE)

state_jail_pop <- us_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  group_by(state) %>%
  summarize(
    total_jail_pop =
      sum(black_jail_pop, na.rm = TRUE) /
      sum(black_pop_15to64, na.rm = TRUE) * 100
  )

# Load state shapefile into `state_shape`
state_shape <- map_data("state")

# Convert state in `state_jail_pop` from abbr to full name to be consistent
# w/ `state_shape`
state_jail_pop <- state_jail_pop %>%
  mutate(state_full_name = tolower(state.name[match(state, state.abb)]))

# Join `state_jail_pop` and `state_shape`
state_jail_pop_shape <- left_join(
  state_jail_pop,
  state_shape,
  by = c("state_full_name" = "region")
)

# Plot Black incarceration rate geographically
pop_map <- ggplot(data = state_jail_pop_shape) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = total_jail_pop)
  ) +
  scale_fill_continuous(
    low = "grey",
    high = "red",
  ) +
  labs(
    title = "Black Jail Incarceration Rate in the USA 1990-2018",
    fill = "Incarceration Rate"
  ) +
  coord_map()
