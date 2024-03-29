library(ggplot2)
library(dplyr)
library(tidyr) 
library(maps)

prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv")

# Define the state of focus
selected_state <- "WA"

# Filter data for the selected state and relevant variables
state_data <- prison_data %>%
  filter(state == selected_state) %>%
  select(year, black_jail_pop_rate, white_jail_pop_rate, latinx_jail_pop_rate)

# Convert non-numeric values to NA
state_data <- state_data %>%
  mutate(across(black_jail_pop_rate:latinx_jail_pop_rate, as.numeric))

# Reshape the data into long format
state_data_long <- state_data %>%
  pivot_longer(cols = c(black_jail_pop_rate, white_jail_pop_rate, latinx_jail_pop_rate),
               names_to = "race",
               values_to = "jail_pop_rate") %>%
  filter(!is.na(jail_pop_rate) & jail_pop_rate != 0)  # Exclude NA and 0 values

# Create the trend over time line plot
trends_chart <- ggplot(state_data_long, aes(x = year, y = jail_pop_rate, color = race)) +
  geom_line(size = 1) +
  labs(title = paste("Trends in Jail Population Rates by Race:", selected_state),
       x = "Year",
       y = "Jail Population Rate per 100,000 People",
       color = "Race") +
  scale_color_manual(values = c("black_jail_pop_rate" = "lavender", "latinx_jail_pop_rate" = "yellow", "white_jail_pop_rate" = "pink")) +
  theme_minimal()

# Create the variable comparison chart
variable_chart <- ggplot(prison_data, aes(x = black_jail_pop_rate, y = white_jail_pop_rate)) +
  geom_point(color = "purple", alpha = 0.5) +
  labs(title = "Comparison of Black vs White Jail Population Rates",
       x = "Black Jail Population Rate per 100,000 People",
       y = "White Jail Population Rate per 100,000 People") +
  theme_minimal()

# Create Map

map_data("state")

# Merge your data with the map data based on state names
merged_data <- merge(map_data("state"), prison_data, by.x = "region", by.y = "state", all.x = TRUE)


female_prison_map <- ggplot(merged_data, aes(x = long, y = lat, group = group, fill = female_prison_pop_rate)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "pink", name = "Female Prison Population Rate") +
  labs(title = "Female Prison Population Rate by State") +
  theme_void()