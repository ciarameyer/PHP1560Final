# Step 7: Results (visualizations)
library(tidyverse)
source("~/Documents/GitHub/PHP1560Final/merging-outputs-for-final-analysis.R")


# Identify 5 most on-time and 5 least on-time routes (overall)
top5_on_time <- merged_data %>%
  arrange(desc(Share_on_time)) %>%
  slice_head(n = 5) %>%
  mutate(performance_group = "Most On-Time")

bottom5_on_time <- merged_data %>%
  arrange(Share_on_time) %>%
  slice_head(n = 5) %>%
  mutate(performance_group = "Least On-Time")

# Combine and reshape demographics for plotting
on_time_comparison <- bind_rows(top5_on_time, bottom5_on_time) %>%
  mutate(Route = factor(Route)) %>%
  pivot_longer(
    cols = c(HS, College, Senior, Disabled, Low_Income),
    names_to = "Demographic",
    values_to = "Share"
  )

# Plot: demographic breakdown for the 5 most on-time routes
plot_top5_on_time_demographics <- on_time_comparison %>%
  filter(performance_group == "Most On-Time") %>%
  ggplot(aes(x = Route, y = Share, fill = Demographic)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Demographic Breakdown: 5 Most On-Time Routes",
    x = "Route",
    y = "Share of Riders",
    fill = "Demographic Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Plot: demographic breakdown for the 5 least on-time routes
plot_bottom5_on_time_demographics <- on_time_comparison %>%
  filter(performance_group == "Least On-Time") %>%
  ggplot(aes(x = Route, y = Share, fill = Demographic)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Demographic Breakdown: 5 Least On-Time Routes",
    x = "Route",
    y = "Share of Riders",
    fill = "Demographic Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))