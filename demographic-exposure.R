# Step 4: Summarizing demographic exposure (function to apply to peak + non-peak)

library(tidyverse)

# REMINDER!! do the roxygen function descriptions

summarize_ridership_demographics <- function(ridership_df) {
  
# classify demographic groups
  classify_groups <- function(df) {
    df %>%
      mutate(
        HS = case_when(
          !is.na(High.School) & High.School != "None" ~ 1,
          TRUE ~ 0
        ),
        Brown_RISD = case_when(
          College != "None" ~ 1,
          TRUE ~ 0
        ),
        Senior = case_when(
          Type == "Senior" ~ 1,
          TRUE ~ 0
        ),
        Disabled = case_when(
          Type == "Disabled" ~ 1,
          TRUE ~ 0
        ),
        Low_Income = Low.Income  # already binary
        )
  }
  
  # summarize by route
  summarize_routes <- function(df) {
    df %>%
      group_by(Route) %>%
      summarise(
        HS = sum(HS, na.rm = TRUE),
        College = sum(Brown_RISD, na.rm = TRUE),
        Senior = sum(Senior, na.rm = TRUE),
        Disabled = sum(Disabled, na.rm = TRUE),
        Low_Income = sum(Low_Income, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # split into peak / off-peak 
  peak_df <- ridership_df %>% filter(Off.Peak == 0)
  offpeak_df <- ridership_df %>% filter(Off.Peak == 1)
  
  # classify demographics
  full_df <- classify_groups(ridership_df)
  peak_df <- classify_groups(peak_df)
  offpeak_df <- classify_groups(offpeak_df)
  
  # summarize by route, raw numbers for visualization
  all_summary <- summarize_routes(full_df)
  peak_summary <- summarize_routes(peak_df)
  offpeak_summary <- summarize_routes(offpeak_df)
  
  # summarize by route (proportion tables)
  all_prop_summary <- summarize_routes(full_df) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(-Route)),
      across(-c(Route, total), ~ .x / total)
    ) %>%
    ungroup() %>%
    select(-total)
  peak_prop_summary <- summarize_routes(peak_df) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(-Route)),
      across(-c(Route, total), ~ .x / total)
    ) %>%
    ungroup() %>%
    select(-total)
  offpeak_prop_summary <- summarize_routes(offpeak_df) %>%
    rowwise() %>%
    mutate(
      total = sum(c_across(-Route)),
      across(-c(Route, total), ~ .x / total)
    ) %>%
    ungroup() %>%
    select(-total)
  

  # make visualizations helper
  make_plot <- function(summary_df, title_text) {
    summary_df %>%
      pivot_longer(cols = -Route, names_to = "Group", values_to = "Count") %>%
      ggplot(aes(x = factor(Route), y = Count, fill = Group)) +
      geom_col(position = "stack") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = title_text,
        x = "Route",
        y = "Ridership Count",
        fill = "Demographic Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  }
  
  peak_plot <- make_plot(peak_summary, "Peak Ridership Demographic Breakdown by Route")
  offpeak_plot <- make_plot(offpeak_summary, "Off-Peak Ridership Demographic Breakdown by Route")
  

  # return tidy output
  tibble(
    overall_summary = list(all_prop_summary),
    peak_summary = list(peak_prop_summary),
    offpeak_summary = list(offpeak_prop_summary),
    plots = list(
      tibble(
        peak_plot = list(peak_plot),
        offpeak_plot = list(offpeak_plot)
      )
    )
  )
}

result <- summarize_ridership_demographics(ridership_data)

overall_summary <- result$overall_summary[[1]]
peak_summary <- result$peak_summary[[1]]
off_summary <- result$offpeak_summary[[1]]

result$plots[[1]]$peak_plot[[1]]
result$plots[[1]]$offpeak_plot[[1]]

# peak vs off peak comparison of single demo. (Seniors)
# NOTE: change to only include the top most-used routes
bind_rows(
  peak_summary  %>% mutate(period = "Peak"),
  off_summary %>% mutate(period = "Off-Peak")
) %>%
  ggplot(aes(x = factor(Route), y = Senior, fill = period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Senior Ridership: Peak vs Off-Peak",
    x = "Route", y = "Count or Proportion", fill = "Period"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
