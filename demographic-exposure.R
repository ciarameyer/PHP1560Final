# Step 4: Summarizing demographic exposure (function to apply to peak + non-peak)

library(tidyverse)

#' Summarize demographic ridership by route (peak, off-peak, and overall).
#'
#' @description
#' Builds counts and proportional summaries for key demographic groups by route,
#' identifies the five busiest routes overall, and returns ready-to-plot ggplot
#' objects for peak, off-peak, and overall top-route views.
#'
#' @param ridership_df tibble/data.frame of ridership records with columns
#'   Route, Off.Peak, High.School, College, Type, and Low.Income.
#'
#' @return A tibble with list-columns:
#'   - overall_summary: proportion table by route (all riders)
#'   - peak_summary: proportion table for peak riders
#'   - offpeak_summary: proportion table for off-peak riders
#'   - top_routes: character vector of the five busiest routes
#'   - top5_demographic_usage: tidy long-format proportions for the top routes
#'   - plots: tibble of ggplots (peak_plot, offpeak_plot, top5_overall_plot)

summarize_ridership_demographics <- function(ridership_df) {
  # classify demographic groups

  # summarize demographic counts by route
  summarize_routes <- function(df) {
    df %>%
      group_by(Route) %>%
      summarise(
        High_School = sum(High_School, na.rm = TRUE),
        Brown_RISD = sum(Brown_RISD, na.rm = TRUE),
        Senior = sum(Senior, na.rm = TRUE),
        Disabled = sum(Disabled, na.rm = TRUE),
        Low_Income = sum(Low_Income, na.rm = TRUE),
        Other_College = sum(Other_College, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # split into peak / off-peak subsets
  peak_df <- ridership_df %>% filter(Off.Peak == 0)
  offpeak_df <- ridership_df %>% filter(Off.Peak == 1)

  # summarize by route (counts for visuals)
  all_summary <- summarize_routes(ridership_df)
  peak_summary <- summarize_routes(peak_df)
  offpeak_summary <- summarize_routes(offpeak_df)

  # summarize by route (proportions)
  all_prop_summary <- summarize_routes(ridership_df) %>%
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

  # find top 5 routes overall by total ridership (all riders)
  top_routes <- ridership_df %>%
    count(Route, name = "total_rides") %>%
    slice_max(total_rides, n = 5, with_ties = FALSE) %>%
    pull(Route)

  # overall demographic mix for the top 5 routes (long format for plotting)
  top5_demographic_usage <- all_prop_summary %>%
    filter(Route %in% top_routes) %>%
    mutate(Route = factor(Route, levels = top_routes)) %>%
    pivot_longer(cols = -Route, names_to = "Group", values_to = "Proportion")

  # reusable stacked bar plot for counts
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
  top5_overall_plot <- top5_demographic_usage %>%
    ggplot(aes(x = Route, y = Proportion, fill = Group)) +
    geom_col(position = "stack") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Top 5 Routes: Demographic Usage (Overall)",
      x = "Route",
      y = "Share of riders",
      fill = "Demographic Group"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

  # return tidy output
  tibble(
    overall_summary = list(all_prop_summary),
    peak_summary = list(peak_prop_summary),
    offpeak_summary = list(offpeak_prop_summary),
    top_routes = list(top_routes),
    top5_demographic_usage = list(top5_demographic_usage),
    plots = list(
      tibble(
        peak_plot = list(peak_plot),
        offpeak_plot = list(offpeak_plot),
        top5_overall_plot = list(top5_overall_plot)
      )
    )
  )
}

result <- summarize_ridership_demographics(ridership_data_updated)

overall_summary <- result$overall_summary[[1]]
peak_summary <- result$peak_summary[[1]]
off_summary <- result$offpeak_summary[[1]]
top_routes <- result$top_routes[[1]]
top5_usage <- result$top5_demographic_usage[[1]]

result$plots[[1]]$peak_plot[[1]]
result$plots[[1]]$offpeak_plot[[1]]
result$plots[[1]]$top5_overall_plot[[1]]
