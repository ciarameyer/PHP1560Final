# Step 3: Summarizing route performance (function to apply to peak and non-peak)
library(tidyverse)

#' Summarize on-time performance by route.
#'
#' @description
#' Categorizes each trip as Early, On Time, or Late using Delay.Sec thresholds
#' (<= -60 early, >= 300 late, otherwise on time), then returns route-level
#' shares of each category.
#'
#' @param data Data frame with columns Route and Delay.Sec (in seconds).
#' @return Tibble with Route and share columns (Share_early, Share_on_time, Share_late).
route_performance <- function(data = otp_data_updated) {
  data %>%
    mutate(
      Delay.Sec = as.numeric(Delay.Sec),
      OnTime = case_when(
        Delay.Sec <= -60 ~ "Early",
        Delay.Sec >= 300 ~ "Late",
        Delay.Sec > -60 & Delay.Sec < 300 ~ "On Time",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(Route) %>%
    summarise(
      Share_early = mean(OnTime == "Early", na.rm = TRUE),
      Share_on_time = mean(OnTime == "On Time", na.rm = TRUE),
      Share_late = mean(OnTime == "Late", na.rm = TRUE),
      .groups = "drop"
    )
}

# Compute performance tables
route_performance_peak <- route_performance(otp_data_peak)
route_performance_overall <- route_performance(otp_data_updated)
route_performance_off_peak <- route_performance(otp_data_off_peak)
