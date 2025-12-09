
#' Add hour buckets and labeled time-of-day to a ridership data frame.
#'
#' @param df Tibble/data.frame with a time column `Time` parseable by mdy_hm().
#' @return Original df plus numeric `hour` and ordered factor `time_of_day`.
add_time_of_day <- function(df) {
  df %>%
    mutate(
      hour = hour(mdy_hm(Time)),
      time_of_day = case_when(
        hour >= 6 & hour < 12  ~ "Morning (6–12)",
        hour >= 12 & hour < 18 ~ "Afternoon (12–6)",
        hour >= 18 & hour < 24 ~ "Evening (6–12)",
        TRUE                   ~ "Night (12–6)"
      ),
      time_of_day = factor(
        time_of_day,
        levels = c("Morning (6–12)", "Afternoon (12–6)",
                   "Evening (6–12)", "Night (12–6)")
      )
    )
}

#' Summarize ridership counts by peak/off-peak and time-of-day bucket.
#'
#' @param ridership_df Tibble of ridership data with Off.Peak flag and Time.
#' @return Tibble with counts per period and time_of_day.
summarize_time_of_day_counts <- function(ridership_df) {
  ridership_df %>%
    add_time_of_day() %>%
    mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
    count(period, time_of_day, name = "riders")
}

#' Build peak vs off-peak total ridership bar plot.
#'
#' @param ridership_df Tibble of ridership data with Off.Peak flag.
#' @return ggplot object.
make_peak_vs_offpeak_plot <- function(ridership_df) {
  ridership_df %>%
    mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
    ggplot(aes(x = period, fill = period)) +
    geom_bar() +
    labs(
      title = "Peak vs Off-Peak Ridership Counts",
      x = NULL, y = "Number of Riders", fill = "Period"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
}

#' Build ridership-by-time-of-day bar plot split by period.
#'
#' @param ridership_df Tibble of ridership data with Off.Peak flag and Time.
#' @return ggplot object.
make_time_of_day_plot <- function(ridership_df) {
  ridership_df %>%
    add_time_of_day() %>%
    mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
    ggplot(aes(x = time_of_day, fill = period)) +
    geom_bar(position = "dodge") +
    labs(
      title = "Ridership by Time of Day",
      x = "Time of Day", y = "Counts", fill = "Period"
    ) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal()
}

#' Build route-by-period bar plot.
#'
#' @param ridership_df Tibble of ridership data with Off.Peak flag and Route.
#' @return ggplot object.
make_route_period_plot <- function(ridership_df) {
  ridership_df %>%
    mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
    ggplot(aes(x = factor(Route), fill = period)) +
    geom_bar(position = "dodge") +
    labs(
      title = "Ridership by Route (Peak vs Off-Peak)",
      x = "Route", y = "Rider Counts", fill = "Period"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

#' Build route × time-of-day heatmap.
#'
#' @param ridership_df Tibble of ridership data with Route and Time columns.
#' @return ggplot heatmap object.
make_route_time_heatmap <- function(ridership_df) {
  ridership_df %>%
    add_time_of_day() %>%
    count(Route, time_of_day) %>%
    ggplot(aes(x = factor(Route), y = time_of_day, fill = n)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(
      title = "Ridership Heatmap: Route × Time of Day",
      x = "Route", y = "Time of Day", fill = "Riders"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}

#' Build peak-period hourly histogram colored by time-of-day bucket.
#'
#' @param peak_df Tibble already filtered to peak period with hour/time_of_day.
#' @return ggplot histogram object.
make_peak_hour_histogram <- function(peak_df) {
  peak_df %>%
    ggplot(aes(x = hour, fill = time_of_day)) +
    geom_histogram(binwidth = 1) +
    labs(
      title = "Distribution of Trip Times (Peak Period)",
      x = "Hour of Day", y = "Count", fill = "Time of Day"
    ) +
    theme_minimal()
}

# Pre-compute peak and off-peak tables with time-of-day classifications
peak <- ridership_data %>% filter(Off.Peak == 0) %>% add_time_of_day()
offpeak <- ridership_data %>% filter(Off.Peak == 1) %>% add_time_of_day()

# Summary tables
time_of_day_summary <- summarize_time_of_day_counts(ridership_data)

# Plots
peak_vs_offpeak_plot <- make_peak_vs_offpeak_plot(ridership_data)
time_of_day_plot <- make_time_of_day_plot(ridership_data)
route_period_plot <- make_route_period_plot(ridership_data)
route_time_heatmap <- make_route_time_heatmap(ridership_data)
peak_hour_histogram <- make_peak_hour_histogram(peak)

## LOOKING @ NUMBERS, WONT USE --------------------------------------------

## peak + peak times of day
p_morning <- peak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 6 & hour < 12)

p_afternoon <- peak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 12 & hour < 18)

p_evening <- peak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 18 & hour < 24)

p_night <- peak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 0 & hour < 6)


## off peak + off peak times of day
op_morning <- offpeak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 6 & hour < 12)

op_afternoon <- offpeak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 12 & hour < 18)

op_evening <- offpeak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 18 & hour < 24)

op_night <- offpeak %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 0 & hour < 6)


## times of day
morning <- ridership_data %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 6 & hour < 12)

afternoon <- ridership_data %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 12 & hour < 18)

evening <- ridership_data %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 18 & hour < 24)

night <- ridership_data %>%
  mutate(hour = hour(mdy_hm(Time))) %>%
  filter(hour > 0 & hour < 6)
  
## times of the week
weekdays <- ridership_data %>%
  subset(Day.of.Week %in% c("Mon","Tue", "Wed", "Thu", "Fri"))
  
weekends <- ridership_data %>%
  subset(Day.of.Week %in% c("Sat","Sun"))


