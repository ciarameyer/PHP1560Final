# Step 1: Analyzing for peak ridership time (split into two data sets, peak and non-peak)
# kourtney 
# some exploratory analysis
  # do a little bit of visualizations here
  # find some summary stats too


# look at number of trips for all of then

library(lubridate)
library(tidyverse)

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
      time_of_day = factor(time_of_day, 
                           levels = c("Morning (6–12)", "Afternoon (12–6)",
                                      "Evening (6–12)", "Night (12–6)"))
    )
}

peak <- ridership_data %>% filter(Off.Peak == 0) %>% add_time_of_day()
offpeak <- ridership_data %>% filter(Off.Peak == 1) %>% add_time_of_day()

# peak vs off peak total ridership
ridership_data %>%
  mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
  ggplot(aes(x = period, fill = period)) +
  geom_bar() +
  labs(title = "Peak vs Off-Peak Ridership Counts",
       x = NULL, y = "Number of Riders") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# ridership by time of day (peak vs offpeak)
ridership_data %>%
  add_time_of_day() %>%
  mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
  ggplot(aes(x = time_of_day, fill = period)) +
  geom_bar(position = "dodge") +
  labs(title = "Ridership by Time of Day",
       x = "Time of Day", y = "Counts") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal()

# ridership by route
ridership_data %>%
  mutate(period = if_else(Off.Peak == 0, "Peak", "Off-Peak")) %>%
  ggplot(aes(x = factor(Route), fill = period)) +
  geom_bar(position = "dodge") +
  labs(title = "Ridership by Route (Peak vs Off-Peak)",
       x = "Route", y = "Rider Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# heatmap; route vs time of day
ridership_data %>%
  add_time_of_day() %>%
  count(Route, time_of_day) %>%
  ggplot(aes(x = factor(Route), y = time_of_day, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Ridership Heatmap: Route × Time of Day",
       x = "Route", y = "Time of Day", fill = "Riders") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# distribution of peak start times
peak %>% 
  ggplot(aes(x = hour, fill = time_of_day)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Trip Times (Peak Period)",
       x = "Hour of Day", y = "Count") +
  theme_minimal()

# ciara: splitting otp data by peak and non-peak times
# via RIPTA website peak is 7am-9am and 3pm-6pm on weekdays

split_time <- str_split_fixed(otp_data_updated$Scheduled.Time, " ", 2)
otp_data_updated$Scheduled_Date <- split_time[,1]
otp_data_updated$Scheduled_Time <- split_time[,2]
otp_data_updated$Scheduled_Date <- weekdays(as.Date(otp_data_updated$Scheduled_Date))
otp_data_updated <- otp_data_updated %>%
  filter(Scheduled_Time != "" & !is.na(Scheduled_Time)) %>%
  mutate(Scheduled_Time = as_hms(Scheduled_Time))
otp_data_peak <- otp_data_updated %>% 
  filter(Scheduled_Date != "Saturday" & Scheduled_Date != "Sunday") %>% 
  filter(between(Scheduled_Time, as_hms("07:00:00"), as_hms("09:00:00")) | 
           between(Scheduled_Time, as_hms("15:00:00"), as_hms("18:00:00")))
otp_data_off_peak <- otp_data_updated %>% 
  filter(Scheduled_Date == "Saturday" | Scheduled_Date == "Sunday" | 
           (Scheduled_Date != "Saturday" & Scheduled_Date != "Sunday" & 
              (Scheduled_Time < as_hms("07:00:00") |  
                 between(Scheduled_Time, as_hms("09:00:00"), as_hms("15:00:00")) |
                           Scheduled_Time > as_hms("18:00:00"))))




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


