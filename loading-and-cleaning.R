# Step 0: loading & cleaning datasets 
# Step 2: Mutating columns of simulated ridership data

library(tidyverse)
library(ggplot2)

ridership_data <- read.csv("~/Desktop/PHP1560/final.data/ridership_simulated.csv")
otp_data <- read.csv("~/Desktop/PHP1560/final.data/otp_simulated (2).csv")

# arranging in order of routes
otp_data <- otp_data %>% 
  arrange(Route) 

# filtering so that we only include routes that are in both datasets
ridership_data <- ridership_data %>%
  filter(Route %in% unique(otp_data$Route))

# checking to make sure filtering worked!
unique(otp_data$Route)
unique(ridership_data$Route)

# mutating OTP data to only include needed variables 
otp_data <- subset(otp_data, select = - c(Driver.ID, Stop, Stop.Sequence, 
                                          StopLat, StopLng)) 

# mutating ridership data to only include needed variables 
ridership_data <- subset(ridership_data, select = c(Time, Day.of.Week, Route, 
                                                    Type, College,
                                                    High.School, Low.Income))

ridership_data <- ridership_data %>%
  mutate(Child = if_else(Type == "Child", 1, 0)) %>%
  mutate(Senior = if_else(Type == "Senior", 1, 0)) %>%
  mutate(Disabled = if_else(Type == "Disabled", 1, 0)) %>%
  mutate(Brown-RISD = if_else(College == "Brown" | "RISD", 1, 0)))
  

