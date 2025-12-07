# Step 0: loading & cleaning datasets 
# Step 2: Mutating columns of simulated ridership data

library(tidyverse)
library(ggplot2)

# ridership_data <- read.csv("~/Downloads/ridership_simulated.csv")
# otp_data <- read.csv("~/Downloads/otp_simulated.csv")
ridership_data <- read.csv("~/Desktop/PHP1560/final.data/ridership_simulated.csv")
otp_data <- read.csv("~/Desktop/PHP1560/final.data/otp_simulated (2).csv")

# arranging in order of routes
otp_data_updated <- otp_data %>% 
  arrange(Route) 

# filtering so that we only include routes that are in both datasets
ridership_data <- ridership_data %>%
  filter(Route %in% unique(otp_data_updated$Route))

# checking to make sure filtering worked!
unique(otp_data_updated$Route)
unique(ridership_data$Route)

# mutating OTP data to only include needed variables 
otp_data_updated <- subset(otp_data, select = - c(Driver.ID, Trip, Stop, Stop.Sequence, 
                                          StopLat, StopLng)) 

# mutating ridership data to only include needed variables 
ridership_data_updated <- subset(ridership_data, select = c(Time, Day.of.Week, Route, 
                                                    Type, College,
                                                    High.School, Low.Income))

ridership_data_updated <- ridership_data_updated  %>%
  mutate(Child = if_else(Type == "Child", 1, 0)) %>%
  mutate(Senior = if_else(Type == "Senior", 1, 0)) %>%
  mutate(Disabled = if_else(Type == "Disabled", 1, 0)) %>%
  mutate(Brown_RISD = if_else(College == "Brown" | College ==  "RISD", 1, 0))  %>%
  mutate(Other_College = if_else(College != "None" & College != "Brown" &
                                   College != "RISD", 1, 0)) %>%
  mutate(High_School = if_else(High.School != "None", 1, 0))

ridership_data_updated  <- subset(ridership_data_updated, select = c(Time, Day.of.Week, Route, 
                                                    Low.Income, Child, Senior, 
                                                    Disabled, Brown_RISD, 
                                                    Other_College, High_School))

# visualize percentage of overall riders who fit categories

for_vis_ridership <- ridership_data_updated %>%
  pivot_longer(cols = c(Child, Senior, Disabled, Low.Income, 
                        High_School, Brown_RISD, Other_College), 
               names_to = "Demographics", 
               values_to = "Yes_or_No")

plot_data <- for_vis_ridership %>%
  group_by(Demographics) %>%
  summarize(yes_rate = mean(Yes_or_No == 1))

plot <- ggplot(plot_data) +
  geom_col(aes(x = Demographics, y = yes_rate), fill = "#FFB3FF") +
  theme_minimal() +
  labs(title = "Demographic Breakdown of RIPTA Riders", x = " ", y = 
         "Share of Riders") +
  scale_x_discrete(labels = c("Brown-RISD \nStudents", "Child", "Disabled", 
                              "High School \nStudents", "Low Income", 
                              "Other College \nStudents", "School")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), fill) +
  scale_y_continuous(labels = c("0", "5%", "10%", "15%"),
                     breaks = c(0.0, 0.05, 0.1, 0.15)) 

ggsave("demographic_ridership.png", plot = plot, width = 6, height = 4, 
       dpi = 300)