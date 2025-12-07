# Step 3: Summarizing route performance (function to apply to peak and non-peak)
#' @description this function takes in data with bus routes and the difference 
#' between their scheduled arrival times and actual arrival times, categorizes 
#' them as on time, early, or late and then summarizes the percentage of the
#' time each route is on time, early, or late 
#' @param a dataframe with data documenting each trip in a bus system over a 
#' set period of time, with variables for the route number and difference 
#' between scheduled and actual arrival time
#' @return a dataframe with a column for each route number and the share of 
#' trips that were on time, late, or early

route_performance <- function(data = otp_data_updated){
  
  data <- data %>%
    mutate(OnTime = case_when(Delay.Sec <= -60 ~ "Early",
                               Delay.Sec >= 300 ~ "Late", 
                               Delay.Sec > -60 & Delay.Sec < 300 ~  "On Time")
           ) 
  
  route_performance <- data %>%
    group_by(Route) %>%
    summarize(
      Share_early = mean(OnTime == "Early"),
      Share_on_time = mean(OnTime == "On Time"),
      Share_late = mean(OnTime == "Late")
    )
              
}

route_performance_results <- route_performance(otp_data_updated)
  