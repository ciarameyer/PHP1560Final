# Step 3: Summarizing route performance (function to apply to peak and non-peak)

route_performance <- function(data = otp_data){
  
  otp_data <- otp_data %>%
    mutate(OnTime = case_when(Delay.Sec <= -60 ~ "Early",
                               Delay.Sec >= 300 ~ "Late", 
                               Delay.Sec > -60 & Delay.Sec < 300 ~  "On Time")
           ) 
  
  performance <- otp_data %>%
    group_by(Route) %>%
    summarize(share_early == sum(Delay.Sec == "Early")/n(),
              share_on_time == sum(Delay.Sec == "On Time")/n(),
              share_late == sum(Delay.Sec == "Late")/n())
    
}
  
  