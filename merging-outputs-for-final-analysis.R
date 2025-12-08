# Step 5: Merging data frames (for peak and non-peak)

merged_data_peak <- right_join(route_performance_peak, peak_summary,
                         by = "Route")

merged_data_off_peak <- right_join(route_performance_off_peak, off_summary,
                                  by = "Route")

merged_data <- right_join(route_performance_overall, overall_summary,
                          by = "Route")
