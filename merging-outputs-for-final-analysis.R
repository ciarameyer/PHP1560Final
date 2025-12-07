# Step 5: Merging data frames (for peak and non-peak)

merged_data_peak <- full_join(route_performance_results, peak_summary,
                         by = "Route")

merged_data_off_peak <- full_join(route_performance_results, off_summary,
                                  by = "Route")
