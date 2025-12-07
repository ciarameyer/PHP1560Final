# Step 5: Merging data frames (for peak and non-peak)

merged_data <- full_join(route_performance_results, demographic_exposure_results,
                         by = "Route")