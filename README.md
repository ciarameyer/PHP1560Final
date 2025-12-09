# PHP1560 Final Project – Ridership & On-Time Performance Analysis

This project analyzes simulated RIPTA ridership and on-time performance data to explore demographic usage, peak vs. off-peak patterns, and route reliability. The workflow is organized into modular R scripts that can be sourced in sequence.

## Data inputs
- `ridership_simulated.csv` – rider demographics and trip times (expects columns like `Time`, `Day.of.Week`, `Route`, `Type`, `College`, `High.School`, `Low.Income`, `Off.Peak`, etc.).
- `otp_simulated.csv` – on-time performance data (expects columns like `Route`, `Scheduled.Time`, `Actual.Arrival.Time`, `Delay.Sec`, plus scheduling fields used for peak/off-peak splits).

Paths default to `~/Downloads/*.csv` in `loading-and-cleaning.R`; change those lines if your files live elsewhere.

## Key scripts (in run order)
1) `loading-and-cleaning.R`  
   - Reads the CSVs, aligns routes across datasets, trims to needed columns.  
   - Builds derived peak/off-peak OTP datasets (`otp_data_peak`, `otp_data_off_peak`) for reuse.

2) `peak-ridership.R`  
   - Adds time-of-day buckets and summarizes ridership by peak vs. off-peak.  
   - Produces plots: peak vs off-peak totals, time-of-day distribution, route-period bars, route × time-of-day heatmap, and peak-hour histogram.

3) `route-performance.R`  
   - Categorizes trips as Early/On Time/Late using `Delay.Sec` thresholds and returns route-level shares.  
   - Computes performance tables for overall, peak, and off-peak OTP subsets.

4) `demographic-exposure.R`  
   - Classifies riders into HS, College, Senior, Disabled, and Low Income.  
   - Summarizes counts/proportions by route and identifies the top 5 busiest routes overall.  
   - Returns plots for peak, off-peak, and the top-5 overall demographic mix.

5) `merging-outputs-for-final-analysis.R`  
   - Joins route performance with demographic summaries (`merged_data`, plus peak/off-peak variants).

6) `correlation-and-hypothesis-tests.R`  
   - Computes correlations and t-tests involving on-time performance shares and demographics.

7) `results.R`  
   - Visualizes demographic breakdowns for the 5 most on-time and 5 least on-time routes using the merged data.

## How to run
From an R session in the project directory:
```r
source("loading-and-cleaning.R")
source("route-performance.R")
source("demographic-exposure.R")
source("merging-outputs-for-final-analysis.R")
source("correlation-and-hypothesis-tests.R")
source("peak-ridership.R")   # optional exploratory visuals
source("results.R")
```

Then print/plot the objects created in each script (e.g., `peak_vs_offpeak_plot`, `route_time_heatmap`, `top5_overall_plot`, `plot_top5_on_time_demographics`, etc.).

## Dependencies
- R packages: `tidyverse`, `lubridate`, `hms`, `scales`, `ggplot2` (loaded in scripts). Install via `install.packages(c("tidyverse","lubridate","hms","scales","ggplot2"))`.