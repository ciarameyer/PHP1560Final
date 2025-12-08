# Step 6: Correlation and hypothesis tests

correlations <- function(df, threshold = 0.6, use = "everything"){
  # results data frame
  res <- data.frame(name1 = vector("character"),
                    name2 = vector("character"),
                    cor = vector("numeric"))
  
  # get rid of any NaN
  df <- select_if(df, is.numeric)
  
  # find correlations 
  cor_mat <- cor(df, use = use)
  
  # find pairs with high correlations 
  n <- nrow(cor_mat)
  for( i in 1:(n-1)){
    for(j in (i + 1):n){
      if(!is.na(cor_mat[i, j]) && abs(cor_mat[i, j]) > threshold){
        res <- add_row(res,
                       name1 = colnames(cor_mat)[i],
                       name2 = colnames(cor_mat)[j],
                       cor = cor_mat[i,j])
      }
    }
  }
  res <- res %>%
    filter(name1 == "Share_on_time" | name2 == "Share_on_time" | 
             name1 == "Share_early" | name2 == "Share_early" |
             name1 == "Share_late" | name2 == "Share_late") %>%
    filter(name1 != "Route" & name2 != "Route")
  return(res)
}

correlation_data_peak <- correlations(df = merged_data_peak, threshold = 0.4, 
                                      use = "everything")
correlation_data_off_peak <- correlations(df = merged_data_off_peak,
                                          threshold = 0.4, use = "everything")
correlation_data_overall <- correlations(df = merged_data,
                                          threshold = 0.4, use = "everything")

t_tests <- function(df, threshold = 0.05, use = "everything"){
  # results data frame
  res <- data.frame(name1 = vector("character"),
                    name2 = vector("character"),
                    t_test = vector("numeric"))
  
  n <- ncol(df)
  # find t tests and pairs with statistically significant t tests
  for( i in 1:(n-1)){
    for(j in (i + 1):n){
      test_result <- t.test(df[[i]], df[[j]])
     if(test_result$p.value < threshold){
        res <- rbind(res, data.frame(
                       name1 = colnames(df)[i],
                       name2 = colnames(df)[j],
                       t_test = test_result$p.value))
      }
    }
  }
  res <- res %>%
    filter(name1 == "Share_on_time" | name2 == "Share_on_time" | 
             name1 == "Share_early" | name2 == "Share_early" |
             name1 == "Share_late" | name2 == "Share_late") %>%
    filter(name1 != "Route" & name2 != "Route")
  return(res)
}

t_tests_peak <- t_tests(merged_data_peak)
t_tests_off_peak <- t_tests(merged_data_off_peak)
t_tests_overall <- t_tests(merged_data)