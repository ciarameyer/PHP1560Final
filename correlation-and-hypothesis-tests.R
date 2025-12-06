# Step 6: Correlation and hypothesis tests

correlations <- function(merged_data, threshold = 0.6){
  # results data frame
  res <- data.frame(name1 = vector("character"),
                    name2 = vector("character"),
                    cor = vector("numeric"))
  # find correlations 
  cor_mat <- cor.test(merged_data)
  
  # find pairs with high correlations 
  n <- nrow(cor_mat)
  for( i in 1(n-1)){
    for(j in (i + 1):n){
      if(abs(cor_mat[i,j]) > threshold){
        res <- add_row(res,
                       name1 = colnames(cor_mat)[i],
                       name2 = colnames(cor_mat)[j],
                       cor = cor_mat[i,j])
      }
    }
  }
  return(res)
}

correlation_data <- correlations(merged_data)

t_tests <- function(merged_data, threshold = 0.05){
  # results data frame
  res <- data.frame(name1 = vector("character"),
                    name2 = vector("character"),
                    t_test = vector("numeric"))
  
  # find t tests
  t_mat <- t.test(merged_data)
  
  # find pairs with statistically significant t tests
  n <- nrow(t_mat)
  for( i in 1(n-1)){
    for(j in (i + 1):n){
      if(abs(t_mat[i,j]) < threshold){
        res <- add_row(res,
                       name1 = colnames(t_mat)[i],
                       name2 = colnames(t_mat)[j],
                       t_test = t_mat[i,j])
      }
    }
  }
  return(res)
}

t_tests_data <- t_tests(merged_data)