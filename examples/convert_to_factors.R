

is_categorical_df <- function(df) {
  results <- sapply(df, function(col) {
    unique_count <- length(unique(col))
    total_count <- length(col)
    ratio <- unique_count / total_count
    
    if (ratio < 0.95) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  results_df <- data.frame(variable = names(results),
                           categorical = results)
  return(results_df)
}


# Example data set
df <- data.frame(
  col1 = c("A", "B", "C", "D", "E"),
  col2 = c(1, 2, 3, 4, 5),
  col3 = c("dog", "cat", "dog", "bird", "bird"),
  col4 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

# Apply the is_categorical_df function
results <- is_categorical_df(df)
print(results)

convert_to_factors <- function(df) {
  categorical_vars <- is_categorical_df(df)
  categorical_vars <- categorical_vars[categorical_vars$categorical == TRUE,]
  
  df_factors <- df
  
  for (variable in categorical_vars$variable) {
    df_factors[[variable]] <- as.factor(df_factors[[variable]])
  }
  
  return(df_factors)
}



df_factors <- convert_to_factors(df)