classify_columns <- function(df) {
  column_types <- c()
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      column_types[col] <- "numeric"
    } else if (is.factor(df[[col]])) {
      column_types[col] <- "categorical"
    } else if (is.character(df[[col]])) {
      column_types[col] <- "character"
    } else {
      is_date <- tryCatch(
        {
          as.Date(df[[col]])
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )
      if (is_date) {
        column_types[col] <- "date"
      } else {
        # Try to estimate the likelihood of each unknown column
        # fitting one of the other categories
        values <- as.character(df[[col]])
        if (all(grepl("[0-9]+", values))) {
          column_types[col] <- "numeric"
        } else if (length(unique(values)) / nrow(df) < 0.05) {
          column_types[col] <- "categorical"
        } else {
          column_types[col] <- "unknown"
        }
      }
    }
  }
  return (column_types)
}



set.seed(123)
df <- data.frame(
  numeric_col = rnorm(100),
  categorical_col = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
  character_col = sample(LETTERS, 100, replace = TRUE),
  date_col = as.Date("2022-01-01") + 1:100,
  unknown_col = c(rnorm(50), sample(c("X", "Y", "Z"), 50, replace = TRUE))
)


classify_columns(df)

