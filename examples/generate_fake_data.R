library(stringi)

# Set seed for reproducibility
set.seed(123)

# Number of samples
n_samples <- 100

# Generate random names
generate_name <- function(n) {
  first_names <- c("John", "Jane", "Robert", "Emily", "Michael", "Sarah", "William", "Jessica")
  last_names <- c("Smith", "Johnson", "Brown", "Williams", "Jones", "Garcia", "Martinez", "Miller")
  
  paste(sample(first_names, n, replace = TRUE),
        sample(last_names, n, replace = TRUE))
}

# Generate random emails
generate_email <- function(names) {
  user <- tolower(gsub(" ", "_", names))
  domains <- c("@gmail.com", "@yahoo.com", "@hotmail.com", "@outlook.com")
  paste0(user, sample(domains, length(user), replace = TRUE))
}

# Generate dataset
fake_data <- data.frame(
  id = 1:n_samples,
  name = generate_name(n_samples),
  email = generate_email(generate_name(n_samples)),
  dob = as.Date("2000-01-01") + sample(0:8000, n_samples, replace = TRUE),
  salary = round(runif(n_samples, min = 40000, max = 100000), 2)
)

head(fake_data)
