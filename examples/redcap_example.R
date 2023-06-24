# Install and load the redcapAPI package if it's not already installed
if (!requireNamespace("redcapAPI", quietly = TRUE)) {
  install.packages("redcapAPI")
}
library(redcapAPI)

connect_to_redcap <- function(api_url, api_token) {
  # Check if the API URL and Token are not empty
  if (api_url == "" || api_token == "") {
    cat("Please provide a valid API URL and Token.\n")
    return(NULL)
  }
  
  # Establish the connection to the REDCap project
  redcap_conn <- tryCatch(
    redcapConnection(url = api_url, token = api_token),
    error = function(e) {
      cat("Error connecting to REDCap project. Check your API URL and Token.\n")
      return(NULL)
    }
  )
  
  # Check the connection
  if (!is.null(redcap_conn)) {
    cat("Successfully connected to REDCap project!\n")
  }
  
  return(redcap_conn)
}

api_url <- "https://your_redcap_instance_url/api/"
api_token <- "your_redcap_project_token"

redcap_conn <- connect_to_redcap(api_url, api_token)

# Query data
# Function to fetch data from a REDCap project
fetch_redcap_data <- function(redcap_conn, fields = NULL, filters = NULL) {
  if (is.null(redcap_conn)) {
    cat("The connection to the REDCap project is not established. Please check your connection.\n")
    return(NULL)
  }
  
  # Prepare the arguments for exportRecords()
  export_args <- list(con = redcap_conn)
  
  # Add fields (variables) to the arguments if specified
  if (!is.null(fields) && is.vector(fields)) {
    export_args$fields <- fields
  }
  
  # Add filters (conditions) to the arguments if specified
  if (!is.null(filters) && is.character(filters)) {
    export_args$filters <- filters
  }
  
  # Retrieve the data from the REDCap project
  redcap_data <- tryCatch(
    do.call(exportRecords, export_args),
    error = function(e) {
      cat("Error fetching data from the REDCap project. Check your connection and permissions.\n")
      return(NULL)
    }
  )
  
  if (!is.null(redcap_data)) {
    cat("Successfully fetched data from the REDCap project!\n")
  }
  
  return(redcap_data)
}

# Specify the fields (variables) you want to fetch
fields_to_fetch <- c("field1", "field2", "field3")

# Specify a filter (condition) to apply on the data
filter_condition <- "[field1] = 'some_value'"

# Fetch data from the REDCap project using the established connection
redcap_data <- fetch_redcap_data(redcap_conn, fields = fields_to_fetch, filters = filter_condition)

# Display the data
if (!is.null(redcap_data)) {
  print(redcap_data)
}
