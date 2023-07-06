#' Retrieve Quarterly Workforce Indicators (QWI) data for specified years, states, and endpoints.
#'
#' This function retrieves QWI data for specified years, states, and endpoints using the Census API. 
#' The data is returned as a list with each element containing the data for one endpoint.
#'
#' Get a Census API key for free by signing up here: https://api.census.gov/data/key_signup.html
#'
#' @param start_year Numeric: The first year for which to retrieve data.
#' @param end_year Numeric: The last year for which to retrieve data.
#' @param states Character vector: State abbreviations for which to retrieve data. Defaults to all U.S. states if not specified.
#' @param endpoints Character vector: Endpoints to retrieve. The possible values are "sa", "se", and "rh". 
#' @param census_key Character string: Your Census API key.
#' 
#' @return A list containing the QWI data for each endpoint.
#' 
#' @examples
#' # Retrieve QWI data for all states and all endpoints for 2010 and 2011
#' 
#' qwi_save <- query_qwi(start_year = 2010, end_year = 2011, census_key = "YOUR_CENSUS_KEY_HERE")
#' 
#' @export
library(tidyqwi)
query_qwi <- function(start_year, end_year, states = NULL, endpoints = c("sa", "se", "rh"), census_key) {
  if(is.null(states)) {
    states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                "UT", "VT", "VA", "WA"," WV", "WI", "WY")
  }
  
  # Check for valid years
  if(!is.numeric(start_year) | !is.numeric(end_year)) {
    stop("start_year and end_year must be numeric")
  }
  
  if(start_year > end_year) {
    stop("start_year must be less than or equal to end_year")
  }
  
  # Check for valid states
  valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                    "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                    "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                    "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  if(!all(states %in% valid_states)) {
    stop("Invalid state abbreviation(s) detected. Please provide valid U.S. state abbreviations.")
  }
  
  # Check for valid endpoints
  valid_endpoints <- c("sa", "se", "rh")
  if(!all(endpoints %in% valid_endpoints)) {
    stop("Invalid endpoint(s) detected. Valid endpoints are 'sa', 'se', and 'rh'.")
  }
  
  years <- as.character(start_year:end_year)
  
  qwi_data_list <- lapply(endpoints, function(endpoint) {
    endpoint_data_list <- lapply(years, function(year) {
      qwi_data <- get_qwi(
        years = year,
        states = states,
        geography = "county",
        apikey = census_key,
        endpoint = endpoint,
        variables = c("sEmp", "Emp"),
        all_groups = FALSE,
        industry_level = "2",
        processing = "sequential"
      )
      endpoint_data <- data.frame(year, state = states, qwi_data)
      return(endpoint_data)
    })
    
    endpoint_data <- do.call(rbind, endpoint_data_list)
    return(endpoint_data)
  })
  
  names(qwi_data_list) <- endpoints
  return(qwi_data_list)
}

# Save flattened file
# Load the necessary package
library(dplyr)

## Add an identifier column
# qwi_save$sa$identifier <- 'sa'
# qwi_save$se$identifier <- 'se'
# qwi_save$rh$identifier <- 'rh'
# 
## Bind the rows
# flattened_df <- bind_rows(qwi_save$sa, qwi_save$se, qwi_save$rh)
# 