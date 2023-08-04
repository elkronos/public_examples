#' Retrieves a ZCTA crosswalk as a tibble
#'
#' @seealso https://rb.gy/h0l5cs
#' @importFrom readr read_delim
#' @importFrom dplyr rename select mutate filter
#' @importFrom rlang .data
#' @importFrom stringr str_sub
#' @export
#' @return A tibble representing each (zcta, county, state) combination.
get_zcta_crosswalk <- function() {
  url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"
  
  # Load the data and rename columns
  read_delim(file = url, delim = "|") %>%
    rename(
      zcta = .data$GEOID_ZCTA5_20,
      county_fips = .data$GEOID_COUNTY_20,
      county_name = .data$NAMELSAD_COUNTY_20
    ) %>%
    select(.data$zcta, .data$county_fips, .data$county_name) %>%
    mutate(state_fips = str_sub(.data$county_fips, 1, 2)) %>%
    filter(!is.na(.data$zcta))
}

#' Returns the ZCTAs based on given parameters
#'
#' @param identifiers A vector of identifiers (counties, states, or ZCTAs)
#' @param type The type of identifiers (can be "county", "state", or "metadata")
#' @importFrom utils data
#' @importFrom dplyr pull filter
#' @export
#' @return A vector of ZCTAs or a tibble of ZCTA metadata based on requested type.
get_zctas_by_identifier <- function(identifiers, type) {
  data("zcta_crosswalk_data", package = "zctaCrosswalk", envir = environment())
  
  # Convert identifiers to lowercase
  identifiers <- tolower(identifiers)
  
  # Choose the column based on the type of input
  col <- switch(type,
                county = ifelse(all(identifiers %in% zcta_crosswalk_data$county_fips), "county_fips", "county_fips_numeric"),
                state = if(all(identifiers %in% tolower(zcta_crosswalk_data$state_name))) {
                  "state_name"
                } else if (all(identifiers %in% tolower(zcta_crosswalk_data$state_usps))) {
                  "state_usps"
                } else if (all(identifiers %in% zcta_crosswalk_data$state_fips)) {
                  "state_fips"
                } else if (all(identifiers %in% zcta_crosswalk_data$state_fips_numeric)) {
                  "state_fips_numeric"
                }
  )
  
  # Validate input and process accordingly
  if (is.null(col) && type != "metadata") {
    stop(sprintf("Invalid '%s' type provided. Please provide valid identifiers.", type))
  } else if (type == "metadata") {
    if (!all(identifiers %in% zcta_crosswalk_data$zcta)) {
      stop("Invalid ZCTAs provided. Please provide valid ZCTAs.")
    }
    return(filter(zcta_crosswalk_data, .data$zcta %in% identifiers))
  } else {
    message(sprintf("Using column '%s'", col))
    return(zcta_crosswalk_data %>%
             filter(get(col) %in% identifiers) %>%
             pull(.data$zcta) %>%
             unique())
  }
}