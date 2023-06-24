#' Import All Files in a Directory
#'
#' This function imports all files of the supported types (csv, xls, xlsx, txt, json, rds, sav, por) 
#' from a specified directory. It uses parallel processing for efficient file importing.
#'
#' @param dir_path A character string specifying the directory path. Default is the current working directory.
#' @param pattern An optional regular expression. Only filenames which match the regular expression will 
#'   be returned. Default is NULL, which means all file names are returned.
#' @param list_output A logical. If TRUE, the function returns a named list of data frames, 
#'   where each name corresponds to a file name. If FALSE (default), each data frame is assigned to a variable 
#'   in the global environment with the corresponding file name.
#' @param collapse A character string specifying the column name to join all data frames by. 
#'   If NULL (default), no join operation is performed.
#' 
#' @return If list_output = TRUE, a named list of imported data frames. If list_output = FALSE and collapse is specified, 
#'   a single data frame resulting from joining all imported data frames by the 'collapse' column. 
#'   If list_output = FALSE and collapse is NULL, no explicit return value, but each imported data frame 
#'   is assigned to a variable in the global environment.
#'
#' @importFrom vroom vroom
#' @importFrom readxl read_excel
#' @importFrom jsonlite fromJSON
#' @importFrom haven read_spss
#' @importFrom tools file_ext
#' @importFrom dplyr mutate
#' @importFrom purrr Reduce
#' @importFrom foreach foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' 
#' @examples
#' \dontrun{
#'   # Import all csv files in the directory and return a list of data frames
#'   data_list <- import_all_files(dir_path = "path/to/your/directory", pattern = "\\.csv$", list_output = TRUE)
#'
#'   # Import all csv files in the directory and assign each data frame to a variable in the global environment
#'   import_all_files(dir_path = "path/to/your/directory", pattern = "\\.csv$")
#'
#'   # Import all files in the directory and return a single data frame resulting from joining all data frames by 'id'
#'   data_df <- import_all_files(dir_path = "path/to/your/directory", collapse = "id")
#' }
#' 
#' @note The function assigns a 'system_date' and 'original_file_name' column to each imported data frame.
#' Unsupported file types will generate a warning and be skipped.
#' Errors during file import will generate a warning with the file name and error message, and the file will be skipped.
#' Errors during data frame join operation will generate a message with the error message and return NULL.
#' 
#' @export
# Install packages if needed
required_packages <- c("vroom", "readxl", "jsonlite", "haven", "dplyr", "purrr", "foreach", "doParallel", "tools")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)
# Save function
import_all_files <- function(dir_path = getwd(), pattern = NULL, list_output = FALSE, collapse = NULL) {
  # Check if directory exists
  if (!dir.exists(dir_path)) {
    stop("The directory does not exist.")
  }
  
  # Get a list of files in the directory
  all_files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  
  # Check if directory is empty
  if(length(all_files) == 0) {
    stop("The directory is empty.")
  }
  
  import_file <- function(file) {
    ext <- tools::file_ext(file)
    data <- NULL
    file_name <- basename(file)
    
    tryCatch({
      switch(ext,
             "csv" = {data <- vroom::vroom(file, delim = ",", col_types = "cc")},
             "xls" = {data <- readxl::read_excel(file, guess_max = min(1000, .Machine$integer.max))},
             "xlsx" = {data <- readxl::read_excel(file, guess_max = min(1000, .Machine$integer.max))},
             "txt" = {data <- vroom::vroom(file, col_types = "cc")},
             "json" = {data <- jsonlite::fromJSON(file, flatten = TRUE)},
             "rds" = {data <- readRDS(file)},
             "sav" = {data <- haven::read_spss(file)},
             "por" = {data <- haven::read_spss(file)},
             {warning(paste("Unsupported file type:", ext))})
    }, error = function(e) {
      warning(paste("Error reading file:", file_name, "Error message:", e$message))
    })
    
    if (!is.null(data)) {
      data <- data %>%
        mutate(system_date = Sys.Date(), original_file_name = file_name)
    }
    return(list(file_name, data))
  }
  
  n_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  imported_objects <- foreach(file = all_files, .packages = required_packages) %dopar% {
    import_file(file)
  }
  
  parallel::stopCluster(cl)
  
  names(imported_objects) <- sapply(imported_objects, `[[`, 1)
  imported_objects <- lapply(imported_objects, `[[`, 2)
  
  if (list_output) {
    return(imported_objects)
  }
  
  if (!is.null(collapse)) {
    tryCatch({
      combined_data <- Reduce(function(x, y) {
        full_join(x, y, by = collapse)
      }, imported_objects)
      return(combined_data)
    }, error = function(e) {
      message(paste("Error while collapsing data:", e$message))
      return(NULL)
    })
  }
  
  for(name in names(imported_objects)) {
    assign(name, imported_objects[[name]], envir = .GlobalEnv)
  }
  
  invisible()
}