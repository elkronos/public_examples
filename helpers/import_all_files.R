###############################################################################
# Required Packages: Install (if needed) and load the required packages.
###############################################################################

required_packages <- c("vroom", "readxl", "jsonlite", "haven", 
                       "dplyr", "purrr", "foreach", "doParallel", "tools")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}
invisible(lapply(required_packages, require, character.only = TRUE))


###############################################################################
# Helper Functions
###############################################################################

#' Check Directory Validity
#'
#' @param dir_path Character string. Directory to check.
#' @return Invisibly returns TRUE if the directory exists and is not empty,
#'   otherwise stops with an error.
check_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  files <- list.files(dir_path, full.names = TRUE)
  if (length(files) == 0) {
    stop("Directory is empty: ", dir_path)
  }
  invisible(TRUE)
}

#' List Files in Directory
#'
#' @param dir_path Character string. Directory path.
#' @param pattern Optional regular expression; only files matching the pattern are returned.
#' @return A character vector of file paths.
list_files_in_directory <- function(dir_path, pattern = NULL) {
  files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    warning("No files found matching pattern in directory: ", dir_path)
  }
  return(files)
}

#' Read a Single File Based on Extension
#'
#' This helper function reads one file (csv, txt, xls, xlsx, json, rds, sav, por)
#' and adds two extra columns: system_date and original_file_name.
#'
#' @param file Character string. Full path to the file.
#' @return A named list with elements \code{file_name} and \code{data} (the imported data frame).
read_file <- function(file) {
  ext <- tolower(tools::file_ext(file))
  file_name <- basename(file)
  
  # Pre-check: Only proceed if the file extension is supported.
  supported_extensions <- c("csv", "txt", "xls", "xlsx", "json", "rds", "sav", "por")
  if (!(ext %in% supported_extensions)) {
    warning("Unsupported file type: ", ext, " in file: ", file_name)
    return(list(file_name = file_name, data = NULL))
  }
  
  data <- tryCatch({
    switch(ext,
           "csv"  = vroom::vroom(file, delim = ",", show_col_types = FALSE),
           "txt"  = vroom::vroom(file, delim = "\t", show_col_types = FALSE),
           "xls"  = readxl::read_excel(file),
           "xlsx" = readxl::read_excel(file),
           "json" = jsonlite::fromJSON(file, flatten = TRUE),
           "rds"  = readRDS(file),
           "sav"  = haven::read_spss(file),
           "por"  = haven::read_spss(file)
    )
  }, error = function(e) {
    warning("Error reading file: ", file_name, ". Message: ", e$message)
    NULL
  })
  
  if (!is.null(data)) {
    data <- dplyr::mutate(data,
                          system_date = Sys.Date(),
                          original_file_name = file_name)
  }
  return(list(file_name = file_name, data = data))
}

#' Combine a List of Data Frames by a Common Column
#'
#' @param data_list A list of data frames.
#' @param by_col Character string. The column name to join on.
#' @param join_method Character string. One of "full_join", "inner_join", "left_join", or "right_join".
#' @return A single data frame resulting from joining all data frames.
combine_data_frames <- function(data_list, by_col, join_method = "full_join") {
  join_func <- switch(join_method,
                      full_join = dplyr::full_join,
                      inner_join = dplyr::inner_join,
                      left_join  = dplyr::left_join,
                      right_join = dplyr::right_join,
                      stop("Invalid join_method. Choose from full_join, inner_join, left_join, or right_join.")
  )
  
  missing_join <- sapply(data_list, function(df) !is.null(df) && !(by_col %in% colnames(df)))
  if (any(missing_join)) {
    warning("Not all data frames have the join column: ", by_col, ". Skipping those files.")
    data_list <- data_list[!missing_join]
  }
  
  if (length(data_list) == 0) {
    stop("No data frames with the join column '", by_col, "' available for merging.")
  }
  
  combined <- tryCatch({
    Reduce(function(x, y) join_func(x, y, by = by_col), data_list)
  }, error = function(e) {
    message("Error while collapsing data: ", e$message)
    NULL
  })
  return(combined)
}

#' Assign Data Frames to the Global Environment
#'
#' @param data_list A named list of data frames.
assign_to_global <- function(data_list) {
  for (name in names(data_list)) {
    if (!is.null(data_list[[name]])) {
      assign(name, data_list[[name]], envir = .GlobalEnv)
    }
  }
}


###############################################################################
# Main Function: import_all_files
###############################################################################

#' Import All Files in a Directory
#'
#' This function imports all files of supported types (csv, xls, xlsx, txt, json, rds, sav, por)
#' from a specified directory. It can work in parallel, return a list of data frames,
#' assign each data frame to the global environment, or collapse (join) all data frames by a given key.
#'
#' @param dir_path Character string specifying the directory path. Default is the current working directory.
#' @param pattern Optional regular expression. Only files with names matching the regular expression are returned.
#'   Default is \code{NULL} (all files).
#' @param list_output Logical. If \code{TRUE}, the function returns a named list of data frames.
#'   If \code{FALSE} (default), data frames are assigned to the global environment (unless collapse is provided).
#' @param collapse Character string specifying a column name on which to join all data frames.
#'   If not \code{NULL}, the function returns a single joined data frame.
#' @param join_method Character string specifying the join method: one of "full_join", "inner_join", 
#'   "left_join", or "right_join". Default is "full_join". (Only used if \code{collapse} is not \code{NULL}.)
#' @param parallel Logical. If \code{TRUE} (default) and if more than one file is found,
#'   parallel processing will be used.
#'
#' @return If \code{list_output = TRUE}, a named list of imported data frames.
#'   If \code{collapse} is provided, a single data frame resulting from joining all imported data frames by the specified column.
#'   Otherwise (if \code{list_output = FALSE} and \code{collapse} is \code{NULL}), the function assigns each data frame
#'   to a variable in the global environment and returns the list invisibly.
#'
#' @export
import_all_files <- function(dir_path = getwd(), 
                             pattern = NULL, 
                             list_output = FALSE, 
                             collapse = NULL, 
                             join_method = "full_join",
                             parallel = TRUE) {
  # Validate directory
  check_directory(dir_path)
  
  # List files based on the pattern
  files <- list_files_in_directory(dir_path, pattern)
  if (length(files) == 0) {
    stop("No files found in directory: ", dir_path)
  }
  
  # Read files using parallel processing (if enabled and more than one file exists)
  if (parallel && length(files) > 1) {
    n_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    data_results <- foreach::foreach(file = files, 
                                     .packages = required_packages,
                                     .export = c("read_file")) %dopar% {
                                       read_file(file)
                                     }
    parallel::stopCluster(cl)
  } else {
    data_results <- lapply(files, read_file)
  }
  
  # Extract file names and data frames
  names(data_results) <- sapply(data_results, function(x) x$file_name)
  data_list <- lapply(data_results, function(x) x$data)
  
  # Remove files that could not be imported (i.e. NULLs)
  valid_data <- data_list[!sapply(data_list, is.null)]
  if (length(valid_data) == 0) {
    stop("No files were successfully imported.")
  }
  
  # If collapse is specified, merge data frames by the specified key.
  if (!is.null(collapse)) {
    combined <- combine_data_frames(valid_data, by_col = collapse, join_method = join_method)
    return(combined)
  }
  
  # Return a list if requested.
  if (list_output) {
    return(valid_data)
  }
  
  # Otherwise, assign each data frame to the global environment.
  assign_to_global(valid_data)
  invisible(valid_data)
}


###############################################################################
# User Acceptance Testing (UAT)
###############################################################################
# The following tests exercise every parameter and many edge cases.
# They create temporary files and directories, run the function, and check outcomes.
# After testing, temporary files/directories are cleaned up.

cat("Starting UAT tests...\n")

# Create a temporary directory for tests.
temp_dir <- tempfile("import_test")
dir.create(temp_dir)

# 1. Create Supported Files
# ---------------------------

# CSV file
csv_file <- file.path(temp_dir, "test.csv")
write.csv(data.frame(id = 1:3, value = c("A", "B", "C")), csv_file, row.names = FALSE)

# TXT file (tab-delimited)
txt_file <- file.path(temp_dir, "test.txt")
write.table(data.frame(id = 1:3, value = c("X", "Y", "Z")), txt_file, sep = "\t", row.names = FALSE)

# JSON file
json_file <- file.path(temp_dir, "test.json")
jsonlite::write_json(data.frame(id = 1:3, value = c("foo", "bar", "baz")), json_file)

# RDS file
rds_file <- file.path(temp_dir, "test.rds")
saveRDS(data.frame(id = 1:3, value = c("alpha", "beta", "gamma")), rds_file)

# Create an unsupported file type.
unsupported_file <- file.path(temp_dir, "test.unsupported")
writeLines("This is an unsupported file type", unsupported_file)

# 2. Test: Non-Existent Directory
test_nonexistent <- tryCatch({
  import_all_files(dir_path = "non_existent_dir")
  FALSE
}, error = function(e) TRUE)
stopifnot(test_nonexistent)
cat("Test 2 passed: Non-existent directory handled.\n")

# 3. Test: Empty Directory
empty_dir <- tempfile("empty_test")
dir.create(empty_dir)
test_empty <- tryCatch({
  import_all_files(dir_path = empty_dir)
  FALSE
}, error = function(e) TRUE)
stopifnot(test_empty)
cat("Test 3 passed: Empty directory handled.\n")

# 4. Test: list_output = TRUE returns a named list with the correct number of files
result_list <- import_all_files(dir_path = temp_dir, list_output = TRUE)
stopifnot(is.list(result_list))
stopifnot(length(result_list) >= 4)  # unsupported file returns NULL and is filtered out.
cat("Test 4 passed: list_output returns a valid list.\n")

# 5. Test: collapse functionality (merging data frames by "id")
combined <- import_all_files(dir_path = temp_dir, collapse = "id", join_method = "full_join")
stopifnot(is.data.frame(combined))
stopifnot("id" %in% colnames(combined))
cat("Test 5 passed: Data frames collapsed (merged) by 'id'.\n")

# 6. Test: Global assignment (list_output = FALSE, collapse = NULL)
if (exists("test.csv", envir = .GlobalEnv)) {
  rm("test.csv", envir = .GlobalEnv)
}
import_all_files(dir_path = temp_dir, list_output = FALSE, collapse = NULL)
stopifnot(exists("test.csv", envir = .GlobalEnv))
rm("test.csv", envir = .GlobalEnv)
cat("Test 6 passed: Global assignment successful.\n")

# 7. Test: Pattern matching parameter (only select CSV files)
csv_only <- import_all_files(dir_path = temp_dir, pattern = "\\.csv$", list_output = TRUE)
stopifnot(all(grepl("\\.csv$", names(csv_only))))
cat("Test 7 passed: Pattern matching returns only CSV files.\n")

# 8. Test: Sequential (non-parallel) processing
result_seq <- suppressWarnings(import_all_files(dir_path = temp_dir, list_output = TRUE, parallel = FALSE))
stopifnot(is.list(result_seq))
cat("Test 8 passed: Sequential processing works.\n")

# Clean up temporary directories and files.
unlink(temp_dir, recursive = TRUE)
unlink(empty_dir, recursive = TRUE)

cat("All UAT tests passed.\n")
