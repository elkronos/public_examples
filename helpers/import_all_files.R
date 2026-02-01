###############################################################################
# File Import Workflow
###############################################################################

IMPORTER_DEFAULT_PACKAGES <- c(
  "vroom", "readxl", "jsonlite", "haven",
  "dplyr", "foreach", "doParallel"
)

#' Ensure Packages Are Available
#'
#' @param packages Character vector of package names.
#' @param install_missing Logical. If TRUE, installs missing packages.
#' @param quietly Logical. If TRUE, reduces messages.
#' @return Invisibly TRUE when all packages are available.
ensure_packages <- function(packages = IMPORTER_DEFAULT_PACKAGES,
                            install_missing = TRUE,
                            quietly = TRUE) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    if (!isTRUE(install_missing)) {
      stop("Missing packages: ", paste(missing, collapse = ", "))
    }
    install.packages(missing)
  }
  
  # Load only when present (some users prefer namespace calls only)
  suppressWarnings(
    invisible(lapply(packages, function(p) {
      if (requireNamespace(p, quietly = TRUE)) {
        suppressPackageStartupMessages(
          library(p, character.only = TRUE, quietly = quietly, warn.conflicts = FALSE)
        )
      }
      TRUE
    }))
  )
  
  invisible(TRUE)
}

#' Validate Directory
#'
#' @param dir_path Directory path.
#' @param must_exist Logical. If TRUE, directory must exist.
#' @return Normalized directory path.
validate_directory <- function(dir_path, must_exist = TRUE) {
  if (!is.character(dir_path) || length(dir_path) != 1L || is.na(dir_path) || !nzchar(dir_path)) {
    stop("dir_path must be a single non-empty string.")
  }
  if (must_exist && !dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  normalizePath(dir_path, winslash = "/", mustWork = must_exist)
}

#' List Files From a Directory
#'
#' @param dir_path Directory path.
#' @param pattern Optional regex pattern (matched against file name).
#' @param recursive Logical. If TRUE, scans subdirectories.
#' @param include_hidden Logical. If TRUE, includes dotfiles.
#' @return Character vector of file paths.
list_files <- function(dir_path,
                       pattern = NULL,
                       recursive = FALSE,
                       include_hidden = FALSE) {
  dir_path <- validate_directory(dir_path, must_exist = TRUE)
  
  files <- list.files(
    dir_path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    all.files = include_hidden,
    include.dirs = FALSE
  )
  
  if (length(files) == 0L) return(character(0))
  
  info <- file.info(files)
  files <- rownames(info)[isTRUE(info$isdir) == FALSE]
  files[!is.na(files)]
}

#' Supported Extensions
#'
#' @return Character vector of extensions.
supported_extensions <- function() {
  c("csv", "txt", "tsv", "xls", "xlsx", "json", "rds", "sav", "por", "zsav")
}

#' Safe Extension Extraction
#'
#' @param file File path.
#' @return Lowercase extension (without dot) or "" if none.
file_extension <- function(file) {
  ext <- tolower(tools::file_ext(file))
  ifelse(is.na(ext), "", ext)
}

#' Convert an Object to a Data Frame Form
#'
#' @param x Any R object.
#' @return A data.frame (possibly with list columns).
as_data_frame_safe <- function(x) {
  if (is.null(x)) return(NULL)
  
  if (is.data.frame(x)) return(x)
  
  if (inherits(x, "tbl")) return(as.data.frame(x))
  
  if (is.matrix(x) || (is.atomic(x) && !is.null(dim(x)))) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  
  if (is.atomic(x) && is.null(dim(x))) {
    return(data.frame(value = x, stringsAsFactors = FALSE))
  }
  
  out <- tryCatch(
    as.data.frame(x, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (!is.null(out)) return(out)
  
  data.frame(value = I(list(x)), stringsAsFactors = FALSE)
}

#' Add Standard Metadata Columns
#'
#' @param df A data.frame.
#' @param file_path Full file path.
#' @param meta_cols Character vector of metadata column names.
#' @return data.frame with metadata columns appended.
add_file_metadata <- function(df,
                              file_path,
                              meta_cols = c("system_date", "original_file_name", "original_file_path")) {
  if (is.null(df)) return(NULL)
  if (!is.data.frame(df)) df <- as_data_frame_safe(df)
  
  file_name <- basename(file_path)
  
  if ("system_date" %in% meta_cols) df[["system_date"]] <- Sys.Date()
  if ("original_file_name" %in% meta_cols) df[["original_file_name"]] <- file_name
  if ("original_file_path" %in% meta_cols) df[["original_file_path"]] <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  
  df
}

#' Reader Options
#'
#' @return A list of defaults used by readers.
default_reader_options <- function() {
  list(
    vroom = list(show_col_types = FALSE, progress = FALSE),
    csv = list(delim = ","),
    tsv = list(delim = "\t"),
    txt = list(delim = "\t"),
    excel = list(sheet = 1),
    json = list(flatten = TRUE),
    haven = list()
  )
}

#' Read One File by Extension
#'
#' @param file_path Full file path.
#' @param reader_options List from default_reader_options(), optionally edited by user.
#' @return A list with fields: status, file_path, file_name, ext, data, message.
read_one_file <- function(file_path,
                          reader_options = default_reader_options()) {
  ext <- file_extension(file_path)
  file_name <- basename(file_path)
  
  if (!(ext %in% supported_extensions())) {
    return(list(
      status = "unsupported",
      file_path = file_path,
      file_name = file_name,
      ext = ext,
      data = NULL,
      message = paste0("Unsupported extension: ", ext)
    ))
  }
  
  data <- NULL
  msg <- NULL
  
  data <- tryCatch({
    if (ext %in% c("csv", "txt", "tsv")) {
      delim <- reader_options[[ext]][["delim"]]
      vroom_args <- reader_options[["vroom"]]
      do.call(
        vroom::vroom,
        c(list(file = file_path, delim = delim), vroom_args)
      )
    } else if (ext %in% c("xls", "xlsx")) {
      sheet <- reader_options[["excel"]][["sheet"]]
      readxl::read_excel(file_path, sheet = sheet)
    } else if (ext == "json") {
      flatten <- isTRUE(reader_options[["json"]][["flatten"]])
      jsonlite::fromJSON(file_path, flatten = flatten)
    } else if (ext == "rds") {
      readRDS(file_path)
    } else if (ext %in% c("sav", "zsav")) {
      haven::read_sav(file_path)
    } else if (ext == "por") {
      haven::read_por(file_path)
    } else {
      stop("No reader available for extension: ", ext)
    }
  }, error = function(e) {
    msg <<- e$message
    NULL
  })
  
  if (is.null(data)) {
    return(list(
      status = "error",
      file_path = file_path,
      file_name = file_name,
      ext = ext,
      data = NULL,
      message = if (!is.null(msg)) msg else "Reader returned NULL"
    ))
  }
  
  # Standardize to data.frame form for downstream handling
  df <- as_data_frame_safe(data)
  df <- add_file_metadata(df, file_path = file_path)
  
  list(
    status = "ok",
    file_path = file_path,
    file_name = file_name,
    ext = ext,
    data = df,
    message = NULL
  )
}

#' Make Unique Names
#'
#' @param x Character vector.
#' @return Character vector with uniqueness guaranteed.
make_unique_names <- function(x) {
  if (length(x) == 0L) return(x)
  make.unique(x, sep = "_")
}

#' Object Names From File Paths
#'
#' @param file_paths Character vector.
#' @param name_from One of "file" (basename including extension) or "stem" (basename without extension).
#' @param sanitize Logical. If TRUE, uses make.names().
#' @return Character vector of names aligned to file_paths.
object_names_from_files <- function(file_paths,
                                    name_from = c("file", "stem"),
                                    sanitize = TRUE) {
  name_from <- match.arg(name_from)
  base <- basename(file_paths)
  nm <- if (name_from == "stem") tools::file_path_sans_ext(base) else base
  if (isTRUE(sanitize)) nm <- make.names(nm, unique = FALSE)
  make_unique_names(nm)
}

#' Compute Worker Count
#'
#' @param workers Integer or NULL.
#' @return Integer worker count >= 1.
compute_workers <- function(workers = NULL) {
  if (!is.null(workers)) {
    workers <- as.integer(workers)
    if (is.na(workers) || workers < 1L) stop("workers must be NULL or an integer >= 1.")
    return(workers)
  }
  
  cores_logical <- parallel::detectCores(logical = TRUE)
  if (is.na(cores_logical) || cores_logical < 1L) return(1L)
  max(1L, cores_logical - 1L)
}

#' Import Many Files
#'
#' @param files Character vector of file paths.
#' @param parallel Logical. If TRUE, may use a parallel backend when available.
#' @param workers Integer or NULL. Used when parallel = TRUE.
#' @param reader_options List from default_reader_options(), optionally edited by user.
#' @return A list: results (per-file), data (named list), problems (data.frame), files (input).
import_files <- function(files,
                         parallel = TRUE,
                         workers = NULL,
                         reader_options = default_reader_options()) {
  if (!is.character(files)) stop("files must be a character vector.")
  files <- files[file.exists(files)]
  if (length(files) == 0L) {
    return(list(
      files = character(0),
      results = list(),
      data = list(),
      problems = data.frame(
        file_path = character(0),
        file_name = character(0),
        ext = character(0),
        status = character(0),
        message = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }
  
  use_parallel <- isTRUE(parallel) &&
    length(files) > 1L &&
    requireNamespace("foreach", quietly = TRUE) &&
    requireNamespace("doParallel", quietly = TRUE)
  
  if (use_parallel) {
    w <- compute_workers(workers)
    cl <- parallel::makeCluster(w)
    on.exit({
      try(parallel::stopCluster(cl), silent = TRUE)
      try(doParallel::registerDoSEQ(), silent = TRUE)
    }, add = TRUE)
    
    doParallel::registerDoParallel(cl)
    
    # Export functions used by workers
    exports <- c(
      "default_reader_options", "supported_extensions", "file_extension",
      "as_data_frame_safe", "add_file_metadata", "read_one_file"
    )
    
    results <- foreach::foreach(
      f = files,
      .packages = c("vroom", "readxl", "jsonlite", "haven", "tools"),
      .export = exports
    ) %dopar% {
      read_one_file(f, reader_options = reader_options)
    }
  } else {
    results <- lapply(files, read_one_file, reader_options = reader_options)
  }
  
  # Organize outputs
  ok_idx <- vapply(results, function(x) identical(x$status, "ok"), logical(1))
  data_list <- lapply(results[ok_idx], function(x) x$data)
  
  # Use file_name as list names by default
  data_names <- vapply(results[ok_idx], function(x) x$file_name, character(1))
  names(data_list) <- make_unique_names(data_names)
  
  problems <- do.call(rbind, lapply(results, function(x) {
    if (identical(x$status, "ok")) return(NULL)
    data.frame(
      file_path = x$file_path,
      file_name = x$file_name,
      ext = x$ext,
      status = x$status,
      message = ifelse(is.null(x$message), "", x$message),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(problems)) {
    problems <- data.frame(
      file_path = character(0),
      file_name = character(0),
      ext = character(0),
      status = character(0),
      message = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  list(
    files = files,
    results = results,
    data = data_list,
    problems = problems
  )
}

#' Join a List of Data Frames
#'
#' @param data_list Named list of data frames.
#' @param by Column name to join on.
#' @param join_method One of "full_join", "inner_join", "left_join", "right_join".
#' @param drop_metadata Logical. If TRUE, drops standard metadata columns before joining and adds one set back.
#' @param metadata_cols Character vector of metadata columns.
#' @return A joined data frame.
join_data_frames <- function(data_list,
                             by,
                             join_method = c("full_join", "inner_join", "left_join", "right_join"),
                             drop_metadata = TRUE,
                             metadata_cols = c("system_date", "original_file_name", "original_file_path")) {
  join_method <- match.arg(join_method)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required for joining.")
  
  if (!is.list(data_list) || length(data_list) == 0L) stop("data_list must be a non-empty list.")
  if (!is.character(by) || length(by) != 1L || !nzchar(by)) stop("by must be a single non-empty string.")
  
  # Keep only data frames that contain the join key
  keep <- vapply(data_list, function(df) is.data.frame(df) && (by %in% names(df)), logical(1))
  filtered <- data_list[keep]
  
  if (length(filtered) == 0L) {
    stop("No data frames contained the join column: ", by)
  }
  
  if (isTRUE(drop_metadata)) {
    filtered <- lapply(filtered, function(df) {
      drop <- intersect(metadata_cols, names(df))
      if (length(drop) > 0L) df <- df[, setdiff(names(df), drop), drop = FALSE]
      df
    })
  }
  
  join_fun <- switch(
    join_method,
    full_join  = dplyr::full_join,
    inner_join = dplyr::inner_join,
    left_join  = dplyr::left_join,
    right_join = dplyr::right_join
  )
  
  out <- Reduce(function(x, y) join_fun(x, y, by = by), filtered)
  
  if (isTRUE(drop_metadata)) {
    out[["system_date"]] <- Sys.Date()
    out[["original_file_name"]] <- "joined"
    out[["original_file_path"]] <- NA_character_
  }
  
  out
}

#' Assign Objects to an Environment
#'
#' @param data_list Named list of objects (typically data frames).
#' @param file_paths Character vector aligned to data_list (optional).
#' @param envir Environment to assign into.
#' @param name_from "file" or "stem" (used when file_paths provided and data_list has no names).
#' @param sanitize_names Logical. If TRUE, uses make.names().
#' @return Character vector of object names assigned.
assign_objects <- function(data_list,
                           file_paths = NULL,
                           envir = .GlobalEnv,
                           name_from = c("file", "stem"),
                           sanitize_names = TRUE) {
  name_from <- match.arg(name_from)
  
  if (!is.list(data_list) || length(data_list) == 0L) return(character(0))
  
  obj_names <- names(data_list)
  
  if ((is.null(obj_names) || any(!nzchar(obj_names))) && !is.null(file_paths)) {
    obj_names <- object_names_from_files(file_paths, name_from = name_from, sanitize = sanitize_names)
  } else {
    if (isTRUE(sanitize_names)) obj_names <- make.names(obj_names, unique = FALSE)
    obj_names <- make_unique_names(obj_names)
  }
  
  for (i in seq_along(data_list)) {
    if (!is.null(data_list[[i]])) {
      assign(obj_names[i], data_list[[i]], envir = envir)
    }
  }
  
  obj_names
}

#' Create an Import Plan
#'
#' @param dir_path Directory path.
#' @param pattern Optional regex pattern.
#' @param recursive Logical.
#' @param include_hidden Logical.
#' @param parallel Logical.
#' @param workers Integer or NULL.
#' @param reader_options Reader options list.
#' @return A list used by execute_import_plan().
create_import_plan <- function(dir_path = getwd(),
                               pattern = NULL,
                               recursive = FALSE,
                               include_hidden = FALSE,
                               parallel = TRUE,
                               workers = NULL,
                               reader_options = default_reader_options()) {
  list(
    dir_path = validate_directory(dir_path, must_exist = TRUE),
    pattern = pattern,
    recursive = recursive,
    include_hidden = include_hidden,
    parallel = parallel,
    workers = workers,
    reader_options = reader_options
  )
}

#' Execute an Import Plan
#'
#' @param plan A list from create_import_plan().
#' @return A list including files, data, problems, and per-file results.
execute_import_plan <- function(plan) {
  if (!is.list(plan)) stop("plan must be a list created by create_import_plan().")
  
  files <- list_files(
    dir_path = plan$dir_path,
    pattern = plan$pattern,
    recursive = isTRUE(plan$recursive),
    include_hidden = isTRUE(plan$include_hidden)
  )
  
  imported <- import_files(
    files = files,
    parallel = isTRUE(plan$parallel),
    workers = plan$workers,
    reader_options = plan$reader_options
  )
  
  imported$plan <- plan
  imported
}

#' Workflow Wrapper
#'
#' Reads files from a directory, then returns either a list, a joined table, or
#' assigns objects and returns the structured run output.
#'
#' @param dir_path Directory path.
#' @param pattern Optional regex pattern.
#' @param recursive Logical.
#' @param include_hidden Logical.
#' @param output One of "list", "joined", "assigned".
#' @param join_by Column name for joining when output = "joined".
#' @param join_method Join method used when output = "joined".
#' @param parallel Logical.
#' @param workers Integer or NULL.
#' @param name_from Used when output = "assigned": "file" or "stem".
#' @param sanitize_names Logical.
#' @param install_missing Logical. If TRUE, installs missing packages.
#' @param reader_options Reader options list.
#' @return Depending on output:
#'   - "list": named list of imported data frames
#'   - "joined": joined data frame
#'   - "assigned": returns the structured run output (including assigned names)
run_import_workflow <- function(dir_path = getwd(),
                                pattern = NULL,
                                recursive = FALSE,
                                include_hidden = FALSE,
                                output = c("list", "joined", "assigned"),
                                join_by = NULL,
                                join_method = c("full_join", "inner_join", "left_join", "right_join"),
                                parallel = TRUE,
                                workers = NULL,
                                name_from = c("file", "stem"),
                                sanitize_names = TRUE,
                                install_missing = TRUE,
                                reader_options = default_reader_options()) {
  output <- match.arg(output)
  join_method <- match.arg(join_method)
  name_from <- match.arg(name_from)
  
  ensure_packages(IMPORTER_DEFAULT_PACKAGES, install_missing = install_missing)
  
  plan <- create_import_plan(
    dir_path = dir_path,
    pattern = pattern,
    recursive = recursive,
    include_hidden = include_hidden,
    parallel = parallel,
    workers = workers,
    reader_options = reader_options
  )
  
  run <- execute_import_plan(plan)
  
  if (length(run$data) == 0L) {
    stop("No files were successfully imported.")
  }
  
  if (identical(output, "joined")) {
    if (is.null(join_by) || !nzchar(join_by)) stop("join_by must be provided when output = 'joined'.")
    return(join_data_frames(run$data, by = join_by, join_method = join_method))
  }
  
  if (identical(output, "list")) {
    return(run$data)
  }
  
  assigned_names <- assign_objects(
    data_list = run$data,
    file_paths = vapply(run$results, function(x) x$file_path, character(1)),
    envir = .GlobalEnv,
    name_from = name_from,
    sanitize_names = sanitize_names
  )
  
  run$assigned_names <- assigned_names
  run
}

###############################################################################
# User Acceptance Testing
###############################################################################

#' Run Basic UAT for the Import Workflow
#'
#' Creates temporary files, exercises key paths, and cleans up.
#' @return TRUE if all checks pass.
run_import_workflow_uat <- function() {
  cat("Starting UAT tests...\n")
  
  temp_dir <- tempfile("import_test_")
  dir.create(temp_dir)
  
  # Supported files
  csv_file <- file.path(temp_dir, "test.csv")
  write.csv(data.frame(id = 1:3, value = c("A", "B", "C")), csv_file, row.names = FALSE)
  
  txt_file <- file.path(temp_dir, "test.txt")
  write.table(data.frame(id = 1:3, value = c("X", "Y", "Z")), txt_file, sep = "\t", row.names = FALSE)
  
  json_file <- file.path(temp_dir, "test.json")
  jsonlite::write_json(data.frame(id = 1:3, value = c("foo", "bar", "baz")), json_file)
  
  rds_file <- file.path(temp_dir, "test.rds")
  saveRDS(data.frame(id = 1:3, value = c("alpha", "beta", "gamma")), rds_file)
  
  unsupported_file <- file.path(temp_dir, "test.unsupported")
  writeLines("This is an unsupported file type", unsupported_file)
  
  # 1) Non-existent directory
  test_nonexistent <- tryCatch({
    run_import_workflow(dir_path = "non_existent_dir", output = "list", install_missing = FALSE)
    FALSE
  }, error = function(e) TRUE)
  stopifnot(test_nonexistent)
  cat("Test 1 passed: Non-existent directory handled.\n")
  
  # 2) Empty directory
  empty_dir <- tempfile("empty_test_")
  dir.create(empty_dir)
  test_empty <- tryCatch({
    run_import_workflow(dir_path = empty_dir, output = "list", install_missing = FALSE)
    FALSE
  }, error = function(e) TRUE)
  stopifnot(test_empty)
  cat("Test 2 passed: Empty directory handled.\n")
  
  # 3) list output
  result_list <- run_import_workflow(dir_path = temp_dir, output = "list", parallel = FALSE)
  stopifnot(is.list(result_list))
  stopifnot(length(result_list) >= 4)
  cat("Test 3 passed: list output returns a valid list.\n")
  
  # 4) joined output
  combined <- run_import_workflow(dir_path = temp_dir, output = "joined", join_by = "id", join_method = "full_join", parallel = FALSE)
  stopifnot(is.data.frame(combined))
  stopifnot("id" %in% colnames(combined))
  cat("Test 4 passed: joined output created.\n")
  
  # 5) assignment output
  obj_name <- make.names("test.csv")
  if (exists(obj_name, envir = .GlobalEnv)) rm(list = obj_name, envir = .GlobalEnv)
  
  run <- run_import_workflow(dir_path = temp_dir, output = "assigned", parallel = FALSE)
  stopifnot(is.list(run))
  stopifnot(exists(obj_name, envir = .GlobalEnv))
  rm(list = obj_name, envir = .GlobalEnv)
  cat("Test 5 passed: assignment created objects.\n")
  
  # 6) pattern filter
  csv_only <- run_import_workflow(dir_path = temp_dir, pattern = "\\.csv$", output = "list", parallel = FALSE)
  stopifnot(all(grepl("\\.csv$", names(csv_only))))
  cat("Test 6 passed: pattern filter works.\n")
  
  unlink(temp_dir, recursive = TRUE)
  unlink(empty_dir, recursive = TRUE)
  
  cat("All UAT tests passed.\n")
  TRUE
}

###############################################################################
# Usage Notes (examples)
###############################################################################
# 1) Return a named list:
# data_list <- run_import_workflow("path/to/dir", output = "list")
#
# 2) Join by a key:
# joined <- run_import_workflow("path/to/dir", output = "joined", join_by = "id")
#
# 3) Assign into the global environment and keep a run record:
# run <- run_import_workflow("path/to/dir", output = "assigned")
# run$problems
#
# 4) Repeatable plan:
# plan <- create_import_plan("path/to/dir", pattern = "\\.(csv|xlsx)$", parallel = TRUE)
# run  <- execute_import_plan(plan)
