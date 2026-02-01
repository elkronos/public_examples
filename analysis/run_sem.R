# ============================================================
# SEM utilities + workflow builder (lavaan + optional semPlot + optional report)
# ============================================================

# ----------------------------
# Package helper
# ----------------------------

#' Ensure a package is installed (does not attach to search path)
#' @param pkg Character scalar; package name
#' @return TRUE (invisible) or stops with an install note
sem_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install it with install.packages('%s').", pkg, pkg),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Ensure multiple packages are installed
#' @param pkgs Character vector of package names
#' @return TRUE (invisible)
sem_require_all <- function(pkgs) {
  for (p in pkgs) sem_require(p)
  invisible(TRUE)
}

# ----------------------------
# Session helper (optional)
# ----------------------------

#' Clear objects, graphics, and console (optional)
#' @param env Environment to clear (default .GlobalEnv)
#' @param clear_objects Logical
#' @param clear_graphics Logical
#' @param clear_console Logical
#' @return NULL (invisible)
sem_clear_session <- function(env = .GlobalEnv,
                              clear_objects = FALSE,
                              clear_graphics = TRUE,
                              clear_console = FALSE) {
  if (isTRUE(clear_objects)) {
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
  }
  if (isTRUE(clear_graphics)) {
    while (grDevices::dev.cur() > 1) grDevices::dev.off()
  }
  if (isTRUE(clear_console)) {
    cat("\014")
    utils::flush.console()
  }
  invisible(NULL)
}

# ----------------------------
# Data I/O
# ----------------------------

#' Load data from: data.frame, object name, or file path
#'
#' File types: .xlsx/.xls, .csv, .tsv/.txt, .rds
#'
#' @param data_input data.frame/tibble, object name, or file path
#' @param env environment for object lookup
#' @param sheet Excel sheet (name or index)
#' @param ... passed to file readers
#' @return data.frame
sem_load_data <- function(data_input,
                          env = .GlobalEnv,
                          sheet = 1,
                          ...) {
  if (is.data.frame(data_input)) return(as.data.frame(data_input))
  
  if (!is.character(data_input) || length(data_input) != 1) {
    stop("data_input must be a data frame/tibble, an object name (character), or a file path (character).",
         call. = FALSE)
  }
  
  # 1) object name in env
  if (exists(data_input, envir = env, inherits = FALSE)) {
    obj <- get(data_input, envir = env, inherits = FALSE)
    if (!is.data.frame(obj)) stop(sprintf("Object '%s' exists but is not a data frame.", data_input), call. = FALSE)
    return(as.data.frame(obj))
  }
  
  # 2) file path
  path <- data_input
  if (!file.exists(path)) {
    stop(sprintf("No object named '%s' in env and file path does not exist: %s", data_input, path),
         call. = FALSE)
  }
  
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("xlsx", "xls")) {
    sem_require("readxl")
    dat <- readxl::read_excel(path = path, sheet = sheet, col_names = TRUE, ...)
    return(as.data.frame(dat))
  }
  
  if (ext == "csv") {
    dat <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
    return(as.data.frame(dat))
  }
  
  if (ext %in% c("tsv", "txt")) {
    dat <- utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE, ...)
    return(as.data.frame(dat))
  }
  
  if (ext == "rds") {
    dat <- readRDS(path)
    if (!is.data.frame(dat)) stop("The .rds file did not contain a data frame.", call. = FALSE)
    return(as.data.frame(dat))
  }
  
  stop(sprintf("Unsupported file type: .%s", ext), call. = FALSE)
}

# ----------------------------
# Column helpers
# ----------------------------

#' Assert columns exist in a data.frame
#' @param data data.frame
#' @param columns character vector
#' @param label label used in messages
#' @return TRUE (invisible)
sem_assert_columns <- function(data, columns, label = "columns") {
  if (is.null(columns) || length(columns) == 0) return(invisible(TRUE))
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("These %s were not found in data: %s", label, paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Convert selected columns to factor or ordered factor
#' @param data data.frame
#' @param columns character vector
#' @param ordered logical
#' @param drop_unused logical
#' @return data.frame
sem_as_factor_columns <- function(data,
                                  columns,
                                  ordered = FALSE,
                                  drop_unused = TRUE) {
  if (is.null(columns) || length(columns) == 0) return(data)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (!is.character(columns)) stop("columns must be a character vector.", call. = FALSE)
  
  sem_assert_columns(data, columns, label = "columns")
  
  for (nm in columns) {
    data[[nm]] <- if (isTRUE(ordered)) factor(data[[nm]], ordered = TRUE) else as.factor(data[[nm]])
    if (isTRUE(drop_unused)) data[[nm]] <- droplevels(data[[nm]])
  }
  data
}

#' One-hot encode selected columns
#'
#' Uses model.matrix(~ f) per column; by default drops intercept and one level
#' (reference coding) when multiple levels exist.
#'
#' @param data data.frame
#' @param columns character vector
#' @param drop_original logical
#' @param drop_first_level logical
#' @param prefix_sep separator between original name and dummy name
#' @return data.frame
sem_dummy_encode <- function(data,
                             columns,
                             drop_original = TRUE,
                             drop_first_level = TRUE,
                             prefix_sep = "__") {
  if (is.null(columns) || length(columns) == 0) return(data)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (!is.character(columns)) stop("columns must be a character vector.", call. = FALSE)
  
  sem_assert_columns(data, columns, label = "columns")
  
  for (nm in columns) {
    f <- data[[nm]]
    if (!is.factor(f)) f <- as.factor(f)
    
    mm <- stats::model.matrix(~ f)
    mm <- mm[, -1, drop = FALSE] # remove intercept
    
    if (isTRUE(drop_first_level) && ncol(mm) > 1) {
      mm <- mm[, -1, drop = FALSE]
    }
    
    colnames(mm) <- paste0(nm, prefix_sep, make.names(colnames(mm)))
    data <- cbind(data, as.data.frame(mm))
    if (isTRUE(drop_original)) data[[nm]] <- NULL
  }
  
  data
}

# ----------------------------
# Scale diagnostics and transforms
# ----------------------------

#' Numeric variance diagnostic
#' @param data data.frame
#' @param cols character vector or NULL for all columns
#' @param variance_ratio_threshold numeric
#' @param warn logical
#' @return list with ratio and per-column variances
sem_scale_diagnostic <- function(data,
                                 cols = NULL,
                                 variance_ratio_threshold = 1000,
                                 warn = TRUE) {
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  
  use_cols <- if (is.null(cols)) names(data) else intersect(cols, names(data))
  is_num <- vapply(data[use_cols], is.numeric, logical(1))
  num_cols <- use_cols[is_num]
  
  if (length(num_cols) == 0) {
    return(list(variances = numeric(0), variance_ratio = NA_real_, flagged = FALSE))
  }
  
  vars <- vapply(data[num_cols], function(x) stats::var(x, na.rm = TRUE), numeric(1))
  vars <- vars[is.finite(vars) & vars >= 0]
  
  if (length(vars) == 0) {
    return(list(variances = numeric(0), variance_ratio = NA_real_, flagged = FALSE))
  }
  
  vmin <- suppressWarnings(min(vars[vars > 0], na.rm = TRUE))
  vmax <- max(vars, na.rm = TRUE)
  ratio <- if (is.finite(vmin) && vmin > 0) vmax / vmin else Inf
  flagged <- isTRUE(is.finite(ratio)) && ratio >= variance_ratio_threshold
  
  if (isTRUE(warn) && isTRUE(flagged)) {
    warning(sprintf("Observed variances differ strongly (max/min variance ratio = %.2f).", ratio),
            call. = FALSE)
  }
  
  list(variances = vars, variance_ratio = ratio, flagged = flagged)
}

#' Scale numeric columns
#'
#' method:
#' - "none": no change
#' - "zscore": (x - mean)/sd
#' - "center": (x - mean)
#' - "scale": x/sd
#' - "range01": (x - min)/(max - min)
#'
#' @param data data.frame
#' @param cols character vector or NULL for all numeric columns
#' @param method character scalar
#' @return data.frame
sem_scale_numeric <- function(data,
                              cols = NULL,
                              method = c("none", "zscore", "center", "scale", "range01")) {
  method <- match.arg(method)
  if (method == "none") return(data)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  
  use_cols <- if (is.null(cols)) names(data) else intersect(cols, names(data))
  is_num <- vapply(data[use_cols], is.numeric, logical(1))
  num_cols <- use_cols[is_num]
  
  for (nm in num_cols) {
    x <- data[[nm]]
    
    if (method == "zscore") {
      mu <- mean(x, na.rm = TRUE)
      sdv <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(sdv) || sdv == 0) sdv <- 1
      data[[nm]] <- (x - mu) / sdv
    }
    
    if (method == "center") {
      mu <- mean(x, na.rm = TRUE)
      data[[nm]] <- x - mu
    }
    
    if (method == "scale") {
      sdv <- stats::sd(x, na.rm = TRUE)
      if (!is.finite(sdv) || sdv == 0) sdv <- 1
      data[[nm]] <- x / sdv
    }
    
    if (method == "range01") {
      mn <- suppressWarnings(min(x, na.rm = TRUE))
      mx <- suppressWarnings(max(x, na.rm = TRUE))
      rng <- mx - mn
      if (!is.finite(rng) || rng == 0) rng <- 1
      data[[nm]] <- (x - mn) / rng
    }
  }
  
  data
}

# ----------------------------
# SEM model checks
# ----------------------------

#' Validate SEM syntax against data column names
#' @param model_syntax character scalar (lavaan syntax)
#' @param data data.frame
#' @param warn logical
#' @return list with referenced variables and missing variables
sem_validate_model <- function(model_syntax, data, warn = TRUE) {
  sem_require("lavaan")
  
  if (!is.character(model_syntax) || length(model_syntax) != 1) {
    stop("model_syntax must be a single character string.", call. = FALSE)
  }
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  
  pt <- lavaan::lavaanify(model_syntax, fixed.x = FALSE, auto.fix.first = FALSE, as.data.frame = TRUE)
  
  vars <- unique(c(pt$lhs, pt$rhs))
  vars <- vars[nzchar(vars)]
  vars <- vars[!grepl("^[0-9.]+$", vars)]
  vars <- setdiff(vars, c("1"))
  
  latent <- unique(pt$lhs[pt$op == "=~"])
  observed_candidates <- setdiff(vars, latent)
  missing_vars <- setdiff(observed_candidates, names(data))
  
  if (isTRUE(warn) && length(missing_vars) > 0) {
    warning(sprintf("Model references variables not present in data: %s",
                    paste(missing_vars, collapse = ", ")), call. = FALSE)
  }
  
  list(
    referenced = vars,
    latent = latent,
    observed_candidates = observed_candidates,
    missing_in_data = missing_vars
  )
}

# ----------------------------
# Estimation
# ----------------------------

#' Fit SEM via lavaan
#' @param model_syntax character scalar (lavaan syntax)
#' @param data data.frame
#' @param engine "sem" or "cfa"
#' @param estimator lavaan estimator or NULL
#' @param missing lavaan missing strategy or NULL
#' @param ordered character vector of ordered variables or NULL
#' @param ... passed to lavaan::sem or lavaan::cfa
#' @return lavaan fit object
sem_fit <- function(model_syntax,
                    data,
                    engine = c("sem", "cfa"),
                    estimator = NULL,
                    missing = NULL,
                    ordered = NULL,
                    ...) {
  sem_require("lavaan")
  engine <- match.arg(engine)
  
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (!is.character(model_syntax) || length(model_syntax) != 1) {
    stop("model_syntax must be a single character string.", call. = FALSE)
  }
  
  sem_validate_model(model_syntax, data, warn = TRUE)
  
  if (!is.null(ordered)) {
    if (!is.character(ordered)) stop("ordered must be NULL or a character vector.", call. = FALSE)
    sem_assert_columns(data, ordered, label = "ordered variables")
    for (nm in ordered) {
      if (!is.ordered(data[[nm]])) data[[nm]] <- factor(data[[nm]], ordered = TRUE)
    }
  }
  
  args <- list(model = model_syntax, data = data, ...)
  if (!is.null(estimator)) args$estimator <- estimator
  if (!is.null(missing)) args$missing <- missing
  if (!is.null(ordered)) args$ordered <- ordered
  
  if (engine == "sem") return(do.call(lavaan::sem, args))
  do.call(lavaan::cfa, args)
}

# ----------------------------
# Extraction utilities
# ----------------------------

#' Fit measures as a one-row data.frame
#' @param fit lavaan object
#' @return data.frame
sem_fit_measures_df <- function(fit) {
  sem_require("lavaan")
  fm <- lavaan::fitMeasures(fit)
  as.data.frame(as.list(fm), check.names = FALSE)
}

#' Parameter estimates table
#' @param fit lavaan object
#' @param standardized logical
#' @param ci logical
#' @return data.frame
sem_parameters <- function(fit, standardized = TRUE, ci = TRUE) {
  sem_require("lavaan")
  lavaan::parameterEstimates(fit, standardized = isTRUE(standardized), ci = isTRUE(ci))
}

#' Variance table
#' @param fit lavaan object
#' @return data.frame
sem_variance_table <- function(fit) {
  sem_require("lavaan")
  as.data.frame(lavaan::varTable(fit))
}

#' Modification indices table
#' @param fit lavaan object
#' @param mi_min numeric threshold
#' @return data.frame
sem_modindices <- function(fit, mi_min = 10) {
  sem_require("lavaan")
  mi <- lavaan::modindices(fit)
  mi <- mi[order(mi$mi, decreasing = TRUE), , drop = FALSE]
  if (!is.null(mi_min)) mi <- mi[mi$mi >= mi_min, , drop = FALSE]
  mi
}

#' Summary wrapper returning structured objects
#' @param fit lavaan object
#' @param fit_measures logical
#' @param standardized logical
#' @param rsquare logical
#' @return list
sem_summarize <- function(fit,
                          fit_measures = TRUE,
                          standardized = TRUE,
                          rsquare = TRUE) {
  sem_require("lavaan")
  list(
    summary = lavaan::summary(fit,
                              fit.measures = isTRUE(fit_measures),
                              standardized = isTRUE(standardized),
                              rsquare = isTRUE(rsquare)),
    fit_measures = if (isTRUE(fit_measures)) lavaan::fitMeasures(fit) else NULL,
    parameters = sem_parameters(fit, standardized = standardized, ci = TRUE)
  )
}

# ----------------------------
# Plotting (optional)
# ----------------------------

#' Plot SEM paths with semPlot::semPaths
#' @param fit lavaan object
#' @param draw logical
#' @param ... passed to semPlot::semPaths
#' @return semPaths return value (invisible if draw=TRUE)
sem_plot_paths <- function(fit, draw = TRUE, ...) {
  sem_require("semPlot")
  p <- semPlot::semPaths(object = fit, ...)
  if (isTRUE(draw)) invisible(p) else p
}

#' Default plot argument set
#' @return list of args for semPlot::semPaths
sem_plot_defaults <- function() {
  list(
    what = "path",
    whatLabels = "par",
    style = "ram",
    layout = "tree",
    rotation = 2,
    sizeMan = 7,
    sizeLat = 7,
    color = "lightgray",
    edge.label.cex = 1.2,
    label.cex = 1.3
  )
}

# ----------------------------
# Reporting (optional)
# ----------------------------

#' Render an HTML report for a lavaan fit
#' @param fit lavaan object
#' @param output_file file path
#' @param title report title
#' @param include_plot logical
#' @param plot_args list passed to semPlot::semPaths
#' @param include_modindices logical
#' @param mi_min numeric
#' @return normalized output path
sem_report_html <- function(fit,
                            output_file,
                            title = "Analysis Report",
                            include_plot = TRUE,
                            plot_args = list(),
                            include_modindices = FALSE,
                            mi_min = 10) {
  sem_require_all(c("rmarkdown", "knitr", "lavaan"))
  
  if (!is.character(output_file) || length(output_file) != 1) {
    stop("output_file must be a single character file path.", call. = FALSE)
  }
  
  out_dir <- dirname(normalizePath(output_file, winslash = "/", mustWork = FALSE))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  fit_rds <- tempfile(fileext = ".rds")
  saveRDS(fit, fit_rds)
  
  plot_args_rds <- tempfile(fileext = ".rds")
  saveRDS(plot_args, plot_args_rds)
  
  opts <- list(include_plot = isTRUE(include_plot),
               include_modindices = isTRUE(include_modindices),
               mi_min = mi_min)
  opts_rds <- tempfile(fileext = ".rds")
  saveRDS(opts, opts_rds)
  
  rmd <- paste0(
    "---\n",
    "title: \"", gsub("\"", "\\\\\"", title), "\"\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_depth: 2\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)\n",
    "library(lavaan)\n",
    "library(knitr)\n",
    "```\n\n",
    "```{r load_objects}\n",
    "fit <- readRDS(\"", fit_rds, "\")\n",
    "plot_args <- readRDS(\"", plot_args_rds, "\")\n",
    "opts <- readRDS(\"", opts_rds, "\")\n",
    "```\n\n",
    "## Model summary\n\n",
    "```{r model_summary}\n",
    "print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))\n",
    "```\n\n",
    "## Fit measures\n\n",
    "```{r fit_measures}\n",
    "fm <- fitMeasures(fit)\n",
    "kable(as.data.frame(as.list(fm), check.names = FALSE))\n",
    "```\n\n",
    "## Parameter estimates\n\n",
    "```{r params}\n",
    "pe <- parameterEstimates(fit, standardized = TRUE, ci = TRUE)\n",
    "kable(pe)\n",
    "```\n\n",
    "## Variance table\n\n",
    "```{r vartable}\n",
    "vt <- varTable(fit)\n",
    "kable(as.data.frame(vt))\n",
    "```\n\n",
    if (isTRUE(include_plot)) {
      paste0(
        "## Path diagram\n\n",
        "```{r plot}\n",
        "if (!requireNamespace(\"semPlot\", quietly = TRUE)) {\n",
        "  cat(\"Package 'semPlot' is required for the diagram. Install it with install.packages('semPlot').\")\n",
        "} else {\n",
        "  do.call(semPlot::semPaths, c(list(object = fit), plot_args))\n",
        "}\n",
        "```\n\n"
      )
    } else "",
    if (isTRUE(include_modindices)) {
      paste0(
        "## Modification indices\n\n",
        "```{r modindices}\n",
        "mi <- modindices(fit)\n",
        "mi <- mi[order(mi$mi, decreasing = TRUE), , drop = FALSE]\n",
        "if (!is.null(opts$mi_min)) mi <- mi[mi$mi >= opts$mi_min, , drop = FALSE]\n",
        "kable(mi)\n",
        "```\n\n"
      )
    } else ""
  )
  
  rmd_file <- tempfile(fileext = ".Rmd")
  writeLines(rmd, rmd_file)
  
  rmarkdown::render(input = rmd_file, output_file = output_file, quiet = TRUE)
  normalizePath(output_file, winslash = "/", mustWork = FALSE)
}

# ----------------------------
# Workflow steps
# ----------------------------

#' Create a preprocessing step
#' @param name step name
#' @param fun function(data) -> data
#' @return list
sem_step <- function(name, fun) {
  if (!is.character(name) || length(name) != 1) stop("step name must be a character scalar.", call. = FALSE)
  if (!is.function(fun)) stop("step fun must be a function.", call. = FALSE)
  list(name = name, fun = fun)
}

# Step factories
sem_step_factor <- function(columns, ordered = FALSE, drop_unused = TRUE) {
  sem_step("factor", function(data) sem_as_factor_columns(data, columns, ordered = ordered, drop_unused = drop_unused))
}

sem_step_ordered <- function(columns, drop_unused = TRUE) {
  sem_step("ordered", function(data) sem_as_factor_columns(data, columns, ordered = TRUE, drop_unused = drop_unused))
}

sem_step_dummy <- function(columns, drop_original = TRUE, drop_first_level = TRUE, prefix_sep = "__") {
  sem_step("dummy", function(data) sem_dummy_encode(data, columns,
                                                    drop_original = drop_original,
                                                    drop_first_level = drop_first_level,
                                                    prefix_sep = prefix_sep))
}

sem_step_scale <- function(method = c("none", "zscore", "center", "scale", "range01"), cols = NULL) {
  method <- match.arg(method)
  sem_step("scale", function(data) sem_scale_numeric(data, cols = cols, method = method))
}

sem_step_scale_if_needed <- function(method = c("zscore", "center", "scale", "range01"),
                                     cols = NULL,
                                     threshold = 1000) {
  method <- match.arg(method)
  sem_step("scale_if_needed", function(data) {
    diag <- sem_scale_diagnostic(data, cols = cols, variance_ratio_threshold = threshold, warn = TRUE)
    if (isTRUE(diag$flagged)) sem_scale_numeric(data, cols = cols, method = method) else data
  })
}

# ----------------------------
# Workflow object
# ----------------------------

#' Create a workflow specification
#' @param data_input data.frame, object name, or file path
#' @param model_syntax lavaan syntax string
#' @param engine "sem" or "cfa"
#' @return sem_workflow object
sem_workflow <- function(data_input = NULL,
                         model_syntax = NULL,
                         engine = c("sem", "cfa")) {
  engine <- match.arg(engine)
  structure(
    list(
      data_input = data_input,
      model_syntax = model_syntax,
      engine = engine,
      data_env = .GlobalEnv,
      sheet = 1,
      reader_args = list(),
      steps = list(),
      check_scales = TRUE,
      scale_threshold = 1000,
      validate_model = TRUE,
      estimator = NULL,
      missing = NULL,
      ordered = NULL,
      sem_args = list(),
      plot_enabled = FALSE,
      plot_draw = TRUE,
      plot_args = sem_plot_defaults(),
      report_enabled = FALSE,
      report_path = NULL,
      report_title = "Analysis Report",
      report_include_plot = TRUE,
      report_include_modindices = FALSE,
      report_mi_min = 10
    ),
    class = "sem_workflow"
  )
}

# Workflow setters
sem_wf_set_data <- function(wf, data_input, env = .GlobalEnv, sheet = 1, reader_args = list()) {
  wf$data_input <- data_input
  wf$data_env <- env
  wf$sheet <- sheet
  wf$reader_args <- reader_args
  wf
}

sem_wf_set_model <- function(wf, model_syntax, engine = wf$engine) {
  wf$model_syntax <- model_syntax
  wf$engine <- match.arg(engine, c("sem", "cfa"))
  wf
}

sem_wf_add_step <- function(wf, step) {
  if (!inherits(wf, "sem_workflow")) stop("wf must be a sem_workflow.", call. = FALSE)
  if (!is.list(step) || is.null(step$fun)) stop("step must be created by sem_step() or a step factory.", call. = FALSE)
  wf$steps <- c(wf$steps, list(step))
  wf
}

sem_wf_set_fit_options <- function(wf, estimator = NULL, missing = NULL, ordered = NULL, sem_args = list()) {
  wf$estimator <- estimator
  wf$missing <- missing
  wf$ordered <- ordered
  wf$sem_args <- sem_args
  wf
}

sem_wf_set_checks <- function(wf, validate_model = TRUE, check_scales = TRUE, scale_threshold = 1000) {
  wf$validate_model <- isTRUE(validate_model)
  wf$check_scales <- isTRUE(check_scales)
  wf$scale_threshold <- scale_threshold
  wf
}

sem_wf_set_plot <- function(wf, enabled = TRUE, draw = TRUE, plot_args = list()) {
  wf$plot_enabled <- isTRUE(enabled)
  wf$plot_draw <- isTRUE(draw)
  wf$plot_args <- utils::modifyList(sem_plot_defaults(), plot_args)
  wf
}

sem_wf_set_report <- function(wf,
                              enabled = TRUE,
                              path = NULL,
                              title = "Analysis Report",
                              include_plot = TRUE,
                              include_modindices = FALSE,
                              mi_min = 10) {
  wf$report_enabled <- isTRUE(enabled)
  wf$report_path <- path
  wf$report_title <- title
  wf$report_include_plot <- isTRUE(include_plot)
  wf$report_include_modindices <- isTRUE(include_modindices)
  wf$report_mi_min <- mi_min
  wf
}

# ----------------------------
# Workflow execution
# ----------------------------

#' Run a workflow specification
#' @param wf sem_workflow
#' @return sem_result object
sem_run <- function(wf) {
  if (!inherits(wf, "sem_workflow")) stop("wf must be a sem_workflow.", call. = FALSE)
  if (is.null(wf$data_input)) stop("wf$data_input is required.", call. = FALSE)
  if (is.null(wf$model_syntax)) stop("wf$model_syntax is required.", call. = FALSE)
  
  # Load
  dat <- do.call(sem_load_data,
                 c(list(data_input = wf$data_input, env = wf$data_env, sheet = wf$sheet),
                   wf$reader_args))
  
  # Steps
  if (length(wf$steps) > 0) {
    for (st in wf$steps) dat <- st$fun(dat)
  }
  
  # Scale diagnostics (numeric only)
  scale_diag <- NULL
  if (isTRUE(wf$check_scales)) {
    scale_diag <- sem_scale_diagnostic(dat, cols = NULL, variance_ratio_threshold = wf$scale_threshold, warn = TRUE)
  }
  
  # Model check
  model_check <- NULL
  if (isTRUE(wf$validate_model)) {
    model_check <- sem_validate_model(wf$model_syntax, dat, warn = TRUE)
  }
  
  # Fit
  fit <- do.call(
    sem_fit,
    c(list(
      model_syntax = wf$model_syntax,
      data = dat,
      engine = wf$engine,
      estimator = wf$estimator,
      missing = wf$missing,
      ordered = wf$ordered
    ), wf$sem_args)
  )
  
  # Outputs
  sums <- sem_summarize(fit, fit_measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  p <- NULL
  if (isTRUE(wf$plot_enabled)) {
    # semPlot is optional; plot call happens only when enabled
    p <- do.call(sem_plot_paths, c(list(fit = fit, draw = wf$plot_draw), wf$plot_args))
  }
  
  report_path <- NULL
  if (isTRUE(wf$report_enabled)) {
    if (is.null(wf$report_path) || !is.character(wf$report_path) || length(wf$report_path) != 1) {
      stop("wf$report_path must be a single file path when report is enabled.", call. = FALSE)
    }
    report_path <- sem_report_html(
      fit = fit,
      output_file = wf$report_path,
      title = wf$report_title,
      include_plot = wf$report_include_plot,
      plot_args = wf$plot_args,
      include_modindices = wf$report_include_modindices,
      mi_min = wf$report_mi_min
    )
  }
  
  structure(
    list(
      workflow = wf,
      data = dat,
      scale_diagnostic = scale_diag,
      model_check = model_check,
      fit = fit,
      summaries = sums,
      fit_measures = sem_fit_measures_df(fit),
      parameters = sem_parameters(fit, standardized = TRUE, ci = TRUE),
      variance_table = sem_variance_table(fit),
      plot = p,
      report = report_path
    ),
    class = "sem_result"
  )
}

# Optional print method
print.sem_result <- function(x, ...) {
  cat("sem_result\n")
  cat("- engine:", x$workflow$engine, "\n")
  cat("- n:", lavaan::nobs(x$fit), "\n")
  if (!is.null(x$scale_diagnostic) && isTRUE(x$scale_diagnostic$flagged)) {
    cat("- scale ratio flagged:", sprintf("%.2f", x$scale_diagnostic$variance_ratio), "\n")
  }
  cat("\nFit measures (selected):\n")
  fm <- x$fit_measures
  keep <- intersect(c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"), names(fm))
  if (length(keep) > 0) print(fm[, keep, drop = FALSE]) else print(fm)
  invisible(x)
}

# ----------------------------
# Example data
# ----------------------------

#' Sample dataset used for examples
#' @return data.frame
sem_create_sample_data <- function() {
  rep <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  water <- c("FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD", "FLOOD",
             "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD", "AWD")
  priming <- c("NP", "NP", "NP", "HP", "HP", "HP", "OP", "OP", "OP",
               "NP", "NP", "NP", "HP", "HP", "HP", "OP", "OP", "OP")
  aba <- c(4.60, 5.70, 4.10, 7.10, 5.60, 6.30, 8.00, 7.40, 7.80, 7.60, 8.70, 7.20, 10.10, 10.60, 9.30, 11.00, 10.40, 10.80)
  apx <- c(0.90, 0.77, 0.81, 0.97, 0.91, 0.87, 1.73, 1.63, 1.45, 1.45, 1.30, 1.10, 1.95, 1.90, 1.85, 2.73, 2.63, 2.50)
  pod <- c(3.78, 4.40, 5.33, 6.73, 6.31, 5.40, 8.38, 7.40, 7.63, 7.31, 6.40, 6.61, 9.31, 9.28, 8.64, 11.38, 11.43, 10.71)
  ph <- c(91, 82, 87, 94, 87, 82, 110, 96, 92, 103, 89, 83, 104, 91, 84, 109, 100, 93)
  til <- c(11, 11, 10, 15, 14, 13, 17, 15, 14, 10, 13, 14, 17, 20, 18, 23, 19, 22)
  pl <- c(13.68, 16.90, 14.35, 18.43, 18.92, 15.36, 22.76, 20.42, 19.96, 20.89, 17.85, 16.27, 23.90, 20.85, 21.46, 27.29, 21.82, 23.45)
  grp <- c(75.0, 84.5, 70.2, 94.1, 88.8, 81.1, 117.3, 104.6, 100.2, 95.3, 89.5, 81.2, 116.6, 100.4, 109.6, 150.2, 124.6, 137.3)
  tgw <- c(17.25, 13.63, 15.61, 20.89, 19.66, 17.37, 22.25, 22.65, 20.39, 18.60, 20.40, 17.09, 23.36, 21.40, 20.26, 29.33, 25.70, 26.40)
  gy <- c(20.92, 23.96, 19.12, 28.77, 25.90, 30.33, 36.38, 35.74, 30.78, 27.98, 31.50, 20.18, 33.01, 40.88, 35.11, 40.56, 45.68, 47.25)
  
  data.frame(
    rep = rep,
    water = water,
    priming = priming,
    aba = aba,
    apx = apx,
    pod = pod,
    ph = ph,
    til = til,
    pl = pl,
    grp = grp,
    tgw = tgw,
    gy = gy,
    check.names = FALSE
  )
}

# ----------------------------
# Example usage
# ----------------------------
if (interactive()) {
  data <- sem_create_sample_data()
  
  model_syntax <- "
    EA =~ aba + apx + pod
    YC =~ til + pl + grp + tgw
    gy ~ EA + YC
  "
  
  # Workflow with explicit steps
  wf <- sem_workflow(data_input = data, model_syntax = model_syntax, engine = "sem")
  wf <- sem_wf_add_step(wf, sem_step_factor(c("rep", "water", "priming")))
  wf <- sem_wf_add_step(wf, sem_step_scale_if_needed(method = "zscore", threshold = 1000))
  wf <- sem_wf_set_plot(wf, enabled = TRUE, draw = TRUE)
  
  res <- sem_run(wf)
  print(res)
  
  # Direct access
  vt <- sem_variance_table(res$fit)
  lavaan_vt <- lavaan::varTable(res$fit)
}
