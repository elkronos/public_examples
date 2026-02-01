## Factor analysis workflow (EFA + CFA + reliability)
## --------------------------------------------------
## Notes:
## - Most functions accept a data.frame with item columns (numeric or coercible to numeric).
## - A batch "plan" is a data.frame where each row defines one run.
## - Outputs are written to per-run folders under output_dir.

options(stringsAsFactors = FALSE)

# -----------------------------
# Utilities
# -----------------------------

ensure_packages <- function(packages, install = FALSE, repos = "https://cloud.r-project.org") {
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  
  if (length(missing) > 0) {
    if (isTRUE(install)) {
      install.packages(missing, repos = repos)
    } else {
      stop("Missing required packages: ", paste(missing, collapse = ", "))
    }
  }
  
  invisible(TRUE)
}

load_libraries <- function(packages) {
  suppressPackageStartupMessages(
    lapply(packages, function(p) library(p, character.only = TRUE))
  )
  invisible(TRUE)
}

sanitize_id <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nchar(x) == 0) "run" else x
}

dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

assert_columns <- function(df, cols, df_name = "data.frame") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

as_scalar_character <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) "" else x[[1]]
}

as_scalar_integer <- function(x) {
  x <- suppressWarnings(as.integer(x))
  if (length(x) == 0 || is.na(x[[1]])) NA_integer_ else x[[1]]
}

as_scalar_logical <- function(x) {
  if (is.logical(x)) return(if (length(x) == 0) FALSE else x[[1]])
  x <- tolower(as.character(x))
  if (length(x) == 0) return(FALSE)
  x <- x[[1]]
  x %in% c("true", "t", "1", "yes", "y")
}

# -----------------------------
# Data selection and preparation
# -----------------------------

select_items <- function(df, pattern, ignore_case = FALSE) {
  pattern <- as_scalar_character(pattern)
  if (!nzchar(pattern)) stop("pattern must be a non-empty string.")
  
  hits <- grepl(pattern, names(df), ignore.case = isTRUE(ignore_case), perl = TRUE)
  if (!any(hits)) stop("No column names matched pattern: ", pattern)
  
  out <- df[, hits, drop = FALSE]
  out
}

coerce_items_numeric <- function(items, strict = TRUE) {
  # Converts each column to numeric when possible.
  # strict = TRUE stops if any column becomes all NA after conversion.
  out <- as.data.frame(items)
  
  for (nm in names(out)) {
    x <- out[[nm]]
    if (is.factor(x)) x <- as.character(x)
    
    if (!is.numeric(x)) {
      x2 <- suppressWarnings(as.numeric(x))
      out[[nm]] <- x2
      
      if (isTRUE(strict) && all(is.na(x2))) {
        stop("Column could not be coerced to numeric: ", nm)
      }
    }
  }
  
  out
}

handle_missing <- function(items, method = c("fail", "listwise", "pairwise")) {
  method <- match.arg(method)
  if (method == "fail" && anyNA(items)) {
    stop("Missing values detected. Choose missing = 'listwise' or 'pairwise' to proceed.")
  }
  if (method == "listwise") {
    items <- items[stats::complete.cases(items), , drop = FALSE]
  }
  items
}

standardize_items <- function(items, standardize = c("none", "zscore")) {
  standardize <- match.arg(standardize)
  if (standardize == "none") return(items)
  
  z <- scale(items)
  as.data.frame(z)
}

# -----------------------------
# Reliability
# -----------------------------

compute_reliability <- function(items,
                                check_keys = FALSE,
                                alpha_use = c("pairwise", "complete.obs", "everything"),
                                alpha_na_rm = TRUE,
                                n_iter = 1) {
  alpha_use <- match.arg(alpha_use)
  
  if (ncol(items) < 2) stop("Reliability requires at least 2 items.")
  a <- psych::alpha(
    items,
    check.keys = isTRUE(check_keys),
    use = alpha_use,
    na.rm = isTRUE(alpha_na_rm),
    n.iter = as.integer(n_iter),
    warnings = FALSE
  )
  
  list(
    raw_alpha = a$total$raw_alpha,
    std_alpha = a$total$std.alpha,
    G6 = a$total$G6,
    average_r = a$total$average_r,
    alpha_object = a
  )
}

# -----------------------------
# EFA
# -----------------------------

perform_efa <- function(items,
                        n_factors,
                        rotation = "oblimin",
                        fm = "minres",
                        cor_method = c("cor", "poly", "tet", "mixed"),
                        use = c("pairwise", "complete.obs")) {
  cor_method <- match.arg(cor_method)
  use <- match.arg(use)
  
  if (ncol(items) < 2) stop("EFA requires at least 2 items.")
  if (is.na(n_factors) || n_factors <= 0) stop("n_factors must be a positive integer.")
  if (n_factors > ncol(items)) stop("n_factors cannot exceed the number of items.")
  
  efa <- psych::fa(
    r = items,
    nfactors = as.integer(n_factors),
    rotate = rotation,
    fm = fm,
    cor = cor_method,
    use = use
  )
  
  efa
}

run_parallel_analysis <- function(items,
                                  fm = "minres",
                                  fa = "fa",
                                  n_iter = 20,
                                  cor_method = c("cor", "poly", "tet", "mixed"),
                                  use = c("pairwise", "complete.obs"),
                                  main = "Parallel Analysis Scree Plots") {
  cor_method <- match.arg(cor_method)
  use <- match.arg(use)
  
  psych::fa.parallel(
    x = items,
    fm = fm,
    fa = fa,
    n.iter = as.integer(n_iter),
    cor = cor_method,
    use = use,
    main = main,
    plot = TRUE
  )
}

extract_efa_loadings <- function(efa_object, cutoff = 0) {
  L <- as.matrix(efa_object$loadings)
  if (!is.null(cutoff) && cutoff > 0) {
    L[abs(L) < cutoff] <- 0
  }
  out <- as.data.frame(L)
  out$item <- rownames(L)
  out <- out[, c("item", setdiff(names(out), "item")), drop = FALSE]
  rownames(out) <- NULL
  out
}

# -----------------------------
# CFA
# -----------------------------

build_one_factor_model <- function(item_names, factor_name = "F1") {
  if (length(item_names) < 2) stop("A one-factor model requires at least 2 items.")
  paste0(factor_name, " =~ ", paste(item_names, collapse = " + "))
}

perform_cfa <- function(items,
                        model_syntax,
                        std_lv = TRUE,
                        estimator = "MLR",
                        missing_lavaan = c("listwise", "fiml")) {
  missing_lavaan <- match.arg(missing_lavaan)
  
  fit <- lavaan::cfa(
    model = model_syntax,
    data = items,
    std.lv = isTRUE(std_lv),
    estimator = estimator,
    missing = missing_lavaan
  )
  
  fit
}

cfa_summary <- function(fit,
                        fit_measures = TRUE,
                        standardized = TRUE,
                        rsquare = TRUE) {
  lavaan::summary(
    fit,
    fit.measures = isTRUE(fit_measures),
    standardized = isTRUE(standardized),
    rsquare = isTRUE(rsquare)
  )
}

extract_cfa_fit_measures <- function(fit,
                                     measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")) {
  fm <- lavaan::fitMeasures(fit, fit.measures = measures)
  as.data.frame(as.list(fm))
}

# -----------------------------
# Plot and export helpers
# -----------------------------

save_png <- function(path, expr, width = 1400, height = 1000, res = 150) {
  grDevices::png(filename = path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  invisible(path)
}

write_csv_safely <- function(df, path) {
  utils::write.csv(df, file = path, row.names = FALSE)
  invisible(path)
}

# -----------------------------
# Single-run wrapper
# -----------------------------

run_scale_analysis <- function(df,
                               pattern,
                               n_factors,
                               rotation = "oblimin",
                               model = NA_character_,
                               scale_id = NA_character_,
                               ignore_case = FALSE,
                               coerce_strict = TRUE,
                               missing_items = c("fail", "listwise", "pairwise"),
                               standardize = c("none", "zscore"),
                               cor_method = c("cor", "poly", "tet", "mixed"),
                               efa_use = c("pairwise", "complete.obs"),
                               fm = "minres",
                               alpha_use = c("pairwise", "complete.obs", "everything"),
                               alpha_check_keys = FALSE,
                               alpha_n_iter = 1,
                               cfa_estimator = "MLR",
                               cfa_missing = c("listwise", "fiml"),
                               output_dir = "fa_outputs",
                               save_outputs = TRUE,
                               parallel_n_iter = 20,
                               seed = NA_integer_,
                               quiet = FALSE) {
  missing_items <- match.arg(missing_items)
  standardize <- match.arg(standardize)
  cor_method <- match.arg(cor_method)
  efa_use <- match.arg(efa_use)
  alpha_use <- match.arg(alpha_use)
  cfa_missing <- match.arg(cfa_missing)
  
  if (!is.na(seed)) set.seed(as.integer(seed))
  
  items_raw <- select_items(df, pattern = pattern, ignore_case = ignore_case)
  items_num <- coerce_items_numeric(items_raw, strict = coerce_strict)
  items_miss <- handle_missing(items_num, method = missing_items)
  items_ready <- standardize_items(items_miss, standardize = standardize)
  
  if (nrow(items_ready) < 5) {
    stop("Too few rows after preparation (n = ", nrow(items_ready), ").")
  }
  
  run_id <- sanitize_id(ifelse(is.na(scale_id) || !nzchar(scale_id), as_scalar_character(pattern), as_scalar_character(scale_id)))
  out_dir <- dir_create(file.path(output_dir, run_id))
  
  if (!quiet) {
    message("Run: ", run_id)
    message("  Items: ", ncol(items_ready), " | Rows: ", nrow(items_ready))
    message("  Output: ", out_dir)
  }
  
  # Reliability
  rel <- compute_reliability(
    items_ready,
    check_keys = alpha_check_keys,
    alpha_use = alpha_use,
    alpha_na_rm = TRUE,
    n_iter = alpha_n_iter
  )
  
  # EFA
  efa <- perform_efa(
    items_ready,
    n_factors = n_factors,
    rotation = rotation,
    fm = fm,
    cor_method = cor_method,
    use = efa_use
  )
  
  # Parallel analysis + scree
  pa_obj <- NULL
  if (isTRUE(save_outputs)) {
    pa_path <- file.path(out_dir, paste0(run_id, "__parallel_scree.png"))
    save_png(pa_path, {
      pa_obj <<- run_parallel_analysis(
        items_ready,
        fm = fm,
        fa = "fa",
        n_iter = parallel_n_iter,
        cor_method = cor_method,
        use = efa_use,
        main = paste0("Parallel Analysis: ", run_id)
      )
    })
  } else {
    pa_obj <- run_parallel_analysis(
      items_ready,
      fm = fm,
      fa = "fa",
      n_iter = parallel_n_iter,
      cor_method = cor_method,
      use = efa_use,
      main = paste0("Parallel Analysis: ", run_id)
    )
  }
  
  # EFA diagram
  if (isTRUE(save_outputs)) {
    efa_diag_path <- file.path(out_dir, paste0(run_id, "__efa_diagram.png"))
    save_png(efa_diag_path, {
      psych::fa.diagram(efa, main = paste0("EFA Diagram: ", run_id))
    })
  }
  
  # EFA loadings table
  efa_loadings <- extract_efa_loadings(efa, cutoff = 0)
  if (isTRUE(save_outputs)) {
    write_csv_safely(efa_loadings, file.path(out_dir, paste0(run_id, "__efa_loadings.csv")))
  }
  
  # CFA model syntax
  model_syntax <- as_scalar_character(model)
  if (!nzchar(model_syntax) || is.na(model_syntax)) {
    model_syntax <- build_one_factor_model(names(items_ready), factor_name = "F1")
  }
  
  # CFA fit + summary
  cfa_fit <- perform_cfa(
    items_ready,
    model_syntax = model_syntax,
    std_lv = TRUE,
    estimator = cfa_estimator,
    missing_lavaan = cfa_missing
  )
  cfa_sum <- cfa_summary(cfa_fit, fit_measures = TRUE, standardized = TRUE, rsquare = TRUE)
  cfa_fit_meas <- extract_cfa_fit_measures(cfa_fit)
  
  if (isTRUE(save_outputs)) {
    write_csv_safely(cfa_fit_meas, file.path(out_dir, paste0(run_id, "__cfa_fit_measures.csv")))
    capture.output(cfa_sum, file = file.path(out_dir, paste0(run_id, "__cfa_summary.txt")))
  }
  
  # CFA path diagram
  if (isTRUE(save_outputs)) {
    cfa_path <- file.path(out_dir, paste0(run_id, "__cfa_path.png"))
    save_png(cfa_path, {
      semPlot::semPaths(
        cfa_fit,
        what = "std",
        whatLabels = "std",
        style = "lisrel",
        curveAdjacent = TRUE,
        layout = "tree",
        title = FALSE
      )
    })
  }
  
  # Return structured results
  list(
    run_id = run_id,
    pattern = as_scalar_character(pattern),
    n_items = ncol(items_ready),
    n_rows = nrow(items_ready),
    options = list(
      n_factors = as.integer(n_factors),
      rotation = rotation,
      fm = fm,
      cor_method = cor_method,
      standardize = standardize,
      missing_items = missing_items,
      cfa_estimator = cfa_estimator,
      cfa_missing = cfa_missing
    ),
    reliability = rel,
    efa = efa,
    efa_loadings = efa_loadings,
    parallel = pa_obj,
    cfa_fit = cfa_fit,
    cfa_fit_measures = cfa_fit_meas,
    cfa_summary = cfa_sum,
    output_dir = out_dir
  )
}

# -----------------------------
# Batch wrapper (analysis plan)
# -----------------------------

normalize_plan <- function(plan) {
  assert_columns(plan, c("pattern", "n_factors"))
  
  if (!("rotation" %in% names(plan))) plan$rotation <- "oblimin"
  if (!("model" %in% names(plan))) plan$model <- NA_character_
  if (!("scale_id" %in% names(plan))) plan$scale_id <- NA_character_
  
  plan
}

run_analysis_plan <- function(df,
                              plan,
                              output_dir = "fa_outputs",
                              install_missing_packages = FALSE,
                              save_outputs = TRUE,
                              quiet = FALSE,
                              ...) {
  required <- c("psych", "lavaan", "semPlot")
  ensure_packages(required, install = install_missing_packages)
  load_libraries(required)
  
  plan <- normalize_plan(plan)
  output_dir <- dir_create(output_dir)
  
  results <- vector("list", nrow(plan))
  names(results) <- paste0("run_", seq_len(nrow(plan)))
  
  for (i in seq_len(nrow(plan))) {
    row <- plan[i, , drop = FALSE]
    
    pattern <- row$pattern[[1]]
    n_factors <- row$n_factors[[1]]
    rotation <- row$rotation[[1]]
    model <- row$model[[1]]
    scale_id <- row$scale_id[[1]]
    
    res <- tryCatch(
      run_scale_analysis(
        df = df,
        pattern = pattern,
        n_factors = n_factors,
        rotation = rotation,
        model = model,
        scale_id = scale_id,
        output_dir = output_dir,
        save_outputs = save_outputs,
        quiet = quiet,
        ...
      ),
      error = function(e) {
        if (!quiet) message("Run failed (pattern = ", pattern, "): ", e$message)
        list(
          run_id = sanitize_id(ifelse(is.na(scale_id) || !nzchar(scale_id), as_scalar_character(pattern), as_scalar_character(scale_id))),
          pattern = as_scalar_character(pattern),
          error = e$message
        )
      }
    )
    
    results[[i]] <- res
  }
  
  results
}

summarize_results <- function(results) {
  rows <- lapply(results, function(x) {
    if (!is.null(x$error)) {
      return(data.frame(
        run_id = x$run_id %||% NA_character_,
        pattern = x$pattern %||% NA_character_,
        n_items = NA_integer_,
        n_rows = NA_integer_,
        raw_alpha = NA_real_,
        std_alpha = NA_real_,
        cfi = NA_real_,
        rmsea = NA_real_,
        srmr = NA_real_,
        error = x$error,
        stringsAsFactors = FALSE
      ))
    }
    
    cfi <- if (!is.null(x$cfa_fit_measures$cfi)) x$cfa_fit_measures$cfi[[1]] else NA_real_
    rmsea <- if (!is.null(x$cfa_fit_measures$rmsea)) x$cfa_fit_measures$rmsea[[1]] else NA_real_
    srmr <- if (!is.null(x$cfa_fit_measures$srmr)) x$cfa_fit_measures$srmr[[1]] else NA_real_
    
    data.frame(
      run_id = x$run_id,
      pattern = x$pattern,
      n_items = x$n_items,
      n_rows = x$n_rows,
      raw_alpha = x$reliability$raw_alpha,
      std_alpha = x$reliability$std_alpha,
      cfi = cfi,
      rmsea = rmsea,
      srmr = srmr,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------
# Example usage
# -----------------------------

# 1) Read data
# df <- read.csv("yourdata.csv")

# 2) Define an analysis plan (one row per run)
# plan <- data.frame(
#   scale_id = c("scale1_varimax_3f", "scale1_promax_4f", "scale2_varimax_2f", "scale2_promax_3f"),
#   pattern = c("scale1_pattern", "scale1_pattern", "scale2_pattern", "scale2_pattern"),
#   n_factors = c(3, 4, 2, 3),
#   rotation = c("varimax", "promax", "varimax", "promax"),
#   model = c(
#     "F1 =~ x1 + x2 + x3",
#     "F1 =~ x1 + x2 + x3 + x4",
#     NA,  # if NA/blank, a one-factor model is created from matched items
#     NA
#   ),
#   stringsAsFactors = FALSE
# )

# 3) Run the plan
# results <- run_analysis_plan(
#   df = df,
#   plan = plan,
#   output_dir = "fa_outputs",
#   save_outputs = TRUE,
#   missing_items = "pairwise",
#   standardize = "none",
#   cor_method = "cor",
#   fm = "minres",
#   cfa_missing = "fiml",
#   cfa_estimator = "MLR",
#   parallel_n_iter = 50,
#   seed = 123
# )

# 4) Create a compact summary table
# summary_tbl <- summarize_results(results)
# print(summary_tbl)
# write.csv(summary_tbl, file = file.path("fa_outputs", "summary.csv"), row.names = FALSE)

# Uncomment to run
# uat <- uat_run(verbose = TRUE)
# uat$report