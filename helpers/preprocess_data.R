################################################################################
# MAIN SCRIPT
# Description: Robust preprocessing pipeline with train-only fitting, consistent
# transforms applied to test/new data, safe handling of contrasts (including
# contr.ltfr), and stable encoding/imputation behavior.
################################################################################

# Global debug flag and logger function
DEBUG <- TRUE

log_debug <- function(fmt, ...) {
  if (isTRUE(DEBUG)) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] [DEBUG] %s\n", ts, sprintf(fmt, ...)))
  }
}

log_info <- function(fmt, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [INFO ] %s\n", ts, sprintf(fmt, ...)))
}

log_warn <- function(fmt, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [WARN ] %s\n", ts, sprintf(fmt, ...)))
}

# Ensure a missing custom contrast function cannot break model.matrix()
if (!exists("contr.ltfr", mode = "function")) {
  contr.ltfr <- function(n, contrasts = TRUE, sparse = FALSE) {
    stats::contr.treatment(n, contrasts = contrasts, sparse = sparse)
  }
}

# Force safe, standard contrasts for all formula/model-matrix operations
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))

# Package checks
require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing required packages: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

assert_df <- function(x, name = "data") {
  if (!is.data.frame(x)) stop(name, " must be a data.frame")
  invisible(TRUE)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  tab <- table(x)
  names(tab)[which.max(tab)][1]
}

is_binary_numeric <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  ux <- unique(x[!is.na(x)])
  length(ux) <= 2 && all(sort(ux) %in% c(0, 1))
}

# Safe contrasts wrapper
with_default_contrasts <- function(expr) {
  if (!exists("contr.ltfr", mode = "function")) {
    contr.ltfr <- function(n, contrasts = TRUE, sparse = FALSE) {
      stats::contr.treatment(n, contrasts = contrasts, sparse = sparse)
    }
  }
  options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
  eval.parent(substitute(expr))
}

# Date parsing and optional feature expansion
parse_date_best_effort <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(as.POSIXct(x))
  if (is.numeric(x)) {
    if (all(is.finite(x), na.rm = TRUE) && median(x, na.rm = TRUE) > 1e8) {
      return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    }
    return(as.POSIXct(rep(NA, length(x)), origin = "1970-01-01", tz = "UTC"))
  }
  x_chr <- as.character(x)
  fmts <- c(
    "%Y-%m-%d", "%Y/%m/%d",
    "%m/%d/%Y", "%d/%m/%Y",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ"
  )
  out <- as.POSIXct(rep(NA, length(x_chr)), origin = "1970-01-01", tz = "UTC")
  for (f in fmts) {
    idx <- is.na(out) & !is.na(x_chr)
    if (!any(idx)) break
    parsed <- as.POSIXct(x_chr[idx], format = f, tz = "UTC")
    out[idx] <- parsed
  }
  out
}

add_date_features <- function(data, date_vars, drop_original = TRUE) {
  assert_df(data, "data")
  if (is.null(date_vars) || !length(date_vars)) return(data)
  if (!all(date_vars %in% names(data))) stop("Some date_vars not found in data")
  
  for (v in date_vars) {
    dt <- parse_date_best_effort(data[[v]])
    data[[paste0(v, "_year")]]  <- as.integer(format(dt, "%Y"))
    data[[paste0(v, "_month")]] <- as.integer(format(dt, "%m"))
    data[[paste0(v, "_day")]]   <- as.integer(format(dt, "%d"))
    data[[paste0(v, "_wday")]]  <- as.integer(format(dt, "%u"))
    data[[paste0(v, "_yday")]]  <- as.integer(format(dt, "%j"))
    if (drop_original) data[[v]] <- NULL
  }
  data
}

### ============================================================
### Helper: Remove Constant or Nearly Constant Columns
### ============================================================
remove_constant_columns <- function(data, tol_var = 1e-8) {
  assert_df(data, "data")
  removed <- list()
  keep <- character(0)
  
  for (col in names(data)) {
    vec <- data[[col]]
    if (is.numeric(vec)) {
      var_val <- stats::var(vec, na.rm = TRUE)
      if (is.na(var_val) || var_val < tol_var) {
        removed[[col]] <- list(type = "numeric", value = mean(vec, na.rm = TRUE))
      } else {
        keep <- c(keep, col)
      }
    } else if (is.factor(vec)) {
      u <- unique(vec[!is.na(vec)])
      if (length(u) < 2) {
        removed[[col]] <- list(type = "factor", value = as.character(u)[1], levels = levels(vec))
      } else {
        keep <- c(keep, col)
      }
    } else if (is.logical(vec)) {
      u <- unique(vec[!is.na(vec)])
      if (length(u) < 2) {
        removed[[col]] <- list(type = "logical", value = if (length(u)) u[1] else FALSE)
      } else {
        keep <- c(keep, col)
      }
    } else {
      keep <- c(keep, col)
    }
  }
  
  list(new_data = data[, keep, drop = FALSE], removed = removed)
}

# Reinserts only numeric constants; constant categorical columns are intentionally not reintroduced
reinsert_constant_columns <- function(data, removed) {
  assert_df(data, "data")
  if (!length(removed)) return(data)
  
  n <- nrow(data)
  for (col in names(removed)) {
    meta <- removed[[col]]
    if (identical(meta$type, "numeric")) {
      data[[col]] <- rep(meta$value, n)
    }
  }
  data
}

### ============================================================
### Helper: Deterministic imputation model
### ============================================================
fit_imputer_model <- function(train, exclude_cols = NULL) {
  assert_df(train, "train")
  cols <- setdiff(names(train), exclude_cols %||% character(0))
  model <- list()
  
  for (col in cols) {
    x <- train[[col]]
    if (is.numeric(x)) {
      model[[col]] <- list(type = "numeric", value = stats::median(x, na.rm = TRUE))
    } else if (is.factor(x)) {
      model[[col]] <- list(type = "factor", value = mode_value(x), levels = levels(x))
    } else if (is.logical(x)) {
      model[[col]] <- list(type = "logical", value = mode_value(x))
    } else {
      model[[col]] <- list(type = "other", value = mode_value(x))
    }
  }
  model
}

apply_imputer_model <- function(data, model) {
  assert_df(data, "data")
  for (col in names(model)) {
    if (!col %in% names(data)) next
    if (!anyNA(data[[col]])) next
    
    m <- model[[col]]
    if (m$type == "numeric") {
      if (!is.numeric(data[[col]])) data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      data[[col]][is.na(data[[col]])] <- m$value
    } else if (m$type == "factor") {
      x <- data[[col]]
      if (!is.factor(x)) x <- factor(as.character(x), levels = m$levels)
      xc <- as.character(x)
      xc[!(xc %in% m$levels)] <- NA
      xc[is.na(xc)] <- m$value
      data[[col]] <- factor(xc, levels = m$levels)
    } else if (m$type == "logical") {
      x <- data[[col]]
      if (!is.logical(x)) x <- as.logical(x)
      x[is.na(x)] <- m$value
      data[[col]] <- x
    } else {
      x <- data[[col]]
      x[is.na(x)] <- m$value
      data[[col]] <- x
    }
  }
  data
}

### ============================================================
### 1. Impute Missing Data (train-only modeling when test_data supplied)
### ============================================================
impute_missing_data <- function(data,
                                numeric_method = "pmm",
                                binary_factor_method = "logreg",
                                multiclass_factor_method = "polyreg",
                                m = 5,
                                maxit = 20,
                                seed = 500,
                                suppress_warnings = TRUE,
                                parallel = FALSE,
                                ncores = max(1L, parallel::detectCores() - 1L),
                                test_data = NULL,
                                outcome_var = NULL,
                                use_outcome_as_predictor = FALSE,
                                return_model = FALSE,
                                tol_var = 1e-8) {
  require_pkgs(c("mice", "parallel"))
  assert_df(data, "data")
  if (!is.null(test_data)) assert_df(test_data, "test_data")
  if (!is.null(outcome_var) && !outcome_var %in% names(data)) stop("outcome_var not found in data")
  
  # Ensure characters become factors (mice logreg/polyreg expect factors)
  for (col in names(data)) {
    if (is.character(data[[col]])) data[[col]] <- factor(data[[col]])
  }
  if (!is.null(test_data)) {
    for (col in names(test_data)) {
      if (is.character(test_data[[col]])) test_data[[col]] <- factor(test_data[[col]])
    }
  }
  
  # Align factor levels between train and test using training levels
  if (!is.null(test_data)) {
    shared <- intersect(names(data), names(test_data))
    for (col in shared) {
      if (is.factor(data[[col]])) {
        lvl <- levels(data[[col]])
        x <- as.character(test_data[[col]])
        x[!(x %in% lvl)] <- NA
        test_data[[col]] <- factor(x, levels = lvl)
      }
    }
  }
  
  # Remove constant/nearly constant columns (helps mice stability)
  rc_train <- remove_constant_columns(data, tol_var = tol_var)
  train_for_impute <- rc_train$new_data
  removed_cols <- rc_train$removed
  
  test_for_impute <- NULL
  if (!is.null(test_data)) {
    test_for_impute <- test_data[, names(train_for_impute), drop = FALSE]
  }
  
  exclude_for_model <- outcome_var %||% character(0)
  det_model <- fit_imputer_model(train_for_impute, exclude_cols = exclude_for_model)
  
  if (!anyNA(train_for_impute) && (is.null(test_for_impute) || !anyNA(test_for_impute))) {
    train_done <- apply_imputer_model(train_for_impute, det_model)
    train_done <- reinsert_constant_columns(train_done, removed_cols)
    
    if (!is.null(test_for_impute)) {
      test_done <- apply_imputer_model(test_for_impute, det_model)
      test_done <- reinsert_constant_columns(test_done, removed_cols)
      out <- list(train = train_done, test = test_done)
      if (return_model) out$model <- det_model
      return(out)
    }
    if (return_model) return(list(data = train_done, model = det_model))
    return(train_done)
  }
  
  # Build mice methods
  methods <- rep("", ncol(train_for_impute))
  names(methods) <- names(train_for_impute)
  
  for (nm in names(train_for_impute)) {
    x <- train_for_impute[[nm]]
    if (is.numeric(x)) {
      methods[nm] <- numeric_method
    } else if (is.factor(x)) {
      methods[nm] <- if (nlevels(x) == 2) binary_factor_method else multiclass_factor_method
    } else if (is.logical(x)) {
      train_for_impute[[nm]] <- factor(x)
      if (!is.null(test_for_impute)) test_for_impute[[nm]] <- factor(test_for_impute[[nm]])
      methods[nm] <- binary_factor_method
    } else {
      methods[nm] <- ""
    }
  }
  
  combined <- if (is.null(test_for_impute)) train_for_impute else rbind(train_for_impute, test_for_impute)
  n_train <- nrow(train_for_impute)
  n_total <- nrow(combined)
  
  if (!is.null(outcome_var) && outcome_var %in% names(combined)) {
    methods[outcome_var] <- ""
  }
  
  pred <- mice::make.predictorMatrix(combined)
  if (!is.null(outcome_var) && outcome_var %in% colnames(pred)) {
    pred[outcome_var, ] <- 0
    pred[, outcome_var] <- if (isTRUE(use_outcome_as_predictor)) 1 else 0
    pred[outcome_var, outcome_var] <- 0
  }
  
  ignore <- rep(FALSE, n_total)
  if (!is.null(test_for_impute)) ignore[(n_train + 1):n_total] <- TRUE
  
  set.seed(seed)
  log_debug("Imputation: n_train=%d, n_test=%d, m=%d, maxit=%d, parallel=%s",
            n_train, ifelse(is.null(test_for_impute), 0L, nrow(test_for_impute)), m, maxit, parallel)
  
  mids <- try({
    if (parallel) {
      if (exists("futuremice", where = asNamespace("mice"), inherits = FALSE) &&
          requireNamespace("future", quietly = TRUE)) {
        
        old_plan <- future::plan()
        on.exit(future::plan(old_plan), add = TRUE)
        future::plan(future::multisession, workers = ncores)
        
        with_default_contrasts({
          mice::futuremice(
            combined,
            m = m,
            maxit = maxit,
            method = methods,
            predictorMatrix = pred,
            ignore = ignore,
            parallelseed = seed,
            n.core = ncores,
            printFlag = FALSE
          )
        })
        
      } else {
        if (!exists("parlmice", where = asNamespace("mice"), inherits = FALSE)) {
          stop("mice::parlmice not available in this mice version.")
        }
        
        cl <- parallel::makeCluster(ncores)
        on.exit(try(suppressWarnings(parallel::stopCluster(cl)), silent = TRUE), add = TRUE)
        
        parallel::clusterEvalQ(cl, {
          library(mice)
          if (!exists("contr.ltfr", mode = "function")) {
            contr.ltfr <- function(n, contrasts = TRUE, sparse = FALSE) {
              stats::contr.treatment(n, contrasts = contrasts, sparse = sparse)
            }
          }
          options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
        })
        
        with_default_contrasts({
          mice::parlmice(
            combined,
            m = m,
            maxit = maxit,
            method = methods,
            predictorMatrix = pred,
            ignore = ignore,
            seed = seed,
            printFlag = FALSE,
            cluster = cl
          )
        })
      }
    } else {
      with_default_contrasts({
        if (suppress_warnings) {
          suppressWarnings(mice::mice(combined,
                                      m = m,
                                      maxit = maxit,
                                      method = methods,
                                      predictorMatrix = pred,
                                      ignore = ignore,
                                      seed = seed,
                                      printFlag = FALSE))
        } else {
          mice::mice(combined,
                     m = m,
                     maxit = maxit,
                     method = methods,
                     predictorMatrix = pred,
                     ignore = ignore,
                     seed = seed,
                     printFlag = FALSE)
        }
      })
    }
  }, silent = TRUE)
  
  if (inherits(mids, "try-error")) {
    log_warn("Imputation using mice did not run; using deterministic median/mode imputation.")
    train_done <- apply_imputer_model(train_for_impute, det_model)
    if (!is.null(test_for_impute)) test_done <- apply_imputer_model(test_for_impute, det_model)
    
    train_done <- reinsert_constant_columns(train_done, removed_cols)
    if (!is.null(test_for_impute)) test_done <- reinsert_constant_columns(test_done, removed_cols)
    
    if (!is.null(test_for_impute)) {
      out <- list(train = train_done, test = test_done)
      if (return_model) out$model <- det_model
      return(out)
    }
    if (return_model) return(list(data = train_done, model = det_model))
    return(train_done)
  }
  
  completed <- mice::complete(mids, 1)
  train_done <- completed[1:n_train, , drop = FALSE]
  test_done <- if (is.null(test_for_impute)) NULL else completed[(n_train + 1):n_total, , drop = FALSE]
  
  train_done <- apply_imputer_model(train_done, det_model)
  if (!is.null(test_done)) test_done <- apply_imputer_model(test_done, det_model)
  
  train_done <- reinsert_constant_columns(train_done, removed_cols)
  if (!is.null(test_done)) test_done <- reinsert_constant_columns(test_done, removed_cols)
  
  if (!is.null(test_for_impute)) {
    out <- list(train = train_done, test = test_done)
    if (return_model) out$model <- det_model
    return(out)
  }
  
  if (return_model) return(list(data = train_done, model = det_model))
  train_done
}

### ============================================================
### 2. Scaling Functions
### ============================================================
fit_scaler <- function(data, scaling_method = "standard",
                       exclude_cols = NULL,
                       scale_binary = FALSE) {
  assert_df(data, "data")
  exclude_cols <- exclude_cols %||% character(0)
  
  cols <- setdiff(names(data), exclude_cols)
  numeric_cols <- cols[vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
  
  if (!scale_binary) {
    numeric_cols <- numeric_cols[!vapply(data[, numeric_cols, drop = FALSE], is_binary_numeric, logical(1))]
  }
  
  params <- list()
  if (scaling_method == "standard") {
    for (col in numeric_cols) {
      params[[col]] <- list(mean = mean(data[[col]], na.rm = TRUE),
                            sd   = stats::sd(data[[col]], na.rm = TRUE))
    }
  } else if (scaling_method == "minmax") {
    for (col in numeric_cols) {
      params[[col]] <- list(min = min(data[[col]], na.rm = TRUE),
                            max = max(data[[col]], na.rm = TRUE))
    }
  } else {
    stop("Unsupported scaling_method. Use 'standard' or 'minmax'.")
  }
  
  log_debug("Scaler fit: method=%s, numeric_cols=%d", scaling_method, length(numeric_cols))
  list(params = params, numeric_cols = numeric_cols, method = scaling_method)
}

apply_scaler <- function(data, scaler) {
  assert_df(data, "data")
  for (col in scaler$numeric_cols) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) next
    
    if (scaler$method == "standard") {
      mean_val <- scaler$params[[col]]$mean
      sd_val <- scaler$params[[col]]$sd
      if (isTRUE(all.equal(sd_val, 0))) {
        data[[col]] <- data[[col]] - mean_val
      } else {
        data[[col]] <- (data[[col]] - mean_val) / sd_val
      }
    } else if (scaler$method == "minmax") {
      min_val <- scaler$params[[col]]$min
      max_val <- scaler$params[[col]]$max
      if (isTRUE(all.equal(max_val, min_val))) {
        data[[col]] <- data[[col]] - min_val
      } else {
        data[[col]] <- as.numeric((data[[col]] - min_val) / (max_val - min_val))
      }
    }
  }
  data
}

### ============================================================
### 3. One–Hot Encoding
### ============================================================
fit_one_hot_encoder <- function(data, outcome_var, fullRank = FALSE) {
  require_pkgs(c("caret"))
  assert_df(data, "data")
  if (!outcome_var %in% names(data)) stop("outcome_var not found in data")
  
  predictors <- setdiff(names(data), outcome_var)
  
  for (col in predictors) {
    if (is.character(data[[col]])) data[[col]] <- factor(data[[col]])
  }
  
  drop_cols <- character(0)
  for (col in predictors) {
    x <- data[[col]]
    if (is.factor(x)) {
      obs <- unique(as.character(x[!is.na(x)]))
      if (length(obs) < 2) drop_cols <- c(drop_cols, col)
    }
  }
  
  if (length(drop_cols)) {
    log_warn("One-hot encoding: dropping %d categorical predictor(s) with <2 observed levels: %s",
             length(drop_cols), paste(drop_cols, collapse = ", "))
    data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  }
  
  formula <- stats::as.formula(paste("~ . -", outcome_var))
  dummy_model <- with_default_contrasts(
    caret::dummyVars(formula, data = data, fullRank = fullRank)
  )
  attr(dummy_model, "dropped_cols") <- drop_cols
  dummy_model
}

apply_one_hot_encoder <- function(data, dummy_model, outcome_var) {
  assert_df(data, "data")
  
  drop_cols <- attr(dummy_model, "dropped_cols") %||% character(0)
  if (length(drop_cols)) {
    data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  }
  
  has_outcome <- outcome_var %in% names(data)
  outcome <- if (has_outcome) data[[outcome_var]] else NULL
  
  if (!is.null(dummy_model$lvls) && length(dummy_model$lvls)) {
    for (nm in names(dummy_model$lvls)) {
      if (!nm %in% names(data)) next
      train_lvls <- dummy_model$lvls[[nm]]
      x <- data[[nm]]
      if (is.character(x)) x <- factor(x)
      if (is.factor(x)) {
        xc <- as.character(x)
        xc[!(xc %in% train_lvls)] <- NA
        data[[nm]] <- factor(xc, levels = train_lvls)
      }
    }
  }
  
  features_encoded <- with_default_contrasts(
    stats::predict(dummy_model, newdata = data)
  )
  features_encoded <- as.data.frame(features_encoded)
  
  if (has_outcome) features_encoded[[outcome_var]] <- outcome
  features_encoded
}

### ============================================================
### 4. Outlier Removal
### ============================================================
fit_outlier_removal <- function(data, multiplier = 1.5,
                                exclude_cols = NULL,
                                ignore_binary = TRUE) {
  assert_df(data, "data")
  exclude_cols <- exclude_cols %||% character(0)
  
  cols <- setdiff(names(data), exclude_cols)
  numeric_cols <- cols[vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
  if (ignore_binary) {
    numeric_cols <- numeric_cols[!vapply(data[, numeric_cols, drop = FALSE], is_binary_numeric, logical(1))]
  }
  
  thresholds <- list()
  for (col in numeric_cols) {
    x <- data[[col]]
    qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
    IQR_val <- qnt[2] - qnt[1]
    H <- multiplier * IQR_val
    thresholds[[col]] <- c(lower = qnt[1] - H, upper = qnt[2] + H)
  }
  
  log_debug("Outlier thresholds fit: numeric_cols=%d", length(numeric_cols))
  thresholds
}

apply_outlier_removal <- function(data, thresholds) {
  assert_df(data, "data")
  for (col in names(thresholds)) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) next
    lower <- thresholds[[col]]["lower"]
    upper <- thresholds[[col]]["upper"]
    idx <- which(data[[col]] < lower | data[[col]] > upper)
    if (length(idx)) data[[col]][idx] <- NA
  }
  data
}

### ============================================================
### 5. Generate Interaction Terms
### ============================================================
generate_interaction_terms <- function(data, degree = 2,
                                       exclude_cols = NULL,
                                       include_binary = FALSE,
                                       max_numeric_cols = 50L) {
  assert_df(data, "data")
  if (degree != 2) stop("Only pairwise (degree = 2) interactions are supported.")
  exclude_cols <- exclude_cols %||% character(0)
  
  cols <- setdiff(names(data), exclude_cols)
  numeric_cols <- cols[vapply(data[, cols, drop = FALSE], is.numeric, logical(1))]
  
  if (!include_binary) {
    numeric_cols <- numeric_cols[!vapply(data[, numeric_cols, drop = FALSE], is_binary_numeric, logical(1))]
  }
  
  if (length(numeric_cols) < 2) return(data)
  
  if (length(numeric_cols) > max_numeric_cols) {
    log_warn("Too many numeric columns for interactions (%d). Using first %d.",
             length(numeric_cols), max_numeric_cols)
    numeric_cols <- numeric_cols[seq_len(max_numeric_cols)]
  }
  
  interactions <- list()
  n <- length(numeric_cols)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      new_col_name <- paste(numeric_cols[i], numeric_cols[j], sep = "_x_")
      interactions[[new_col_name]] <- data[[numeric_cols[i]]] * data[[numeric_cols[j]]]
    }
  }
  
  cbind(data, as.data.frame(interactions))
}

### ============================================================
### Helper: Near-zero variance pruning
### ============================================================
fit_nzv_filter <- function(data, exclude_cols = NULL,
                           freqCut = 95/5, uniqueCut = 10) {
  require_pkgs(c("caret"))
  assert_df(data, "data")
  exclude_cols <- exclude_cols %||% character(0)
  
  cols <- setdiff(names(data), exclude_cols)
  if (!length(cols)) return(list(drop_cols = character(0)))
  
  nzv <- with_default_contrasts(
    caret::nearZeroVar(data[, cols, drop = FALSE],
                       saveMetrics = TRUE,
                       freqCut = freqCut,
                       uniqueCut = uniqueCut)
  )
  drop_cols <- rownames(nzv)[nzv$nzv]
  list(drop_cols = drop_cols)
}

apply_nzv_filter <- function(data, nzv_fit) {
  assert_df(data, "data")
  drop_cols <- intersect(names(data), nzv_fit$drop_cols)
  if (length(drop_cols)) {
    data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
  }
  data
}

### ============================================================
### Helper: Feature selection implementations
### ============================================================
select_features_ranger <- function(train, outcome_var,
                                   num_selected_features = 20,
                                   num_trees = 200,
                                   seed = 500) {
  require_pkgs(c("ranger"))
  assert_df(train, "train")
  if (!outcome_var %in% names(train)) stop("outcome_var not found in train")
  
  predictors <- setdiff(names(train), outcome_var)
  if (!length(predictors)) return(character(0))
  
  form <- stats::as.formula(paste(outcome_var, "~", paste(predictors, collapse = " + ")))
  
  set.seed(seed)
  rf <- with_default_contrasts(
    ranger::ranger(
      formula = form,
      data = train,
      num.trees = num_trees,
      importance = "impurity",
      seed = seed
    )
  )
  
  imp <- rf$variable.importance
  imp <- sort(imp, decreasing = TRUE)
  names(imp)[seq_len(min(num_selected_features, length(imp)))]
}

select_features_boruta <- function(train, outcome_var,
                                   num_selected_features = 20,
                                   seed = 500,
                                   maxRuns = 100) {
  require_pkgs(c("Boruta"))
  assert_df(train, "train")
  if (!outcome_var %in% names(train)) stop("outcome_var not found in train")
  
  set.seed(seed)
  form <- stats::as.formula(paste(outcome_var, "~ ."))
  
  b <- with_default_contrasts(
    Boruta::Boruta(form, data = train, doTrace = 0, maxRuns = maxRuns)
  )
  
  # Call TentativeRoughFix() only if there are tentative features
  if (!is.null(b$finalDecision) && any(b$finalDecision == "Tentative", na.rm = TRUE)) {
    b <- Boruta::TentativeRoughFix(b)
  }
  
  sel <- Boruta::getSelectedAttributes(b, withTentative = FALSE)
  if (!length(sel)) {
    log_warn("Boruta selected 0 features; using all predictors.")
    sel <- setdiff(names(train), outcome_var)
  }
  
  if (length(sel) > num_selected_features) sel <- sel[seq_len(num_selected_features)]
  sel
}

select_features_lasso <- function(train, outcome_var,
                                  num_selected_features = 20,
                                  seed = 500,
                                  alpha = 1,
                                  lambda_choice = c("lambda.1se", "lambda.min")) {
  require_pkgs(c("glmnet"))
  assert_df(train, "train")
  if (!outcome_var %in% names(train)) stop("outcome_var not found in train")
  lambda_choice <- match.arg(lambda_choice)
  
  y <- train[[outcome_var]]
  X <- train[, setdiff(names(train), outcome_var), drop = FALSE]
  X <- as.matrix(X)
  
  family <- "gaussian"
  if (is.factor(y)) {
    if (nlevels(y) == 2) family <- "binomial" else family <- "multinomial"
  }
  
  set.seed(seed)
  cv <- glmnet::cv.glmnet(X, y, family = family, alpha = alpha, standardize = TRUE)
  
  lambda <- if (lambda_choice == "lambda.1se") cv$lambda.1se else cv$lambda.min
  
  if (family == "multinomial") {
    coefs <- glmnet::coef.glmnet(cv$glmnet.fit, s = lambda)
    nz <- unique(unlist(lapply(coefs, function(m) rownames(m)[as.numeric(m) != 0])))
    nz <- setdiff(nz, "(Intercept)")
  } else {
    coefs <- glmnet::coef.glmnet(cv$glmnet.fit, s = lambda)
    nz <- rownames(coefs)[as.numeric(coefs) != 0]
    nz <- setdiff(nz, "(Intercept)")
  }
  
  sel <- intersect(colnames(X), nz)
  if (!length(sel)) {
    log_warn("LASSO selected 0 features; using all predictors.")
    sel <- colnames(X)
  }
  
  if (length(sel) > num_selected_features) sel <- sel[seq_len(num_selected_features)]
  sel
}

### ============================================================
### 6. Apply Transformations to Data
### ============================================================
apply_transformations <- function(data, preproc_params) {
  assert_df(data, "data")
  if (is.null(preproc_params) || !is.list(preproc_params)) stop("preproc_params must be a list")
  
  outcome_var <- preproc_params$outcome_var %||% NULL
  
  if (!is.null(outcome_var) &&
      !is.null(preproc_params$outcome_encoding) &&
      outcome_var %in% names(data)) {
    enc <- preproc_params$outcome_encoding
    data[[outcome_var]] <- as.numeric(enc[as.character(data[[outcome_var]])])
  }
  
  if (!is.null(preproc_params$date_vars)) {
    data <- add_date_features(
      data,
      date_vars = preproc_params$date_vars,
      drop_original = isTRUE(preproc_params$drop_date_original %||% TRUE)
    )
  }
  
  if (!is.null(preproc_params$outlier_thresholds)) {
    data <- apply_outlier_removal(data, preproc_params$outlier_thresholds)
  }
  
  if (!is.null(preproc_params$imputer_model)) {
    data <- apply_imputer_model(data, preproc_params$imputer_model)
  }
  
  if (!is.null(preproc_params$dummy_model)) {
    data <- apply_one_hot_encoder(data, preproc_params$dummy_model, outcome_var = outcome_var)
  }
  
  if (!is.null(preproc_params$nzv_fit)) {
    data <- apply_nzv_filter(data, preproc_params$nzv_fit)
  }
  
  if (!is.null(preproc_params$scaler)) {
    data <- apply_scaler(data, preproc_params$scaler)
  }
  
  if (!is.null(preproc_params$interaction_degree) && preproc_params$interaction_degree > 1) {
    data <- generate_interaction_terms(
      data,
      degree = preproc_params$interaction_degree,
      exclude_cols = outcome_var,
      include_binary = isTRUE(preproc_params$interactions_include_binary %||% FALSE),
      max_numeric_cols = preproc_params$interactions_max_numeric_cols %||% 50L
    )
  }
  
  if (!is.null(preproc_params$custom_transform) && is.function(preproc_params$custom_transform)) {
    data <- preproc_params$custom_transform(data)
  }
  
  if (!is.null(preproc_params$selected_features) && length(preproc_params$selected_features)) {
    keep <- intersect(preproc_params$selected_features, names(data))
    if (!is.null(outcome_var) && outcome_var %in% names(data)) keep <- c(keep, outcome_var)
    data <- data[, keep, drop = FALSE]
  }
  
  as.data.frame(data)
}

### ============================================================
### 7. Comprehensive Preprocessing Pipeline with Feature Selection
### ============================================================
preprocess_data <- function(data,
                            outcome_var,
                            partition_ratio = 0.7,
                            date_vars = NULL,
                            outlier_multiplier = 1.5,
                            interaction_degree = 2,
                            custom_transform = NULL,
                            feature_selection = TRUE,
                            feature_selection_method = "ranger",  # "ranger", "Boruta", "LASSO"
                            num_selected_features = 20,
                            ordinal_encoding = FALSE,
                            scale_data_flag = TRUE,
                            scaling_method = "standard",
                            numeric_impute_method = "pmm",
                            binary_factor_impute_method = "logreg",
                            multiclass_factor_impute_method = "polyreg",
                            m = 5,
                            maxit = 20,
                            seed = 500,
                            one_hot_fullRank = FALSE,
                            impute_parallel = FALSE,
                            impute_ncores = max(1L, parallel::detectCores() - 1L),
                            use_outcome_as_predictor_in_impute = FALSE,
                            drop_date_original = TRUE,
                            scale_binary = FALSE,
                            nzv_after_onehot = TRUE,
                            interactions_include_binary = FALSE,
                            interactions_max_numeric_cols = 50L,
                            lasso_lambda_choice = c("lambda.1se", "lambda.min")) {
  
  require_pkgs(c("caret", "parallel"))
  assert_df(data, "data")
  if (!outcome_var %in% names(data)) stop("outcome_var not found in data")
  if (!is.null(date_vars) && !all(date_vars %in% names(data))) stop("Some date_vars not found in data")
  
  set.seed(seed)
  
  outcome_encoding <- NULL
  if (ordinal_encoding) {
    y <- data[[outcome_var]]
    if (!is.factor(y)) y <- factor(y)
    lvls <- levels(y)
    outcome_encoding <- setNames(seq_along(lvls), lvls)
    data[[outcome_var]] <- as.numeric(outcome_encoding[as.character(y)])
  }
  
  trainIndex <- caret::createDataPartition(data[[outcome_var]], p = partition_ratio, list = FALSE)
  train <- data[trainIndex, , drop = FALSE]
  test  <- data[-trainIndex, , drop = FALSE]
  log_info("Split: train=%d rows, test=%d rows", nrow(train), nrow(test))
  
  if (!is.null(date_vars)) {
    train <- add_date_features(train, date_vars = date_vars, drop_original = drop_date_original)
    test  <- add_date_features(test,  date_vars = date_vars, drop_original = drop_date_original)
  }
  
  outlier_thresholds <- fit_outlier_removal(train, multiplier = outlier_multiplier, exclude_cols = outcome_var)
  train <- apply_outlier_removal(train, outlier_thresholds)
  test  <- apply_outlier_removal(test,  outlier_thresholds)
  
  imp <- impute_missing_data(
    data = train,
    test_data = test,
    outcome_var = outcome_var,
    numeric_method = numeric_impute_method,
    binary_factor_method = binary_factor_impute_method,
    multiclass_factor_method = multiclass_factor_impute_method,
    m = m,
    maxit = maxit,
    seed = seed,
    parallel = impute_parallel,
    ncores = impute_ncores,
    use_outcome_as_predictor = use_outcome_as_predictor_in_impute,
    return_model = TRUE
  )
  train <- imp$train
  test  <- imp$test
  imputer_model <- imp$model
  
  dummy_model <- fit_one_hot_encoder(train, outcome_var, fullRank = one_hot_fullRank)
  train <- apply_one_hot_encoder(train, dummy_model, outcome_var)
  test  <- apply_one_hot_encoder(test,  dummy_model, outcome_var)
  
  nzv_fit <- NULL
  if (nzv_after_onehot) {
    nzv_fit <- fit_nzv_filter(train, exclude_cols = outcome_var)
    train <- apply_nzv_filter(train, nzv_fit)
    test  <- apply_nzv_filter(test,  nzv_fit)
  }
  
  scaler <- NULL
  if (scale_data_flag) {
    scaler <- fit_scaler(train, scaling_method = scaling_method, exclude_cols = outcome_var, scale_binary = scale_binary)
    train <- apply_scaler(train, scaler)
    test  <- apply_scaler(test,  scaler)
  }
  
  if (!is.null(interaction_degree) && interaction_degree > 1) {
    train <- generate_interaction_terms(
      train,
      degree = interaction_degree,
      exclude_cols = outcome_var,
      include_binary = interactions_include_binary,
      max_numeric_cols = interactions_max_numeric_cols
    )
    test <- generate_interaction_terms(
      test,
      degree = interaction_degree,
      exclude_cols = outcome_var,
      include_binary = interactions_include_binary,
      max_numeric_cols = interactions_max_numeric_cols
    )
  }
  
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    train <- custom_transform(train)
    test  <- custom_transform(test)
  }
  
  selected_features <- setdiff(names(train), outcome_var)
  
  if (isTRUE(feature_selection)) {
    method <- feature_selection_method
    if (!method %in% c("ranger", "Boruta", "LASSO")) stop("feature_selection_method must be one of: ranger, Boruta, LASSO")
    
    if (method == "ranger") {
      log_debug("Feature selection: ranger")
      selected_features <- select_features_ranger(
        train,
        outcome_var = outcome_var,
        num_selected_features = num_selected_features,
        seed = seed
      )
    } else if (method == "Boruta") {
      log_debug("Feature selection: Boruta")
      selected_features <- select_features_boruta(
        train,
        outcome_var = outcome_var,
        num_selected_features = num_selected_features,
        seed = seed
      )
    } else if (method == "LASSO") {
      log_debug("Feature selection: LASSO")
      lasso_lambda_choice <- match.arg(lasso_lambda_choice)
      selected_features <- select_features_lasso(
        train,
        outcome_var = outcome_var,
        num_selected_features = num_selected_features,
        seed = seed,
        lambda_choice = lasso_lambda_choice
      )
    }
    
    keep <- c(intersect(selected_features, names(train)), outcome_var)
    train <- train[, keep, drop = FALSE]
    test  <- test[,  keep, drop = FALSE]
  }
  
  preproc_params <- list(
    outcome_var = outcome_var,
    outcome_encoding = outcome_encoding,
    date_vars = date_vars,
    drop_date_original = drop_date_original,
    outlier_thresholds = outlier_thresholds,
    imputer_model = imputer_model,
    dummy_model = dummy_model,
    nzv_fit = nzv_fit,
    scaler = scaler,
    interaction_degree = interaction_degree,
    interactions_include_binary = interactions_include_binary,
    interactions_max_numeric_cols = interactions_max_numeric_cols,
    custom_transform = custom_transform,
    selected_features = selected_features,
    seed = seed
  )
  
  log_info("Done. Train: %d x %d | Test: %d x %d", nrow(train), ncol(train), nrow(test), ncol(test))
  list(train = train, test = test, preproc_params = preproc_params)
}


################################################################################
# UAT SCRIPT
# Description: End-to-end checks for the preprocessing functions and pipeline.
# Usage:
#   1) Source your main script first (the one defining preprocess_data()).
#   2) Paste/run this file in RStudio, then run: uat_run_all()
################################################################################

# Optional: mute debug if you want quieter output
# DEBUG <- FALSE

uat_require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing required packages for UAT: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

uat_has_pkgs <- function(pkgs) {
  all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
}

# Simple assertion framework
.uat_state <- new.env(parent = emptyenv())
.uat_state$results <- data.frame(
  test = character(0),
  status = character(0),
  message = character(0),
  stringsAsFactors = FALSE
)

uat_record <- function(test_name, status, message = "") {
  .uat_state$results <- rbind(
    .uat_state$results,
    data.frame(test = test_name, status = status, message = message, stringsAsFactors = FALSE)
  )
  invisible(NULL)
}

uat_pass <- function(test_name, message = "") uat_record(test_name, "PASS", message)
uat_fail <- function(test_name, message = "") uat_record(test_name, "FAIL", message)
uat_skip <- function(test_name, message = "") uat_record(test_name, "SKIP", message)

uat_assert <- function(cond, test_name, message_fail, message_pass = "") {
  if (isTRUE(cond)) uat_pass(test_name, message_pass) else uat_fail(test_name, message_fail)
  invisible(isTRUE(cond))
}

uat_try <- function(expr, test_name) {
  out <- try(eval.parent(substitute(expr)), silent = TRUE)
  if (inherits(out, "try-error")) {
    uat_fail(test_name, as.character(out))
    return(list(ok = FALSE, value = NULL))
  }
  uat_pass(test_name)
  list(ok = TRUE, value = out)
}

uat_summary <- function() {
  res <- .uat_state$results
  cat("\n==================== UAT SUMMARY ====================\n")
  print(res, row.names = FALSE)
  cat("-----------------------------------------------------\n")
  cat(sprintf("PASS: %d | FAIL: %d | SKIP: %d | TOTAL: %d\n",
              sum(res$status == "PASS"),
              sum(res$status == "FAIL"),
              sum(res$status == "SKIP"),
              nrow(res)))
  cat("=====================================================\n\n")
  invisible(res)
}

# Data generators
uat_make_regression_data <- function(n = 500, seed = 123) {
  set.seed(seed)
  
  x1 <- rnorm(n)
  x2 <- rnorm(n, mean = 1)
  x3 <- runif(n, -2, 2)
  x4 <- rnorm(n)
  x5 <- rnorm(n)
  x6 <- rnorm(n)
  x7 <- rnorm(n)
  x8 <- rnorm(n)
  x9 <- rnorm(n)
  x10 <- rnorm(n)
  
  f3 <- factor(sample(c("A", "B", "C"), n, replace = TRUE), levels = c("A", "B", "C"))
  f2 <- factor(sample(c("no", "yes"), n, replace = TRUE), levels = c("no", "yes"))
  
  dt <- as.Date("2022-01-01") + sample(0:900, n, replace = TRUE)
  
  y <- 3*x1 - 2*x2 + 0.8*(x3^2) + ifelse(f3 == "B", 1.5, 0) + rnorm(n, sd = 0.6)
  
  df <- data.frame(
    y = y,
    x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
    x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10,
    f3 = f3,
    f2 = f2,
    dt = dt,
    const_num = 5,
    const_fac = factor("K", levels = c("K")),
    stringsAsFactors = FALSE
  )
  
  # Missingness
  miss_cols <- c("x1", "x3", "x8", "f3", "f2")
  for (cname in miss_cols) {
    idx <- sample(seq_len(n), size = floor(0.08 * n))
    df[idx, cname] <- NA
  }
  
  # Outliers
  out_idx <- sample(seq_len(n), size = floor(0.02 * n))
  df$x2[out_idx] <- df$x2[out_idx] + rnorm(length(out_idx), mean = 0, sd = 20)
  
  df
}

uat_make_binary_classification_data <- function(n = 600, seed = 456) {
  set.seed(seed)
  
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- runif(n, -3, 3)
  x5 <- rnorm(n)
  
  f3 <- factor(sample(c("A", "B", "C"), n, replace = TRUE), levels = c("A", "B", "C"))
  dt <- as.Date("2021-06-01") + sample(0:1100, n, replace = TRUE)
  
  lin <- 1.2*x1 - 1.1*x2 + 0.9*(x4 > 0) + ifelse(f3 == "C", 0.7, 0) + rnorm(n, sd = 0.25)
  p <- 1 / (1 + exp(-lin))
  y <- factor(ifelse(runif(n) < p, "yes", "no"), levels = c("no", "yes"))
  
  df <- data.frame(
    y = y,
    x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
    f3 = f3,
    dt = dt,
    b01 = sample(c(0, 1), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Missingness
  miss_cols <- c("x1", "x4", "f3")
  for (cname in miss_cols) {
    idx <- sample(seq_len(n), size = floor(0.06 * n))
    df[idx, cname] <- NA
  }
  
  # Outliers
  out_idx <- sample(seq_len(n), size = floor(0.02 * n))
  df$x2[out_idx] <- df$x2[out_idx] + rnorm(length(out_idx), sd = 25)
  
  df
}

uat_make_multiclass_data <- function(n = 700, seed = 789) {
  set.seed(seed)
  
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- runif(n, -2, 2)
  x4 <- rnorm(n)
  f3 <- factor(sample(c("A", "B", "C"), n, replace = TRUE), levels = c("A", "B", "C"))
  
  s1 <- 0.9*x1 - 0.6*x2 + 0.4*(x3^2) + ifelse(f3 == "A", 0.7, 0) + rnorm(n, sd = 0.4)
  s2 <- -0.7*x1 + 0.8*x2 + 0.3*x4 + ifelse(f3 == "B", 0.6, 0) + rnorm(n, sd = 0.4)
  s3 <- 0.2*x1 + 0.2*x2 - 0.5*x4 + ifelse(f3 == "C", 0.6, 0) + rnorm(n, sd = 0.4)
  
  cls <- apply(cbind(s1, s2, s3), 1, which.max)
  y <- factor(c("c1", "c2", "c3")[cls], levels = c("c1", "c2", "c3"))
  
  df <- data.frame(
    y = y,
    x1 = x1, x2 = x2, x3 = x3, x4 = x4,
    f3 = f3,
    stringsAsFactors = FALSE
  )
  
  # Missingness
  miss_cols <- c("x3", "f3")
  for (cname in miss_cols) {
    idx <- sample(seq_len(n), size = floor(0.05 * n))
    df[idx, cname] <- NA
  }
  
  df
}

uat_inject_unseen_level <- function(df, factor_col, new_level_value = "NEWLEVEL", frac = 0.03, seed = 999) {
  set.seed(seed)
  if (!factor_col %in% names(df)) return(df)
  if (!is.factor(df[[factor_col]])) return(df)
  idx <- sample(seq_len(nrow(df)), size = max(1L, floor(frac * nrow(df))))
  x <- as.character(df[[factor_col]])
  x[idx] <- new_level_value
  df[[factor_col]] <- factor(x)
  df
}

# Core invariant checks
uat_check_no_na <- function(df, outcome_var, test_name_prefix) {
  if (!is.data.frame(df)) {
    uat_fail(paste0(test_name_prefix, ":is_df"), "Object is not a data.frame")
    return(FALSE)
  }
  cols <- setdiff(names(df), outcome_var)
  has_na <- anyNA(df[, cols, drop = FALSE])
  uat_assert(!has_na, paste0(test_name_prefix, ":no_na_predictors"),
             "Found NA values in predictors", "No NA in predictors")
}

uat_check_same_columns <- function(train, test, test_name_prefix) {
  uat_assert(identical(names(train), names(test)),
             paste0(test_name_prefix, ":same_columns"),
             "Train/test columns differ",
             "Train/test columns identical")
}

uat_check_outcome_present <- function(df, outcome_var, test_name_prefix) {
  uat_assert(outcome_var %in% names(df),
             paste0(test_name_prefix, ":outcome_present"),
             "Outcome column missing",
             "Outcome column present")
}

uat_check_finite_numeric <- function(df, outcome_var, test_name_prefix) {
  cols <- setdiff(names(df), outcome_var)
  num_cols <- cols[vapply(df[, cols, drop = FALSE], is.numeric, logical(1))]
  if (!length(num_cols)) {
    uat_skip(paste0(test_name_prefix, ":finite_numeric"), "No numeric predictor columns to check")
    return(invisible(TRUE))
  }
  ok <- all(vapply(num_cols, function(cn) all(is.finite(df[[cn]])), logical(1)))
  uat_assert(ok, paste0(test_name_prefix, ":finite_numeric"),
             "Non-finite numeric values found (Inf/NaN)",
             "All numeric predictor values are finite")
}

uat_check_apply_transformations <- function(original_data, preproc_params, outcome_var, test_name_prefix) {
  newdata <- original_data
  
  # Add some missingness
  set.seed(2024)
  for (cn in intersect(c("x1", "x2", "x3", "x4"), names(newdata))) {
    idx <- sample(seq_len(nrow(newdata)), size = max(1L, floor(0.04 * nrow(newdata))))
    newdata[idx, cn] <- NA
  }
  
  # Add outliers
  if ("x2" %in% names(newdata) && is.numeric(newdata$x2)) {
    idx <- sample(seq_len(nrow(newdata)), size = max(1L, floor(0.02 * nrow(newdata))))
    newdata$x2[idx] <- newdata$x2[idx] + rnorm(length(idx), sd = 30)
  }
  
  # Inject unseen factor level
  if ("f3" %in% names(newdata) && is.factor(newdata$f3)) {
    newdata <- uat_inject_unseen_level(newdata, "f3", new_level_value = "ZZZ", frac = 0.03, seed = 2025)
  }
  
  out <- uat_try(apply_transformations(newdata, preproc_params),
                 paste0(test_name_prefix, ":apply_transformations_runs"))
  if (!out$ok) return(invisible(FALSE))
  
  df_t <- out$value
  uat_check_outcome_present(df_t, outcome_var, paste0(test_name_prefix, ":apply_transformations"))
  uat_check_no_na(df_t, outcome_var, paste0(test_name_prefix, ":apply_transformations"))
  uat_check_finite_numeric(df_t, outcome_var, paste0(test_name_prefix, ":apply_transformations"))
  invisible(TRUE)
}

# Existence checks
uat_check_functions_exist <- function() {
  needed <- c(
    "preprocess_data",
    "apply_transformations",
    "impute_missing_data",
    "fit_scaler", "apply_scaler",
    "fit_one_hot_encoder", "apply_one_hot_encoder",
    "fit_outlier_removal", "apply_outlier_removal",
    "generate_interaction_terms"
  )
  missing <- needed[!vapply(needed, exists, logical(1), mode = "function")]
  if (length(missing)) {
    uat_fail("functions_exist", paste("Missing functions:", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  uat_pass("functions_exist", "All required functions found")
  TRUE
}

# Pipeline runs for different feature selection methods
uat_run_regression_smoke <- function() {
  uat_require_pkgs(c("caret", "mice"))
  
  df <- uat_make_regression_data(n = 500, seed = 11)
  
  out <- uat_try(
    preprocess_data(
      data = df,
      outcome_var = "y",
      partition_ratio = 0.75,
      date_vars = c("dt"),
      outlier_multiplier = 1.5,
      interaction_degree = 2,
      feature_selection = FALSE,
      ordinal_encoding = FALSE,
      scale_data_flag = TRUE,
      scaling_method = "standard",
      m = 5,
      maxit = 10,
      seed = 101,
      one_hot_fullRank = FALSE,
      impute_parallel = FALSE,
      drop_date_original = TRUE
    ),
    "regression:run_no_feature_selection"
  )
  if (!out$ok) return(invisible(FALSE))
  
  res <- out$value
  train <- res$train
  test  <- res$test
  preproc_params <- res$preproc_params
  
  uat_check_outcome_present(train, "y", "regression:no_fs:train")
  uat_check_outcome_present(test,  "y", "regression:no_fs:test")
  uat_check_same_columns(train, test, "regression:no_fs")
  uat_check_no_na(train, "y", "regression:no_fs:train")
  uat_check_no_na(test,  "y", "regression:no_fs:test")
  uat_check_finite_numeric(train, "y", "regression:no_fs:train")
  uat_check_finite_numeric(test,  "y", "regression:no_fs:test")
  
  uat_check_apply_transformations(df, preproc_params, "y", "regression:no_fs")
  
  invisible(TRUE)
}

uat_run_binary_classification_methods <- function() {
  uat_require_pkgs(c("caret", "mice"))
  
  df <- uat_make_binary_classification_data(n = 600, seed = 22)
  
  # ranger selection
  if (uat_has_pkgs(c("ranger"))) {
    out1 <- uat_try(
      preprocess_data(
        data = df,
        outcome_var = "y",
        partition_ratio = 0.8,
        date_vars = c("dt"),
        outlier_multiplier = 1.5,
        interaction_degree = 2,
        feature_selection = TRUE,
        feature_selection_method = "ranger",
        num_selected_features = 25,
        ordinal_encoding = FALSE,
        scale_data_flag = TRUE,
        scaling_method = "standard",
        m = 5,
        maxit = 10,
        seed = 202,
        one_hot_fullRank = FALSE,
        impute_parallel = FALSE,
        drop_date_original = TRUE
      ),
      "binary:ranger:run"
    )
    if (out1$ok) {
      res <- out1$value
      uat_check_same_columns(res$train, res$test, "binary:ranger")
      uat_check_no_na(res$train, "y", "binary:ranger:train")
      uat_check_no_na(res$test,  "y", "binary:ranger:test")
      uat_check_apply_transformations(df, res$preproc_params, "y", "binary:ranger")
    }
  } else {
    uat_skip("binary:ranger:run", "Package ranger not available")
  }
  
  # Boruta selection (runtime can be higher)
  if (uat_has_pkgs(c("Boruta", "ranger"))) {
    df_small <- df[sample(seq_len(nrow(df)), 350), , drop = FALSE]
    out2 <- uat_try(
      preprocess_data(
        data = df_small,
        outcome_var = "y",
        partition_ratio = 0.8,
        date_vars = c("dt"),
        outlier_multiplier = 1.5,
        interaction_degree = 1,
        feature_selection = TRUE,
        feature_selection_method = "Boruta",
        num_selected_features = 20,
        ordinal_encoding = FALSE,
        scale_data_flag = TRUE,
        scaling_method = "standard",
        m = 3,
        maxit = 7,
        seed = 303,
        one_hot_fullRank = FALSE,
        impute_parallel = FALSE,
        drop_date_original = TRUE
      ),
      "binary:boruta:run"
    )
    if (out2$ok) {
      res <- out2$value
      uat_check_same_columns(res$train, res$test, "binary:boruta")
      uat_check_no_na(res$train, "y", "binary:boruta:train")
      uat_check_no_na(res$test,  "y", "binary:boruta:test")
      uat_check_apply_transformations(df_small, res$preproc_params, "y", "binary:boruta")
    }
  } else {
    uat_skip("binary:boruta:run", "Packages Boruta and/or ranger not available")
  }
  
  # LASSO selection
  if (uat_has_pkgs(c("glmnet"))) {
    out3 <- uat_try(
      preprocess_data(
        data = df,
        outcome_var = "y",
        partition_ratio = 0.8,
        date_vars = c("dt"),
        outlier_multiplier = 1.5,
        interaction_degree = 1,
        feature_selection = TRUE,
        feature_selection_method = "LASSO",
        num_selected_features = 40,
        ordinal_encoding = FALSE,
        scale_data_flag = TRUE,
        scaling_method = "standard",
        m = 5,
        maxit = 10,
        seed = 404,
        one_hot_fullRank = FALSE,
        impute_parallel = FALSE,
        drop_date_original = TRUE,
        lasso_lambda_choice = "lambda.1se"
      ),
      "binary:lasso:run"
    )
    if (out3$ok) {
      res <- out3$value
      uat_check_same_columns(res$train, res$test, "binary:lasso")
      uat_check_no_na(res$train, "y", "binary:lasso:train")
      uat_check_no_na(res$test,  "y", "binary:lasso:test")
      uat_check_apply_transformations(df, res$preproc_params, "y", "binary:lasso")
    }
  } else {
    uat_skip("binary:lasso:run", "Package glmnet not available")
  }
  
  invisible(TRUE)
}

uat_run_multiclass_lasso <- function() {
  uat_require_pkgs(c("caret", "mice"))
  
  if (!uat_has_pkgs(c("glmnet"))) {
    uat_skip("multiclass:lasso:run", "Package glmnet not available")
    return(invisible(TRUE))
  }
  
  df <- uat_make_multiclass_data(n = 700, seed = 33)
  
  out <- uat_try(
    preprocess_data(
      data = df,
      outcome_var = "y",
      partition_ratio = 0.8,
      date_vars = NULL,
      outlier_multiplier = 1.5,
      interaction_degree = 1,
      feature_selection = TRUE,
      feature_selection_method = "LASSO",
      num_selected_features = 50,
      ordinal_encoding = FALSE,
      scale_data_flag = TRUE,
      scaling_method = "standard",
      m = 5,
      maxit = 10,
      seed = 505,
      one_hot_fullRank = FALSE,
      impute_parallel = FALSE,
      drop_date_original = TRUE,
      lasso_lambda_choice = "lambda.1se"
    ),
    "multiclass:lasso:run"
  )
  if (!out$ok) return(invisible(FALSE))
  
  res <- out$value
  uat_check_same_columns(res$train, res$test, "multiclass:lasso")
  uat_check_no_na(res$train, "y", "multiclass:lasso:train")
  uat_check_no_na(res$test,  "y", "multiclass:lasso:test")
  uat_check_apply_transformations(df, res$preproc_params, "y", "multiclass:lasso")
  
  invisible(TRUE)
}

uat_run_parallel_impute_smoke <- function() {
  uat_require_pkgs(c("mice", "parallel", "caret"))
  
  df <- uat_make_regression_data(n = 250, seed = 44)
  
  out <- uat_try(
    preprocess_data(
      data = df,
      outcome_var = "y",
      partition_ratio = 0.8,
      date_vars = c("dt"),
      outlier_multiplier = 1.5,
      interaction_degree = 1,
      feature_selection = FALSE,
      ordinal_encoding = FALSE,
      scale_data_flag = TRUE,
      scaling_method = "standard",
      m = 3,
      maxit = 5,
      seed = 606,
      one_hot_fullRank = FALSE,
      impute_parallel = TRUE,
      impute_ncores = 2,
      drop_date_original = TRUE
    ),
    "parallel_impute:run"
  )
  
  if (out$ok) {
    res <- out$value
    uat_check_same_columns(res$train, res$test, "parallel_impute")
    uat_check_no_na(res$train, "y", "parallel_impute:train")
    uat_check_no_na(res$test,  "y", "parallel_impute:test")
  }
  
  invisible(TRUE)
}

uat_run_reproducibility_smoke <- function() {
  uat_require_pkgs(c("caret", "mice"))
  
  df <- uat_make_binary_classification_data(n = 500, seed = 55)
  
  out1 <- uat_try(
    preprocess_data(
      data = df,
      outcome_var = "y",
      partition_ratio = 0.8,
      date_vars = c("dt"),
      outlier_multiplier = 1.5,
      interaction_degree = 1,
      feature_selection = FALSE,
      ordinal_encoding = FALSE,
      scale_data_flag = TRUE,
      scaling_method = "standard",
      m = 3,
      maxit = 7,
      seed = 777,
      one_hot_fullRank = FALSE,
      impute_parallel = FALSE,
      drop_date_original = TRUE
    ),
    "repro:run1"
  )
  
  out2 <- uat_try(
    preprocess_data(
      data = df,
      outcome_var = "y",
      partition_ratio = 0.8,
      date_vars = c("dt"),
      outlier_multiplier = 1.5,
      interaction_degree = 1,
      feature_selection = FALSE,
      ordinal_encoding = FALSE,
      scale_data_flag = TRUE,
      scaling_method = "standard",
      m = 3,
      maxit = 7,
      seed = 777,
      one_hot_fullRank = FALSE,
      impute_parallel = FALSE,
      drop_date_original = TRUE
    ),
    "repro:run2"
  )
  
  if (out1$ok && out2$ok) {
    n1 <- names(out1$value$train)
    n2 <- names(out2$value$train)
    uat_assert(identical(n1, n2), "repro:columns_identical",
               "Column names differ across identical runs",
               "Column names identical across identical runs")
  }
  
  invisible(TRUE)
}

# Master runner
uat_run_all <- function() {
  .uat_state$results <- .uat_state$results[0, , drop = FALSE]
  
  uat_check_functions_exist()
  uat_run_regression_smoke()
  uat_run_binary_classification_methods()
  uat_run_multiclass_lasso()
  uat_run_parallel_impute_smoke()
  uat_run_reproducibility_smoke()
  
  uat_summary()
}

################################################################################
# Run this in RStudio:
#   uat_run_all()
################################################################################

