# SMOTE
# -----------------------------------------------------
# Requires the 'FNN' package:
# install.packages("FNN")

.smote_require_fnn <- function() {
  if (!requireNamespace("FNN", quietly = TRUE)) {
    stop("Please install the 'FNN' package: install.packages('FNN')")
  }
  invisible(TRUE)
}

.smote_assert <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

.smote_with_seed <- function(seed, expr) {
  if (is.null(seed)) return(eval.parent(substitute(expr)))
  .smote_assert(length(seed) == 1 && is.numeric(seed) && is.finite(seed), "seed must be a single finite number.")
  
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  set.seed(seed)
  eval.parent(substitute(expr))
}

.smote_target_levels <- function(y) {
  if (is.factor(y)) levels(y) else NULL
}

.smote_cast_class_value <- function(class_value_chr, y) {
  if (is.factor(y) || is.character(y)) return(as.character(class_value_chr))
  if (is.integer(y)) {
    v <- suppressWarnings(as.integer(class_value_chr))
    .smote_assert(!is.na(v), "Class value cannot be cast to integer target type.")
    return(v)
  }
  if (is.numeric(y)) {
    v <- suppressWarnings(as.numeric(class_value_chr))
    .smote_assert(!is.na(v), "Class value cannot be cast to numeric target type.")
    return(v)
  }
  class_value_chr
}

.smote_choose_default_class <- function(y) {
  y0 <- y[!is.na(y)]
  if (is.factor(y0)) y0 <- droplevels(y0)
  
  counts <- table(y0)
  .smote_assert(length(counts) >= 1, "Target has no non-missing values.")
  min_count <- min(counts)
  candidates <- names(counts)[counts == min_count]
  candidates[[1]]
}

.smote_select_feature_cols <- function(data, target, features) {
  .smote_assert(is.data.frame(data), "data must be a data.frame.")
  .smote_assert(is.character(target) && length(target) == 1, "target must be a single column name.")
  .smote_assert(target %in% names(data), "Target variable not found in the data.")
  
  if (!is.null(features)) {
    .smote_assert(is.character(features) && length(features) >= 1, "features must be a character vector of column names.")
    .smote_assert(all(features %in% names(data)), "One or more feature columns were not found in the data.")
    .smote_assert(!target %in% features, "features must not include the target column.")
    x_df <- data[, features, drop = FALSE]
    .smote_assert(all(vapply(x_df, is.numeric, logical(1))), "All selected feature columns must be numeric.")
    return(features)
  }
  
  candidate_cols <- setdiff(names(data), target)
  numeric_cols <- candidate_cols[vapply(data[, candidate_cols, drop = FALSE], is.numeric, logical(1))]
  .smote_assert(length(numeric_cols) >= 1, "No numeric feature columns were found. Provide features= or preprocess to numeric.")
  numeric_cols
}

.smote_scale_for_knn <- function(x) {
  .smote_assert(is.matrix(x), "Internal: x must be a matrix.")
  center <- colMeans(x)
  scale <- apply(x, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  x_scaled <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  list(x = x_scaled, center = center, scale = scale)
}

.smote_knn_index <- function(x_knn, k) {
  .smote_require_fnn()
  n <- nrow(x_knn)
  
  .smote_assert(n >= 2, "At least 2 rows are required within the chosen class.")
  .smote_assert(is.numeric(k) && length(k) == 1 && is.finite(k), "k must be a single finite number.")
  k <- as.integer(k)
  .smote_assert(k >= 1, "k must be >= 1.")
  .smote_assert(k <= (n - 1), "k must be <= (n_minority - 1).")
  
  # Using get.knnx with query=data ensures the nearest row is itself (distance 0),
  # then the next k rows provide k neighbors.
  kn <- FNN::get.knnx(data = x_knn, query = x_knn, k = k + 1)
  nn <- kn$nn.index
  nn[, -1, drop = FALSE]
}

.smote_resolve_param_for_class <- function(param, class_key, default_value = NULL) {
  if (is.null(param)) return(default_value)
  if (length(param) == 1) return(param)
  
  .smote_assert(!is.null(names(param)), "Named vectors are required when param has length > 1.")
  .smote_assert(class_key %in% names(param), paste0("No entry found for class '", class_key, "' in a named vector parameter."))
  param[[class_key]]
}

.smote_n_synth <- function(n_min, perc_over, n_synth) {
  .smote_assert(n_min >= 0, "Internal: n_min must be >= 0.")
  
  if (!is.null(n_synth)) {
    .smote_assert(is.numeric(n_synth) && length(n_synth) == 1 && is.finite(n_synth), "n_synth must be a single finite number.")
    n_out <- as.integer(round(n_synth))
    .smote_assert(n_out >= 0, "n_synth must be >= 0.")
    return(n_out)
  }
  
  .smote_assert(is.numeric(perc_over) && length(perc_over) == 1 && is.finite(perc_over), "perc_over must be a single finite number.")
  .smote_assert(perc_over >= 0, "perc_over must be >= 0.")
  as.integer(round(n_min * (perc_over / 100)))
}

.smote_base_indices <- function(n_min, n_synth) {
  if (n_synth <= 0) return(integer(0))
  base_reps <- n_synth %/% n_min
  extra <- n_synth %% n_min
  c(
    rep(seq_len(n_min), base_reps),
    if (extra > 0) sample.int(n_min, size = extra, replace = FALSE) else integer(0)
  )
}

.smote_synthesize_matrix <- function(x_raw, nn_index, base_idx) {
  n_synth <- length(base_idx)
  if (n_synth == 0) return(x_raw[0, , drop = FALSE])
  
  k <- ncol(nn_index)
  neighbor_col <- sample.int(k, size = n_synth, replace = TRUE)
  neighbor_idx <- nn_index[cbind(base_idx, neighbor_col)]
  gaps <- stats::runif(n_synth)
  
  x_i <- x_raw[base_idx, , drop = FALSE]
  x_n <- x_raw[neighbor_idx, , drop = FALSE]
  delta <- x_n - x_i
  
  x_i + sweep(delta, 1, gaps, "*")
}

.smote_make_synthetic_df <- function(data, target, feature_cols, other_cols, synth_x, class_value_typed) {
  n_synth <- nrow(synth_x)
  if (n_synth == 0) return(data[0, , drop = FALSE])
  
  out <- as.data.frame(matrix(NA, nrow = n_synth, ncol = ncol(data)), stringsAsFactors = FALSE)
  names(out) <- names(data)
  
  # Feature columns are numeric by construction.
  for (j in seq_along(feature_cols)) {
    col <- feature_cols[[j]]
    out[[col]] <- as.numeric(synth_x[, j])
  }
  
  # Non-feature columns remain NA by design (IDs, timestamps, notes, etc.).
  if (length(other_cols) > 0) {
    for (col in other_cols) out[[col]] <- NA
  }
  
  # Target column follows the input's storage mode.
  y <- data[[target]]
  if (is.factor(y)) {
    out[[target]] <- factor(rep(as.character(class_value_typed), n_synth), levels = levels(y))
  } else if (is.character(y)) {
    out[[target]] <- rep(as.character(class_value_typed), n_synth)
  } else if (is.integer(y)) {
    out[[target]] <- rep(as.integer(class_value_typed), n_synth)
  } else if (is.numeric(y)) {
    out[[target]] <- rep(as.numeric(class_value_typed), n_synth)
  } else {
    out[[target]] <- rep(class_value_typed, n_synth)
  }
  
  out
}

# Create a reusable specification object.
# - class can be NULL (auto), or a vector of class labels to oversample.
# - perc_over / n_synth can be scalars or named vectors keyed by class label.
smote_spec <- function(target,
                       perc_over = 100,
                       n_synth = NULL,
                       k = 5,
                       class = NULL,
                       features = NULL,
                       scale_features = FALSE,
                       seed = NULL) {
  .smote_assert(is.character(target) && length(target) == 1, "target must be a single column name.")
  .smote_assert(is.null(class) || (is.atomic(class) && length(class) >= 1), "class must be NULL or a vector of class labels.")
  .smote_assert(is.logical(scale_features) && length(scale_features) == 1, "scale_features must be TRUE or FALSE.")
  
  list(
    target = target,
    perc_over = perc_over,
    n_synth = n_synth,
    k = k,
    class = class,
    features = features,
    scale_features = scale_features,
    seed = seed
  )
}

# Apply SMOTE using a spec.
smote_apply <- function(data, spec, return = c("data", "list")) {
  return <- match.arg(return)
  .smote_assert(is.list(spec), "spec must be a list created by smote_spec().")
  
  target <- spec$target
  .smote_assert(is.data.frame(data), "data must be a data.frame.")
  .smote_assert(target %in% names(data), "Target variable not found in the data.")
  
  y <- data[[target]]
  .smote_assert(!all(is.na(y)), "Target column contains only missing values.")
  
  feature_cols <- .smote_select_feature_cols(data, target, spec$features)
  other_cols <- setdiff(names(data), c(feature_cols, target))
  
  default_class_chr <- .smote_choose_default_class(y)
  class_vec <- spec$class
  if (is.null(class_vec)) class_vec <- default_class_chr
  
  # Work in character keys for param lookup.
  class_keys <- as.character(class_vec)
  
  synthetic_all <- list()
  meta <- list()
  data_out <- data
  
  .smote_with_seed(spec$seed, {
    for (idx in seq_along(class_keys)) {
      class_key <- class_keys[[idx]]
      
      # Select rows of this class using a character comparison.
      in_class <- as.character(data_out[[target]]) == class_key
      class_data <- data_out[in_class, , drop = FALSE]
      
      n_min <- nrow(class_data)
      .smote_assert(n_min >= 1, paste0("No rows found for class '", class_key, "'."))
      .smote_assert(n_min >= 2, paste0("At least 2 rows are required within class '", class_key, "'."))
      
      perc_i <- .smote_resolve_param_for_class(spec$perc_over, class_key, default_value = 100)
      n_synth_i <- .smote_resolve_param_for_class(spec$n_synth, class_key, default_value = NULL)
      n_s <- .smote_n_synth(n_min, perc_i, n_synth_i)
      
      if (n_s == 0) {
        meta[[class_key]] <- list(n_minority = n_min, n_synth = 0, k = as.integer(spec$k))
        next
      }
      
      x_df <- class_data[, feature_cols, drop = FALSE]
      .smote_assert(all(vapply(x_df, is.numeric, logical(1))), "All feature columns must be numeric.")
      .smote_assert(!anyNA(x_df), paste0("Missing values found in feature columns for class '", class_key, "'."))
      
      x_raw <- as.matrix(x_df)
      x_knn <- x_raw
      
      if (isTRUE(spec$scale_features)) {
        x_knn <- .smote_scale_for_knn(x_raw)$x
      }
      
      nn_index <- .smote_knn_index(x_knn, spec$k)
      base_idx <- .smote_base_indices(n_min, n_s)
      synth_x <- .smote_synthesize_matrix(x_raw, nn_index, base_idx)
      
      class_value_typed <- .smote_cast_class_value(class_key, data_out[[target]])
      
      synth_df <- .smote_make_synthetic_df(
        data = data_out,
        target = target,
        feature_cols = feature_cols,
        other_cols = other_cols,
        synth_x = synth_x,
        class_value_typed = class_value_typed
      )
      
      synthetic_all[[class_key]] <- synth_df
      data_out <- rbind(data_out, synth_df)
      
      meta[[class_key]] <- list(
        n_minority = n_min,
        n_synth = nrow(synth_df),
        k = as.integer(spec$k),
        scale_features = isTRUE(spec$scale_features)
      )
    }
  })
  
  synthetic_df <- if (length(synthetic_all) == 0) data_out[0, , drop = FALSE] else do.call(rbind, synthetic_all)
  
  res <- list(
    data = data_out,
    synthetic = synthetic_df,
    spec = spec,
    meta = meta
  )
  class(res) <- "smote_result"
  
  if (return == "data") return(res$data)
  res
}

# Convenience wrapper: spec + apply
smote <- function(data,
                  target,
                  perc_over = 100,
                  n_synth = NULL,
                  k = 5,
                  class = NULL,
                  features = NULL,
                  scale_features = FALSE,
                  seed = NULL,
                  return = c("data", "list")) {
  spec <- smote_spec(
    target = target,
    perc_over = perc_over,
    n_synth = n_synth,
    k = k,
    class = class,
    features = features,
    scale_features = scale_features,
    seed = seed
  )
  smote_apply(data, spec, return = return)
}

print.smote_result <- function(x, ...) {
  cat("SMOTE result\n")
  cat("- Rows (output): ", nrow(x$data), "\n", sep = "")
  cat("- Rows (synthetic): ", nrow(x$synthetic), "\n", sep = "")
  cat("- Target: ", x$spec$target, "\n", sep = "")
  if (length(x$meta) > 0) {
    cat("- Per-class:\n")
    for (nm in names(x$meta)) {
      m <- x$meta[[nm]]
      cat("  * ", nm, ": n_minority=", m$n_minority, ", n_synth=", m$n_synth, ", k=", m$k, "\n", sep = "")
    }
  }
  invisible(x)
}

# -----------------------------------------------------
# Example usage:
#
# spec <- smote_spec(target = "Class", perc_over = 200, k = 5, seed = 123)
# out  <- smote_apply(df, spec, return = "list")
# new_df <- out$data
#
# One-call usage:
# new_df <- smote(df, target = "Class", perc_over = 200, k = 5, seed = 123)
#
# Oversample multiple classes with per-class settings:
# new_df <- smote(
#   df, target = "Class",
#   class = c("A", "B"),
#   perc_over = c(A = 200, B = 100),
#   k = 5,
#   seed = 123
# )


# -----------------------------------------------------
# SMOTE UAT (paste into same session after loading SMOTE functions)
# -----------------------------------------------------

uat_expect_error <- function(expr, pattern = NULL) {
  ok <- FALSE
  msg <- NULL
  tryCatch(
    {
      force(expr)
      ok <- FALSE
    },
    error = function(e) {
      ok <<- TRUE
      msg <<- conditionMessage(e)
    }
  )
  stopifnot(ok)
  if (!is.null(pattern)) {
    stopifnot(grepl(pattern, msg, fixed = FALSE))
  }
  invisible(TRUE)
}

uat_expect_true <- function(cond, msg = "Expectation failed.") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

uat_equal <- function(a, b, msg = "Values are not equal.") {
  if (!isTRUE(all.equal(a, b))) stop(msg, call. = FALSE)
  invisible(TRUE)
}

uat_same_names <- function(a, b, msg = "Names differ.") {
  if (!identical(names(a), names(b))) stop(msg, call. = FALSE)
  invisible(TRUE)
}

uat_is_data_frame <- function(x, msg = "Not a data.frame.") {
  uat_expect_true(is.data.frame(x), msg)
}

uat_run_smote_uat <- function(verbose = TRUE) {
  if (verbose) cat("UAT: starting\n")
  
  # ---------------------------
  # Test dataset builders
  # ---------------------------
  make_df_factor <- function(n_major = 90, n_minor = 10, seed = 1) {
    set.seed(seed)
    x1 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 2, 1))
    x2 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 2, 1))
    id <- seq_len(n_major + n_minor)
    note <- sample(c("alpha", "beta", "gamma"), n_major + n_minor, replace = TRUE)
    cls <- factor(c(rep("major", n_major), rep("minor", n_minor)))
    data.frame(id = id, x1 = x1, x2 = x2, note = note, Class = cls, stringsAsFactors = FALSE)
  }
  
  make_df_char <- function(n_major = 80, n_minor = 20, seed = 2) {
    set.seed(seed)
    x1 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 3, 1))
    x2 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 3, 1))
    cls <- c(rep("A", n_major), rep("B", n_minor))
    data.frame(x1 = x1, x2 = x2, Class = cls, stringsAsFactors = FALSE)
  }
  
  make_df_numeric_target <- function(n_major = 50, n_minor = 10, seed = 3) {
    set.seed(seed)
    x1 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 4, 1))
    x2 <- c(rnorm(n_major, 0, 1), rnorm(n_minor, 4, 1))
    cls <- c(rep(0, n_major), rep(1, n_minor))
    data.frame(x1 = x1, x2 = x2, Class = cls)
  }
  
  # ---------------------------
  # UAT 1: Basic output shape / columns preserved
  # ---------------------------
  df <- make_df_factor()
  n0 <- nrow(df)
  features <- c("x1", "x2")
  target <- "Class"
  non_features <- setdiff(names(df), c(features, target))
  
  out <- smote(df, target = target, features = features, perc_over = 200, k = 5, seed = 123, return = "list")
  
  uat_is_data_frame(out$data)
  uat_is_data_frame(out$synthetic)
  uat_same_names(out$data, df, "Output columns changed.")
  uat_expect_true(nrow(out$data) > n0, "Expected additional rows.")
  uat_expect_true(all(features %in% names(out$data)), "Expected feature columns present.")
  uat_expect_true(target %in% names(out$data), "Expected target present.")
  uat_expect_true(is.factor(out$data[[target]]), "Expected target to remain factor in output.")
  
  # synthetic rows: non-feature cols should be NA, target set, features numeric
  if (nrow(out$synthetic) > 0) {
    for (col in non_features) {
      uat_expect_true(all(is.na(out$synthetic[[col]])),
                      paste0("Expected ", col, " to be NA in synthetic rows."))
    }
    uat_expect_true(is.numeric(out$synthetic$x1) && is.numeric(out$synthetic$x2), "Synthetic features not numeric.")
    uat_expect_true(all(as.character(out$synthetic[[target]]) == "minor"), "Synthetic target values unexpected.")
  }
  
  if (verbose) cat("UAT 1: ok\n")
  
  # ---------------------------
  # UAT 2: Expected synthetic count for perc_over
  # ---------------------------
  n_min <- sum(df[[target]] == "minor")
  expected_added <- round(n_min * 2.0)  # perc_over=200 -> +2x minority count
  uat_equal(nrow(out$synthetic), expected_added, "Unexpected number of synthetic rows.")
  uat_equal(nrow(out$data), n0 + expected_added, "Unexpected total row count.")
  if (verbose) cat("UAT 2: ok\n")
  
  # ---------------------------
  # UAT 3: Determinism with seed (same input -> same output)
  # ---------------------------
  out1 <- smote(df, target = target, features = features, perc_over = 200, k = 5, seed = 999, return = "list")
  out2 <- smote(df, target = target, features = features, perc_over = 200, k = 5, seed = 999, return = "list")
  
  uat_equal(out1$synthetic[, c("x1", "x2", target)], out2$synthetic[, c("x1", "x2", target)],
            "Seeded runs are not identical.")
  if (verbose) cat("UAT 3: ok\n")
  
  # ---------------------------
  # UAT 4: return="data" returns only data.frame
  # ---------------------------
  new_df <- smote(df, target = target, features = features, perc_over = 50, k = 5, seed = 1, return = "data")
  uat_is_data_frame(new_df)
  uat_same_names(new_df, df, "return='data' changed columns.")
  if (verbose) cat("UAT 4: ok\n")
  
  # ---------------------------
  # UAT 5: Character target support
  # ---------------------------
  dfc <- make_df_char()
  outc <- smote(dfc, target = "Class", features = c("x1", "x2"), perc_over = 100, k = 5, seed = 1, return = "list")
  uat_expect_true(is.character(outc$data$Class), "Expected character target to remain character.")
  uat_expect_true(all(outc$synthetic$Class == .smote_choose_default_class(dfc$Class)), "Synthetic class mismatch.")
  if (verbose) cat("UAT 5: ok\n")
  
  # ---------------------------
  # UAT 6: Numeric target support
  # ---------------------------
  dfn <- make_df_numeric_target()
  outn <- smote(dfn, target = "Class", features = c("x1", "x2"), perc_over = 100, k = 5, seed = 2, return = "list")
  uat_expect_true(is.numeric(outn$data$Class) || is.integer(outn$data$Class), "Expected numeric/integer target.")
  if (verbose) cat("UAT 6: ok\n")
  
  # ---------------------------
  # UAT 7: features= subset works, other numeric columns left NA
  # ---------------------------
  df2 <- df
  df2$x3 <- rnorm(nrow(df2))
  outf <- smote(df2, target = target, features = c("x1", "x2"), perc_over = 100, k = 5, seed = 7, return = "list")
  uat_expect_true(all(is.na(outf$synthetic$x3)), "Expected x3 to be NA in synthetic rows when not in features.")
  if (verbose) cat("UAT 7: ok\n")
  
  # ---------------------------
  # UAT 8: scale_features flag runs
  # ---------------------------
  outs <- smote(df, target = target, features = features, perc_over = 100, k = 5, scale_features = TRUE, seed = 11, return = "list")
  uat_expect_true(is.list(outs$meta) && length(outs$meta) >= 1, "Expected meta info present.")
  if (verbose) cat("UAT 8: ok\n")
  
  # ---------------------------
  # UAT 9: Multi-class settings with named perc_over
  # ---------------------------
  set.seed(10)
  df3 <- data.frame(
    x1 = c(rnorm(60, 0, 1), rnorm(20, 2, 1), rnorm(10, 4, 1)),
    x2 = c(rnorm(60, 0, 1), rnorm(20, 2, 1), rnorm(10, 4, 1)),
    Class = factor(c(rep("A", 60), rep("B", 20), rep("C", 10)))
  )
  out3 <- smote(
    df3, target = "Class",
    features = c("x1", "x2"),
    class = c("B", "C"),
    perc_over = c(B = 50, C = 200),
    k = 5,
    seed = 42,
    return = "list"
  )
  
  nB <- sum(df3$Class == "B")
  nC <- sum(df3$Class == "C")
  expB <- round(nB * 0.5)
  expC <- round(nC * 2.0)
  
  uat_equal(nrow(out3$synthetic), expB + expC, "Unexpected synthetic row count for multi-class.")
  uat_expect_true(all(as.character(out3$synthetic$Class) %in% c("B", "C")), "Unexpected class values in synthetic rows.")
  if (verbose) cat("UAT 9: ok\n")
  
  # ---------------------------
  # Negative tests (expected errors)
  # ---------------------------
  if (verbose) cat("UAT: negative tests\n")
  
  # Missing target
  uat_expect_error(smote(df, target = "Nope", features = features, perc_over = 100, k = 5), "Target variable not found")
  
  # k too large (k > n_minority-1)
  uat_expect_error(smote(df, target = target, features = features, perc_over = 100, k = 999), "k must be <=")
  
  # Non-numeric feature (force it into feature set)
  df_bad <- df
  df_bad$x1 <- as.character(df_bad$x1)
  uat_expect_error(smote(df_bad, target = target, features = c("x1", "x2"), perc_over = 100, k = 5), "numeric")
  
  # Missing values in feature column for chosen class
  df_na <- df
  df_na$x1[df_na$Class == "minor"][1] <- NA
  uat_expect_error(smote(df_na, target = target, features = features, perc_over = 100, k = 5), "Missing values")
  
  if (verbose) cat("UAT: ok (all tests passed)\n")
  invisible(TRUE)
}

# Uncomment the line below to run all UAT checks
# uat_run_smote_uat(verbose = TRUE)

