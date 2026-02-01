##############################
# Outlier Detection Toolkit
##############################

# -------------------------
# Defaults + method naming
# -------------------------

OUTLIER_DEFAULTS <- list(
  z = list(thresh = 1.96),
  tukey = list(k = 1.5),
  grubbs = list(alpha = 0.05, max_outliers = 1),
  mad = list(k = 3),
  iglewicz_hoaglin = list(thresh = 3.5),
  
  mahalanobis = list(quantile = 0.95, threshold = NULL, use_cov_rob = FALSE),
  
  isolation_forest = list(
    contamination = 0.1,
    ndim = 1,
    ntrees = 100,
    nthreads = 1,
    seed = 1,
    standardize_data = TRUE
  ),
  
  dbscan = list(eps = 0.5, minPts = 5, scale = TRUE, borderPoints = TRUE),
  one_class_svm = list(nu = 0.05, kernel = "radial", gamma = NULL, scale = TRUE),
  elliptic_envelope = list(contamination = 0.1),
  lof = list(minPts = 5, thresh = 1.5, scale = TRUE)
)

OUTLIER_ALIASES <- c(
  # univariate
  z = "z",
  zscore = "z",
  z_score = "z",
  tukey = "tukey",
  iqr = "tukey",
  grubbs = "grubbs",
  mad = "mad",
  modified_z = "iglewicz_hoaglin",
  iglewicz = "iglewicz_hoaglin",
  hoaglin = "iglewicz_hoaglin",
  iglewicz_hoaglin = "iglewicz_hoaglin",
  
  # multivariate / ML
  mahalanobis = "mahalanobis",
  md = "mahalanobis",
  isolation_forest = "isolation_forest",
  iforest = "isolation_forest",
  dbscan = "dbscan",
  one_class_svm = "one_class_svm",
  ocsvm = "one_class_svm",
  svm = "one_class_svm",
  elliptic_envelope = "elliptic_envelope",
  elliptic = "elliptic_envelope",
  lof = "lof"
)

OUTLIER_PRESETS <- list(
  basic = c("z", "tukey"),
  robust = c("tukey", "mad", "iglewicz_hoaglin"),
  ml = c("isolation_forest", "dbscan", "one_class_svm", "lof"),
  all = names(OUTLIER_DEFAULTS)
)

# Flat/legacy parameter names can be supplied through ... or config
OUTLIER_FLAT_KEY_MAP <- list(
  z_thresh = c("z", "thresh"),
  tukey_mult = c("tukey", "k"),
  grubbs_thresh = c("grubbs", "alpha"),
  grubbs_alpha = c("grubbs", "alpha"),
  grubbs_max_outliers = c("grubbs", "max_outliers"),
  mad_mult = c("mad", "k"),
  iglewicz_hoaglin_thresh = c("iglewicz_hoaglin", "thresh"),
  
  mahalanobis_thresh = c("mahalanobis", "threshold"),
  mahalanobis_quantile = c("mahalanobis", "quantile"),
  mahalanobis_robust = c("mahalanobis", "use_cov_rob"),
  
  isolation_forest_contamination = c("isolation_forest", "contamination"),
  isolation_forest_ndim = c("isolation_forest", "ndim"),
  isolation_forest_ntree = c("isolation_forest", "ntrees"),
  isolation_forest_ntrees = c("isolation_forest", "ntrees"),
  isolation_forest_nthreads = c("isolation_forest", "nthreads"),
  isolation_forest_seed = c("isolation_forest", "seed"),
  
  dbscan_eps = c("dbscan", "eps"),
  dbscan_minPts = c("dbscan", "minPts"),
  
  one_class_svm_nu = c("one_class_svm", "nu"),
  
  elliptic_envelope_contamination = c("elliptic_envelope", "contamination"),
  
  lof_minPts = c("lof", "minPts"),
  lof_thresh = c("lof", "thresh")
)

UNIVARIATE_METHODS <- c("z", "tukey", "grubbs", "mad", "iglewicz_hoaglin")
MULTIVARIATE_METHODS <- c("mahalanobis", "isolation_forest", "dbscan", "one_class_svm", "elliptic_envelope", "lof")


# -------------------------
# Small helpers
# -------------------------

.tokenize <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(character())
  y <- x
  for (ch in c(",", ";", "|")) y <- gsub(ch, " ", y, fixed = TRUE)
  tokens <- strsplit(y, "\\s+")[[1]]
  tokens[nchar(tokens) > 0]
}

.as_character_vec <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) return(x)
  as.character(x)
}

.is_numeric_col <- function(v) is.numeric(v) || is.integer(v)

.numeric_columns <- function(df) {
  names(df)[vapply(df, .is_numeric_col, logical(1))]
}

.ensure_df <- function(df) {
  if (is.null(df) || !is.data.frame(df)) stop("data must be a data frame.")
  if (nrow(df) == 0) stop("data has zero rows.")
  invisible(TRUE)
}

.ensure_pkg <- function(pkg, hint) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required for %s.", pkg, hint), call. = FALSE)
  }
  invisible(TRUE)
}

.unique_name <- function(name, existing) {
  if (!name %in% existing) return(name)
  i <- 1L
  repeat {
    cand <- paste0(name, "_", i)
    if (!cand %in% existing) return(cand)
    i <- i + 1L
  }
}

.standardize_matrix <- function(mat) {
  mu <- colMeans(mat)
  sdv <- apply(mat, 2, stats::sd)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  centered <- sweep(mat, 2, mu, "-")
  sweep(centered, 2, sdv, "/")
}

.complete_idx <- function(df) {
  stats::complete.cases(df)
}

.parse_columns <- function(df, columns) {
  if (is.null(columns)) return(.numeric_columns(df))
  if (is.character(columns) && length(columns) == 1L) return(.tokenize(columns))
  .as_character_vec(columns)
}

.parse_methods <- function(methods) {
  if (is.null(methods)) return(OUTLIER_PRESETS$all)
  
  if (is.character(methods) && length(methods) == 1L) {
    key <- tolower(trimws(methods))
    if (key %in% names(OUTLIER_PRESETS)) return(OUTLIER_PRESETS[[key]])
    toks <- .tokenize(key)
    mapped <- vapply(toks, function(t) OUTLIER_ALIASES[[t]] %||% t, character(1))
    return(mapped)
  }
  
  m <- .as_character_vec(methods)
  m <- tolower(trimws(m))
  vapply(m, function(t) OUTLIER_ALIASES[[t]] %||% t, character(1))
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0L && !is.na(x[1])) x else y

.merge_config <- function(user_cfg) {
  cfg <- lapply(OUTLIER_DEFAULTS, function(x) x)
  if (is.null(user_cfg)) return(cfg)
  if (!is.list(user_cfg)) stop("config must be a list (or NULL).", call. = FALSE)
  
  # Apply flat keys
  for (flat_key in names(OUTLIER_FLAT_KEY_MAP)) {
    if (!flat_key %in% names(user_cfg)) next
    mp <- OUTLIER_FLAT_KEY_MAP[[flat_key]]
    m <- mp[[1]]; p <- mp[[2]]
    cfg[[m]][[p]] <- user_cfg[[flat_key]]
  }
  
  # Apply nested or shorthand keys
  for (k in names(user_cfg)) {
    if (k %in% names(OUTLIER_FLAT_KEY_MAP)) next
    if (!k %in% names(cfg)) next
    
    v <- user_cfg[[k]]
    if (is.list(v)) {
      cfg[[k]] <- utils::modifyList(cfg[[k]], v)
    } else {
      # shorthand: primary parameter per method
      if (k == "z") cfg[[k]]$thresh <- v
      if (k == "tukey") cfg[[k]]$k <- v
      if (k == "grubbs") cfg[[k]]$alpha <- v
      if (k == "mad") cfg[[k]]$k <- v
      if (k == "iglewicz_hoaglin") cfg[[k]]$thresh <- v
      
      if (k == "mahalanobis") {
        if (is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0 && v <= 1) {
          cfg[[k]]$quantile <- v
          cfg[[k]]$threshold <- NULL
        } else {
          cfg[[k]]$threshold <- v
        }
      }
      
      if (k == "isolation_forest") cfg[[k]]$contamination <- v
      if (k == "dbscan") cfg[[k]]$eps <- v
      if (k == "one_class_svm") cfg[[k]]$nu <- v
      if (k == "elliptic_envelope") cfg[[k]]$contamination <- v
      if (k == "lof") cfg[[k]]$thresh <- v
    }
  }
  
  cfg
}


# -------------------------
# Univariate detectors (vector -> flags)
# -------------------------

detect_z <- function(x, thresh = 1.96) {
  out <- rep.int(NA_integer_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  
  xv <- as.numeric(x[valid])
  mu <- mean(xv)
  sdv <- stats::sd(xv)
  
  if (!is.finite(sdv) || sdv == 0) {
    out[valid] <- 0L
    return(out)
  }
  
  z <- (xv - mu) / sdv
  out[valid] <- ifelse(abs(z) > as.numeric(thresh), 1L, 0L)
  out
}

detect_tukey <- function(x, k = 1.5) {
  out <- rep.int(NA_integer_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  
  xv <- as.numeric(x[valid])
  q1 <- as.numeric(stats::quantile(xv, 0.25, names = FALSE, type = 7))
  q3 <- as.numeric(stats::quantile(xv, 0.75, names = FALSE, type = 7))
  iqr <- q3 - q1
  
  if (!is.finite(iqr) || iqr == 0) {
    out[valid] <- 0L
    return(out)
  }
  
  lo <- q1 - as.numeric(k) * iqr
  hi <- q3 + as.numeric(k) * iqr
  out[valid] <- ifelse(xv < lo | xv > hi, 1L, 0L)
  out
}

detect_mad <- function(x, k = 3) {
  out <- rep.int(NA_integer_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  
  xv <- as.numeric(x[valid])
  med <- stats::median(xv)
  mad0 <- stats::median(abs(xv - med))
  
  if (!is.finite(mad0) || mad0 == 0) {
    out[valid] <- ifelse(xv == med, 0L, 1L)
    return(out)
  }
  
  out[valid] <- ifelse(abs(xv - med) > as.numeric(k) * mad0, 1L, 0L)
  out
}

detect_iglewicz_hoaglin <- function(x, thresh = 3.5) {
  out <- rep.int(NA_integer_, length(x))
  valid <- !is.na(x)
  if (!any(valid)) return(out)
  
  xv <- as.numeric(x[valid])
  med <- stats::median(xv)
  mad0 <- stats::median(abs(xv - med))
  
  if (!is.finite(mad0) || mad0 == 0) {
    out[valid] <- ifelse(xv == med, 0L, 1L)
    return(out)
  }
  
  mz <- 0.6745 * (xv - med) / mad0
  out[valid] <- ifelse(abs(mz) > as.numeric(thresh), 1L, 0L)
  out
}

.grubbs_crit <- function(n, alpha) {
  tval <- stats::qt(1 - as.numeric(alpha) / (2 * n), df = n - 2)
  ((n - 1) / sqrt(n)) * sqrt(tval^2 / (n - 2 + tval^2))
}

detect_grubbs <- function(x, alpha = 0.05, max_outliers = 1) {
  out <- rep.int(NA_integer_, length(x))
  valid <- !is.na(x)
  n <- sum(valid)
  if (n < 3) {
    warning("Grubbs test requires at least 3 non-missing values.")
    return(out)
  }
  
  xv_full <- as.numeric(x[valid])
  idx_full <- which(valid)
  
  flagged_local <- integer()
  keep <- seq_along(xv_full)
  
  for (i in seq_len(as.integer(max_outliers))) {
    if (length(keep) < 3) break
    
    xv <- xv_full[keep]
    mu <- mean(xv)
    sdv <- stats::sd(xv)
    
    if (!is.finite(sdv) || sdv == 0) break
    
    dev <- abs(xv - mu)
    j <- which.max(dev)
    G <- dev[j] / sdv
    Gcrit <- .grubbs_crit(length(xv), alpha)
    
    if (!(G > Gcrit)) break
    
    flagged_local <- c(flagged_local, keep[j])
    keep <- keep[-j]
  }
  
  out[valid] <- 0L
  if (length(flagged_local) > 0) {
    out[idx_full[flagged_local]] <- 1L
  }
  out
}


# -------------------------
# Multivariate detectors (data.frame -> flags)
# -------------------------

.mahalanobis_md2 <- function(X, center, cov_mat) {
  p <- ncol(X)
  
  inv_cov <- tryCatch(solve(cov_mat), error = function(e) NULL)
  if (is.null(inv_cov)) {
    if (requireNamespace("MASS", quietly = TRUE)) {
      inv_cov <- MASS::ginv(cov_mat)
    } else {
      eps <- 1e-8
      inv_cov <- solve(cov_mat + diag(eps, p))
    }
  }
  
  dif <- sweep(X, 2, center, "-")
  rowSums((dif %*% inv_cov) * dif)
}

detect_mahalanobis <- function(df, quantile = 0.95, threshold = NULL, use_cov_rob = FALSE) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 2) return(out)
  
  X <- as.matrix(df[cc, , drop = FALSE])
  X <- apply(X, 2, as.numeric)
  
  p <- ncol(X)
  if (nrow(X) < (p + 1)) return(out)
  
  if (isTRUE(use_cov_rob)) {
    .ensure_pkg("MASS", "MASS::cov.rob (Mahalanobis)")
    cov_obj <- tryCatch(MASS::cov.rob(X), error = function(e) NULL)
    if (is.null(cov_obj)) return(out)
    center <- as.numeric(cov_obj$center)
    cov_mat <- as.matrix(cov_obj$cov)
  } else {
    center <- colMeans(X)
    cov_mat <- stats::cov(X)
  }
  
  md2 <- tryCatch(.mahalanobis_md2(X, center, cov_mat), error = function(e) NULL)
  if (is.null(md2)) return(out)
  
  if (is.null(threshold)) {
    threshold <- stats::qchisq(as.numeric(quantile), df = p)
  }
  
  out[cc] <- ifelse(md2 > as.numeric(threshold), 1L, 0L)
  out
}

detect_elliptic_envelope <- function(df, contamination = 0.1) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 2) return(out)
  
  X <- as.matrix(df[cc, , drop = FALSE])
  X <- apply(X, 2, as.numeric)
  
  p <- ncol(X)
  if (nrow(X) < (p + 1)) return(out)
  
  .ensure_pkg("MASS", "MASS::cov.rob (Elliptic Envelope)")
  cov_obj <- tryCatch(MASS::cov.rob(X), error = function(e) NULL)
  if (is.null(cov_obj)) return(out)
  
  md2 <- tryCatch(.mahalanobis_md2(X, cov_obj$center, cov_obj$cov), error = function(e) NULL)
  if (is.null(md2)) return(out)
  
  cont <- as.numeric(contamination)
  if (!is.finite(cont)) cont <- 0.1
  cont <- max(0, min(1, cont))
  
  thr <- stats::qchisq(1 - cont, df = p)
  out[cc] <- ifelse(md2 > thr, 1L, 0L)
  out
}

detect_isolation_forest <- function(df, contamination = 0.1, ndim = 1, ntrees = 100, nthreads = 1, seed = 1, standardize_data = TRUE) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 3) return(out)
  
  .ensure_pkg("isotree", "isotree::isolation.forest")
  
  X <- df[cc, , drop = FALSE]
  
  cont <- as.numeric(contamination)
  if (!is.finite(cont)) cont <- 0.1
  cont <- max(0, min(1, cont))
  
  model <- isotree::isolation.forest(
    data = X,
    ndim = as.integer(ndim),
    ntrees = as.integer(ntrees),
    nthreads = as.integer(nthreads),
    seed = as.integer(seed),
    standardize_data = isTRUE(standardize_data)
  )
  
  scores <- tryCatch(as.numeric(predict(model, X, type = "score")), error = function(e) NULL)
  if (is.null(scores)) return(out)
  
  thr <- if (cont <= 0) {
    Inf
  } else if (cont >= 1) {
    -Inf
  } else {
    as.numeric(stats::quantile(scores, probs = 1 - cont, names = FALSE, type = 7, na.rm = TRUE))
  }
  
  out[cc] <- ifelse(scores >= thr, 1L, 0L)
  out
}

detect_dbscan <- function(df, eps = 0.5, minPts = 5, scale = TRUE, borderPoints = TRUE) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 3) return(out)
  
  .ensure_pkg("dbscan", "dbscan::dbscan")
  
  X <- as.matrix(df[cc, , drop = FALSE])
  X <- apply(X, 2, as.numeric)
  if (isTRUE(scale)) X <- .standardize_matrix(X)
  
  cl <- tryCatch(
    dbscan::dbscan(X, eps = as.numeric(eps), minPts = as.integer(minPts), borderPoints = isTRUE(borderPoints)),
    error = function(e) NULL
  )
  if (is.null(cl)) return(out)
  
  out[cc] <- ifelse(cl$cluster == 0, 1L, 0L)
  out
}

detect_one_class_svm <- function(df, nu = 0.05, kernel = "radial", gamma = NULL, scale = TRUE) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 3) return(out)
  
  .ensure_pkg("e1071", "e1071::svm")
  
  X <- as.matrix(df[cc, , drop = FALSE])
  X <- apply(X, 2, as.numeric)
  
  args <- list(
    x = X,
    y = NULL,
    type = "one-classification",
    nu = as.numeric(nu),
    kernel = as.character(kernel),
    scale = isTRUE(scale)
  )
  if (!is.null(gamma)) args$gamma <- as.numeric(gamma)
  
  model <- tryCatch(do.call(e1071::svm, args), error = function(e) NULL)
  if (is.null(model)) return(out)
  
  preds <- tryCatch(predict(model, X), error = function(e) NULL)
  if (is.null(preds)) return(out)
  
  out[cc] <- ifelse(as.logical(preds), 0L, 1L)
  out
}

detect_lof <- function(df, minPts = 5, thresh = 1.5, scale = TRUE) {
  out <- rep.int(NA_integer_, nrow(df))
  cc <- .complete_idx(df)
  if (sum(cc) < 3) return(out)
  
  .ensure_pkg("dbscan", "dbscan::lof")
  
  X <- as.matrix(df[cc, , drop = FALSE])
  X <- apply(X, 2, as.numeric)
  if (isTRUE(scale)) X <- .standardize_matrix(X)
  
  scores <- tryCatch(as.numeric(dbscan::lof(X, minPts = as.integer(minPts))), error = function(e) NULL)
  if (is.null(scores)) return(out)
  
  out[cc] <- ifelse(scores > as.numeric(thresh), 1L, 0L)
  out
}


# -------------------------
# Summary plot (optional)
# -------------------------

plot_outlier_summary <- function(flags_df, main = "Outlier summary") {
  if (is.null(flags_df) || !is.data.frame(flags_df) || nrow(flags_df) == 0) return(invisible(NULL))
  if (ncol(flags_df) == 0) return(invisible(NULL))
  
  pct <- vapply(flags_df, function(x) mean(as.integer(x == 1L), na.rm = TRUE) * 100, numeric(1))
  ord <- order(pct, decreasing = TRUE)
  pct <- pct[ord]
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(mar = c(8, 4, 3, 1))
  barplot(pct, las = 2, ylab = "Outliers (%)", main = main)
  invisible(NULL)
}


# -------------------------
# Wrapper + workflow
# -------------------------

#' Detect outliers in a data frame.
#'
#' @param data A data frame.
#' @param methods NULL, a preset name ("basic", "robust", "ml", "all"), a string of method tokens,
#'   or a character vector of methods.
#' @param columns NULL, a string of column tokens, or a character vector of column names.
#' @param features Feature set for multivariate methods:
#'   - NULL or "columns": use the resolved `columns`
#'   - "all": use all numeric columns in `data`
#'   - "column": run multivariate methods per column (one feature at a time)
#'   - character vector: explicit feature names
#' @param config Optional list overriding defaults (nested or shorthand). See OUTLIER_DEFAULTS keys.
#' @param broadcast_multivariate If TRUE and features is not "column", copy multivariate row flags into per-column outputs.
#' @param output One of "augmented", "flags", "result".
#' @param plot FALSE or "summary".
#' @param ... Named legacy parameters (e.g., z_thresh=..., dbscan_eps=...) are accepted.
#'
#' @return A data frame (augmented or flags) or an outlier_result object.
detect_outliers <- function(
    data,
    methods = NULL,
    columns = NULL,
    features = NULL,
    config = NULL,
    broadcast_multivariate = FALSE,
    output = c("augmented", "flags", "result"),
    plot = FALSE,
    ...
) {
  .ensure_df(data)
  
  legacy <- list(...)
  user_cfg <- if (is.null(config)) list() else config
  if (!is.list(user_cfg)) stop("config must be a list (or NULL).", call. = FALSE)
  if (length(legacy) > 0) user_cfg <- utils::modifyList(user_cfg, legacy)
  
  cfg <- .merge_config(user_cfg)
  
  num_all <- .numeric_columns(data)
  if (length(num_all) == 0) stop("data must have at least one numeric column.", call. = FALSE)
  
  cols <- .parse_columns(data, columns)
  if (length(cols) == 0) stop("No columns selected.", call. = FALSE)
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) stop(sprintf("Columns not found: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  
  nonnum <- cols[!vapply(data[cols], .is_numeric_col, logical(1))]
  if (length(nonnum) > 0) stop(sprintf("Columns must be numeric: %s", paste(nonnum, collapse = ", ")), call. = FALSE)
  
  meths <- .parse_methods(methods)
  meths <- as.character(meths)
  meths <- meths[meths %in% names(OUTLIER_DEFAULTS)]
  meths <- meths[!duplicated(meths)]
  if (length(meths) == 0) stop("No valid methods selected.", call. = FALSE)
  
  # Resolve features
  if (is.null(features)) {
    features <- "columns"
  }
  feat_mode <- NULL
  feat_cols <- NULL
  if (is.character(features) && length(features) == 1L) {
    feat_mode <- tolower(trimws(features))
    if (feat_mode == "all") feat_cols <- num_all
    if (feat_mode == "columns") feat_cols <- cols
    if (feat_mode == "column") feat_cols <- NULL
    if (is.null(feat_cols) && feat_mode != "column") stop("Invalid features mode.", call. = FALSE)
  } else {
    feat_mode <- "explicit"
    feat_cols <- .as_character_vec(features)
  }
  
  if (feat_mode != "column") {
    missing_feat <- setdiff(feat_cols, names(data))
    if (length(missing_feat) > 0) stop(sprintf("Feature columns not found: %s", paste(missing_feat, collapse = ", ")), call. = FALSE)
    nonnum_feat <- feat_cols[!vapply(data[feat_cols], .is_numeric_col, logical(1))]
    if (length(nonnum_feat) > 0) stop(sprintf("Feature columns must be numeric: %s", paste(nonnum_feat, collapse = ", ")), call. = FALSE)
  }
  
  flags <- data.frame(row.names = rownames(data))
  existing_names <- names(data)
  
  add_flag <- function(name, vec) {
    nm <- .unique_name(name, c(existing_names, names(flags)))
    flags[[nm]] <<- as.integer(vec)
    invisible(nm)
  }
  
  # Univariate, applied per selected column
  for (col in cols) {
    x <- data[[col]]
    
    if ("z" %in% meths) {
      add_flag(paste0(col, "_z_outlier"), detect_z(x, thresh = cfg$z$thresh))
    }
    if ("tukey" %in% meths) {
      add_flag(paste0(col, "_tukey_outlier"), detect_tukey(x, k = cfg$tukey$k))
    }
    if ("mad" %in% meths) {
      add_flag(paste0(col, "_mad_outlier"), detect_mad(x, k = cfg$mad$k))
    }
    if ("iglewicz_hoaglin" %in% meths) {
      add_flag(paste0(col, "_iglewicz_hoaglin_outlier"), detect_iglewicz_hoaglin(x, thresh = cfg$iglewicz_hoaglin$thresh))
    }
    if ("grubbs" %in% meths) {
      add_flag(
        paste0(col, "_grubbs_outlier"),
        detect_grubbs(x, alpha = cfg$grubbs$alpha, max_outliers = cfg$grubbs$max_outliers)
      )
    }
  }
  
  # Multivariate / row-level methods
  run_multivariate_once <- function(method, df_features) {
    if (method == "mahalanobis") {
      detect_mahalanobis(
        df_features,
        quantile = cfg$mahalanobis$quantile,
        threshold = cfg$mahalanobis$threshold,
        use_cov_rob = cfg$mahalanobis$use_cov_rob
      )
    } else if (method == "elliptic_envelope") {
      detect_elliptic_envelope(df_features, contamination = cfg$elliptic_envelope$contamination)
    } else if (method == "isolation_forest") {
      detect_isolation_forest(
        df_features,
        contamination = cfg$isolation_forest$contamination,
        ndim = cfg$isolation_forest$ndim,
        ntrees = cfg$isolation_forest$ntrees,
        nthreads = cfg$isolation_forest$nthreads,
        seed = cfg$isolation_forest$seed,
        standardize_data = cfg$isolation_forest$standardize_data
      )
    } else if (method == "dbscan") {
      detect_dbscan(
        df_features,
        eps = cfg$dbscan$eps,
        minPts = cfg$dbscan$minPts,
        scale = cfg$dbscan$scale,
        borderPoints = cfg$dbscan$borderPoints
      )
    } else if (method == "one_class_svm") {
      detect_one_class_svm(
        df_features,
        nu = cfg$one_class_svm$nu,
        kernel = cfg$one_class_svm$kernel,
        gamma = cfg$one_class_svm$gamma,
        scale = cfg$one_class_svm$scale
      )
    } else if (method == "lof") {
      detect_lof(
        df_features,
        minPts = cfg$lof$minPts,
        thresh = cfg$lof$thresh,
        scale = cfg$lof$scale
      )
    } else {
      rep.int(NA_integer_, nrow(df_features))
    }
  }
  
  multi_meths <- intersect(meths, MULTIVARIATE_METHODS)
  
  if (length(multi_meths) > 0) {
    if (feat_mode == "column") {
      # Per-column application of multivariate methods (single feature)
      for (col in cols) {
        df1 <- data.frame(tmp = as.numeric(data[[col]]))
        names(df1) <- col
        for (m in multi_meths) {
          v <- tryCatch(run_multivariate_once(m, df1), error = function(e) rep.int(NA_integer_, nrow(data)))
          add_flag(paste0(col, "_", m, "_outlier"), v)
        }
      }
    } else {
      dfF <- data[, feat_cols, drop = FALSE]
      for (m in multi_meths) {
        v <- tryCatch(run_multivariate_once(m, dfF), error = function(e) rep.int(NA_integer_, nrow(data)))
        row_name <- add_flag(paste0("multivariate_", m, "_outlier"), v)
        
        if (isTRUE(broadcast_multivariate)) {
          for (col in cols) {
            add_flag(paste0(col, "_", m, "_outlier"), flags[[row_name]])
          }
        }
      }
    }
  }
  
  if (isTRUE(plot) || identical(plot, "summary")) {
    plot_outlier_summary(flags, main = "Outliers by output flag")
  }
  
  output <- match.arg(output)
  if (output == "flags") {
    return(flags)
  }
  
  augmented <- cbind(data, flags)
  
  if (output == "augmented") {
    return(augmented)
  }
  
  structure(
    list(
      data = data,
      augmented = augmented,
      flags = flags,
      methods = meths,
      columns = cols,
      features = features,
      config = cfg
    ),
    class = "outlier_result"
  )
}

print.outlier_result <- function(x, ...) {
  cat("outlier_result\n")
  cat(sprintf("  rows: %d\n", nrow(x$data)))
  cat(sprintf("  flags: %d\n", ncol(x$flags)))
  cat(sprintf("  methods: %s\n", paste(x$methods, collapse = ", ")))
  invisible(x)
}

summary.outlier_result <- function(object, ...) {
  flags_df <- object$flags
  pct <- vapply(flags_df, function(x) mean(as.integer(x == 1L), na.rm = TRUE), numeric(1))
  cnt <- vapply(flags_df, function(x) sum(as.integer(x == 1L), na.rm = TRUE), numeric(1))
  data.frame(
    flag = names(flags_df),
    outliers = as.integer(cnt),
    rate = as.numeric(pct),
    row.names = NULL
  )
}

# Produces a callable workflow that reuses the same arguments
make_outlier_workflow <- function(
    methods = NULL,
    columns = NULL,
    features = NULL,
    config = NULL,
    broadcast_multivariate = FALSE,
    output = c("augmented", "flags", "result"),
    plot = FALSE,
    ...
) {
  force(methods); force(columns); force(features); force(config)
  force(broadcast_multivariate); force(output); force(plot)
  dots <- list(...)
  function(data) {
    do.call(
      detect_outliers,
      c(
        list(
          data = data,
          methods = methods,
          columns = columns,
          features = features,
          config = config,
          broadcast_multivariate = broadcast_multivariate,
          output = output,
          plot = plot
        ),
        dots
      )
    )
  }
}


#####################################
# User Acceptance Testing (UAT)
#####################################

run_uat <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("The 'testthat' package is required for running the UAT. Please install it.")
  }
  
  testthat::local_edition(3)
  
  testthat::test_that("detect_z identifies extreme values", {
    x <- c(rep(10, 10), 100)
    out <- detect_z(x, thresh = 1.96)
    testthat::expect_equal(length(out), length(x))
    testthat::expect_true(sum(out, na.rm = TRUE) >= 1)
  })
  
  testthat::test_that("detect_tukey flags values beyond fences", {
    x <- c(1, 2, 3, 4, 50)
    out <- detect_tukey(x, k = 1.5)
    testthat::expect_true(any(out == 1, na.rm = TRUE))
  })
  
  testthat::test_that("detect_grubbs warns on too few values", {
    x <- c(5, NA)
    testthat::expect_warning(detect_grubbs(x))
  })
  
  testthat::test_that("detect_mad flags extreme deviations", {
    x <- c(5, 5, 5, 5, 20)
    out <- detect_mad(x, k = 3)
    testthat::expect_true(any(out == 1, na.rm = TRUE))
  })
  
  testthat::test_that("detect_iglewicz_hoaglin works as expected", {
    x <- c(1, 1, 1, 1, 10)
    out <- detect_iglewicz_hoaglin(x, thresh = 3.5)
    testthat::expect_true(any(out == 1, na.rm = TRUE))
  })
  
  set.seed(123)
  df_multi <- data.frame(
    A = rnorm(50, 10, 2),
    B = rnorm(50, 20, 5),
    C = rnorm(50, 30, 10)
  )
  df_multi[c(5, 15), "A"] <- c(25, -5)
  df_multi[c(10, 20), "B"] <- c(40, 5)
  df_multi[c(8, 22), "C"] <- c(60, 0)
  
  testthat::test_that("detect_mahalanobis flags multivariate outliers", {
    out <- detect_mahalanobis(df_multi, threshold = qchisq(0.95, df = ncol(df_multi)))
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(sum(out, na.rm = TRUE) >= 1)
  })
  
  testthat::test_that("detect_dbscan returns binary flags (if installed)", {
    testthat::skip_if_not_installed("dbscan")
    out <- detect_dbscan(df_multi, eps = 1, minPts = 3, scale = TRUE)
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(all(is.na(out) | out %in% c(0L, 1L)))
  })
  
  testthat::test_that("detect_one_class_svm returns binary flags (if installed)", {
    testthat::skip_if_not_installed("e1071")
    out <- detect_one_class_svm(df_multi, nu = 0.05)
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(all(is.na(out) | out %in% c(0L, 1L)))
  })
  
  testthat::test_that("detect_elliptic_envelope returns binary flags (if installed)", {
    testthat::skip_if_not_installed("MASS")
    out <- detect_elliptic_envelope(df_multi, contamination = 0.1)
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(all(is.na(out) | out %in% c(0L, 1L)))
  })
  
  testthat::test_that("detect_lof returns binary flags (if installed)", {
    testthat::skip_if_not_installed("dbscan")
    out <- detect_lof(df_multi, minPts = 5, thresh = 1.5, scale = TRUE)
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(all(is.na(out) | out %in% c(0L, 1L)))
  })
  
  testthat::test_that("detect_isolation_forest returns correct length (if installed)", {
    testthat::skip_if_not_installed("isotree")
    out <- detect_isolation_forest(df_multi, contamination = 0.1, nthreads = 1, seed = 123)
    testthat::expect_equal(length(out), nrow(df_multi))
    testthat::expect_true(all(is.na(out) | out %in% c(0L, 1L)))
  })
  
  testthat::test_that("detect_outliers() returns original plus added columns", {
    example_data <- data.frame(
      A = rnorm(100, 10, 2),
      B = rnorm(100, 20, 5),
      C = rnorm(100, 30, 10),
      D = sample(letters, 100, replace = TRUE)
    )
    example_data[c(25, 50, 75), "B"] <- c(40, 10, 35)
    example_data[c(10, 90), "C"] <- c(5, 50)
    
    res <- detect_outliers(
      example_data,
      methods = c("z", "tukey", "mahalanobis", "grubbs", "mad", "iglewicz_hoaglin"),
      columns = c("A", "B", "C"),
      features = "columns",
      output = "augmented"
    )
    
    testthat::expect_true(ncol(res) > ncol(example_data))
    flag_cols <- grep("_outlier$", names(res), value = TRUE)
    testthat::expect_true(length(flag_cols) > 0)
  })
  
  invisible(TRUE)
}


##############################
# Example usage (notes)
##############################
# 1) One-shot:
#    out <- detect_outliers(df, methods="all", columns="A B C", features="columns", output="augmented")
#
# 2) Only flags:
#    flags <- detect_outliers(df, methods="basic", columns=NULL, output="flags")
#
# 3) Repeatable workflow:
#    wf <- make_outlier_workflow(methods="ml", columns="A B C", features="all", config=list(isolation_forest=list(ntrees=200)), output="flags")
#    flags1 <- wf(df1)
#    flags2 <- wf(df2)
#
# 4) Run UAT:
#    run_uat()
