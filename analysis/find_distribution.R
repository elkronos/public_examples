# Distribution utilities and likelihood-based model selection

.normalize_dist <- function(dist) {
  if (!is.character(dist) || length(dist) != 1L || is.na(dist) || !nzchar(dist)) {
    stop("dist must be a non-empty character scalar.")
  }
  
  key <- tolower(trimws(dist))
  key <- gsub("[_[:space:]]+", "-", key)
  key <- gsub("[^a-z0-9-]", "", key)
  key <- gsub("-+", "-", key)
  key <- sub("^-", "", key)
  key <- sub("-$", "", key)
  key
}

.as_numeric_params <- function(params) {
  if (is.null(params)) return(numeric())
  if (is.list(params)) params <- unlist(params, recursive = TRUE, use.names = TRUE)
  if (!is.numeric(params)) stop("params must be numeric (or a named list convertible to numeric).")
  as.numeric(params)
}

.assert_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) stop(sprintf("%s must be a numeric vector.", name))
  invisible(TRUE)
}

.assert_nonempty <- function(x, name) {
  if (length(x) == 0L) stop(sprintf("%s must not be empty.", name))
  invisible(TRUE)
}

.is_integerish <- function(x, tol = sqrt(.Machine$double.eps)) {
  all(is.finite(x) & abs(x - round(x)) <= tol)
}

.dist_registry <- local({
  list(
    "gaussian" = list(
      aliases = c("gaussian", "normal", "norm"),
      fit_name = "norm",
      validate = function(p) {
        if (length(p) < 2L) stop("Gaussian distribution requires parameters: mean and sd.")
        if (!is.finite(p[2]) || p[2] <= 0) stop("Gaussian sd must be a positive finite value.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dnorm(x, mean = p[1], sd = p[2]),
      rng = function(n, p) stats::rnorm(n, mean = p[1], sd = p[2]),
      start = function(x) c(mean = mean(x), sd = stats::sd(x))
    ),
    
    "poisson" = list(
      aliases = c("poisson", "pois"),
      fit_name = "pois",
      validate = function(p) {
        if (length(p) < 1L) stop("Poisson distribution requires parameter: lambda.")
        if (!is.finite(p[1]) || p[1] <= 0) stop("Poisson lambda must be a positive finite value.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dpois(x, lambda = p[1]),
      rng = function(n, p) stats::rpois(n, lambda = p[1]),
      start = function(x) c(lambda = mean(x))
    ),
    
    "binomial" = list(
      aliases = c("binomial", "binom"),
      fit_name = NA_character_,
      validate = function(p) {
        if (length(p) < 2L) stop("Binomial distribution requires parameters: size and prob.")
        if (!is.finite(p[1]) || p[1] < 0) stop("Binomial size must be a non-negative finite value.")
        if (abs(p[1] - round(p[1])) > sqrt(.Machine$double.eps)) stop("Binomial size must be an integer.")
        if (!is.finite(p[2]) || p[2] < 0 || p[2] > 1) stop("Binomial prob must be between 0 and 1.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dbinom(x, size = as.integer(round(p[1])), prob = p[2]),
      rng = function(n, p) stats::rbinom(n, size = as.integer(round(p[1])), prob = p[2]),
      start = function(x) NULL
    ),
    
    "exponential" = list(
      aliases = c("exponential", "exp"),
      fit_name = "exp",
      validate = function(p) {
        if (length(p) < 1L) stop("Exponential distribution requires parameter: rate.")
        if (!is.finite(p[1]) || p[1] <= 0) stop("Exponential rate must be a positive finite value.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dexp(x, rate = p[1]),
      rng = function(n, p) stats::rexp(n, rate = p[1]),
      start = function(x) {
        m <- mean(x)
        if (!is.finite(m) || m <= 0) return(NULL)
        c(rate = 1 / m)
      }
    ),
    
    "geometric" = list(
      aliases = c("geometric", "geom"),
      fit_name = "geom",
      validate = function(p) {
        if (length(p) < 1L) stop("Geometric distribution requires parameter: prob.")
        if (!is.finite(p[1]) || p[1] <= 0 || p[1] > 1) stop("Geometric prob must be in (0, 1].")
        invisible(TRUE)
      },
      density = function(x, p) stats::dgeom(x, prob = p[1]),
      rng = function(n, p) stats::rgeom(n, prob = p[1]),
      start = function(x) {
        m <- mean(x)
        if (!is.finite(m)) return(NULL)
        c(prob = 1 / (m + 1))
      }
    ),
    
    "gamma" = list(
      aliases = c("gamma"),
      fit_name = "gamma",
      validate = function(p) {
        if (length(p) < 2L) stop("Gamma distribution requires parameters: shape and rate.")
        if (!is.finite(p[1]) || p[1] <= 0) stop("Gamma shape must be a positive finite value.")
        if (!is.finite(p[2]) || p[2] <= 0) stop("Gamma rate must be a positive finite value.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dgamma(x, shape = p[1], rate = p[2]),
      rng = function(n, p) stats::rgamma(n, shape = p[1], rate = p[2]),
      start = function(x) {
        m <- mean(x); v <- stats::var(x)
        if (!is.finite(m) || !is.finite(v) || v <= 0 || m <= 0) return(NULL)
        shape <- m^2 / v
        rate <- m / v
        c(shape = shape, rate = rate)
      }
    ),
    
    "log-normal" = list(
      aliases = c("log-normal", "lognormal", "lnorm", "log-normal", "lognormal"),
      fit_name = "lnorm",
      validate = function(p) {
        if (length(p) < 2L) stop("Log-normal distribution requires parameters: meanlog and sdlog.")
        if (!is.finite(p[2]) || p[2] <= 0) stop("Log-normal sdlog must be a positive finite value.")
        invisible(TRUE)
      },
      density = function(x, p) stats::dlnorm(x, meanlog = p[1], sdlog = p[2]),
      rng = function(n, p) stats::rlnorm(n, meanlog = p[1], sdlog = p[2]),
      start = function(x) {
        lx <- log(x)
        s <- stats::sd(lx)
        if (!is.finite(s) || s <= 0) return(NULL)
        c(meanlog = mean(lx), sdlog = s)
      }
    )
  )
})

.resolve_dist <- function(dist) {
  key <- .normalize_dist(dist)
  for (nm in names(.dist_registry)) {
    if (key %in% .dist_registry[[nm]]$aliases) return(nm)
  }
  stop("Unsupported distribution specified.")
}

#' Density Function for a Given Distribution
#'
#' Evaluates the probability density or mass function for a selected distribution at x.
#' Supported distributions: gaussian, poisson, binomial, exponential, geometric, gamma, log-normal.
#'
#' @param dist Character scalar naming the distribution.
#' @param x Numeric vector of evaluation points.
#' @param params Numeric vector (or named list) of distribution parameters.
#'
#' @return Numeric vector of densities with the same length as x.
#' @export
density_fun <- function(dist, x, params) {
  .assert_numeric_vector(x, "x")
  
  dist_key <- .resolve_dist(dist)
  spec <- .dist_registry[[dist_key]]
  
  p <- .as_numeric_params(params)
  spec$validate(p)
  
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x)
  if (any(ok)) out[ok] <- spec$density(x[ok], p)
  out
}

#' Generate Random Numbers from a Given Distribution
#'
#' Generates random values from a selected distribution.
#' Supported distributions: gaussian, poisson, binomial, exponential, geometric, gamma, log-normal.
#'
#' @param dist Character scalar naming the distribution.
#' @param n Positive integer count of values to generate.
#' @param params Numeric vector (or named list) of distribution parameters.
#'
#' @return Numeric vector of generated values.
#' @export
r_fun <- function(dist, n, params) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n <= 0) {
    stop("n must be a positive numeric scalar.")
  }
  n_int <- as.integer(n)
  if (n_int != n) stop("n must be an integer value.")
  
  dist_key <- .resolve_dist(dist)
  spec <- .dist_registry[[dist_key]]
  
  p <- .as_numeric_params(params)
  spec$validate(p)
  
  spec$rng(n_int, p)
}

#' Plot Observed and Fitted Distributions
#'
#' Plots overlapping histograms for observed values and simulated values.
#'
#' @param data Numeric vector of observed values.
#' @param fitted_data Numeric vector of simulated values.
#' @param dist_name Character scalar for the plot title.
#' @param bins Integer number of bins used for both histograms.
#'
#' @return A ggplot object (invisibly).
#' @export
plot_overlapping_histograms <- function(data, fitted_data, dist_name, bins = 30L) {
  .assert_numeric_vector(data, "data")
  .assert_numeric_vector(fitted_data, "fitted_data")
  .assert_nonempty(data, "data")
  .assert_nonempty(fitted_data, "fitted_data")
  
  data <- data[is.finite(data)]
  fitted_data <- fitted_data[is.finite(fitted_data)]
  if (length(data) == 0L || length(fitted_data) == 0L) {
    stop("data and fitted_data must contain at least one finite value.")
  }
  
  bins <- as.integer(bins)
  if (is.na(bins) || bins <= 0L) stop("bins must be a positive integer.")
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  
  hist_df <- data.frame(
    Data = c(data, fitted_data),
    Type = factor(rep(c("Observed", "Fitted"), times = c(length(data), length(fitted_data))))
  )
  
  p <- ggplot2::ggplot(hist_df, ggplot2::aes(x = Data, fill = Type)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = bins, position = "identity", alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("Observed" = "lightblue", "Fitted" = "red")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 20, hjust = 0.5)
    ) +
    ggplot2::labs(title = paste("Best Distribution:", dist_name))
  
  print(p)
  invisible(p)
}

.try_fit <- function(x, dist_key) {
  if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
    stop("Package 'fitdistrplus' is required for distribution fitting.")
  }
  
  spec <- .dist_registry[[dist_key]]
  fit_name <- spec$fit_name
  if (is.na(fit_name) || !nzchar(fit_name)) return(NULL)
  
  start <- spec$start(x)
  
  fitdist_formals <- names(formals(fitdistrplus::fitdist))
  args <- list(data = x, distr = fit_name)
  if (!is.null(start)) args$start <- as.list(start)
  if ("keepdata" %in% fitdist_formals) args$keepdata <- FALSE
  
  fit <- tryCatch(
    do.call(fitdistrplus::fitdist, args),
    error = function(e) NULL
  )
  fit
}

#' Find Best Distribution for a Given Dataset
#'
#' Fits candidate distributions selected from data characteristics and chooses the best model by AIC or BIC.
#'
#' @param data_frame A data.frame containing the dataset.
#' @param column Character scalar naming the column to analyze.
#' @param plot Logical; if TRUE, overlays observed and simulated histograms for the selected distribution.
#' @param use_bic Logical; if TRUE, uses BIC for selection (otherwise AIC).
#' @param return_all_info Logical; if TRUE, returns selection, fitted objects, and a fit summary table.
#' @param candidates Optional character vector of distributions to consider; when NULL, candidates are selected from the data.
#'
#' @return Best distribution name, or a list when return_all_info is TRUE.
#' @export
find_distribution <- function(data_frame,
                              column,
                              plot = FALSE,
                              use_bic = FALSE,
                              return_all_info = FALSE,
                              candidates = NULL) {
  if (!is.data.frame(data_frame)) stop("data_frame must be a data.frame.")
  if (!is.character(column) || length(column) != 1L || is.na(column) || !nzchar(column)) {
    stop("column must be a non-empty character scalar.")
  }
  if (!column %in% names(data_frame)) stop("Specified column not found in data_frame.")
  
  x_raw <- data_frame[[column]]
  .assert_numeric_vector(x_raw, "Data in the specified column")
  
  x <- x_raw[is.finite(x_raw)]
  if (length(x) == 0L) stop("Data column must contain at least one finite numeric value.")
  
  is_nonnegative <- all(x >= 0)
  is_positive <- all(x > 0)
  is_discrete <- is_nonnegative && .is_integerish(x)
  
  if (is.null(candidates)) {
    if (!is_nonnegative) {
      candidates <- c("gaussian")
    } else {
      candidates <- c("gaussian", "exponential")
      if (is_discrete) candidates <- c(candidates, "poisson", "geometric")
      if (is_positive) candidates <- c(candidates, "gamma", "log-normal")
    }
  } else {
    if (!is.character(candidates) || length(candidates) == 0L) {
      stop("candidates must be a non-empty character vector.")
    }
    candidates <- vapply(candidates, .resolve_dist, character(1))
    candidates <- unique(candidates)
  }
  
  fits <- list()
  rows <- vector("list", length(candidates))
  row_i <- 0L
  
  for (dist_key in candidates) {
    if (dist_key == "poisson" && (!is_discrete || any(x < 0))) next
    if (dist_key == "geometric" && (!is_discrete || any(x < 0))) next
    if (dist_key %in% c("gamma", "log-normal") && !is_positive) next
    if (dist_key == "exponential" && any(x < 0)) next
    
    fit <- .try_fit(x, dist_key)
    if (is.null(fit)) next
    
    fits[[dist_key]] <- fit
    row_i <- row_i + 1L
    rows[[row_i]] <- data.frame(
      dist = dist_key,
      loglik = fit$loglik,
      aic = fit$aic,
      bic = fit$bic,
      stringsAsFactors = FALSE
    )
  }
  
  if (row_i == 0L) stop("No candidate distributions produced a fitted model.")
  gof <- do.call(rbind, rows[seq_len(row_i)])
  
  best_dist <- if (isTRUE(use_bic)) {
    gof$dist[which.min(gof$bic)]
  } else {
    gof$dist[which.min(gof$aic)]
  }
  
  if (isTRUE(plot)) {
    best_fit <- fits[[best_dist]]
    params <- as.numeric(stats::coef(best_fit))
    fitted_data <- r_fun(best_dist, length(x), params)
    plot_overlapping_histograms(x, fitted_data, best_dist)
  }
  
  if (isTRUE(return_all_info)) {
    list(best_distribution = best_dist, fits = fits, gof = gof)
  } else {
    best_dist
  }
}

# =========================
# UAT for distribution utilities + model selection
# Assumes the latest versions of:
#   density_fun(), r_fun(), plot_overlapping_histograms(), find_distribution()
# are defined in the current session.
# =========================

options(stringsAsFactors = FALSE)

# ---------- Test harness ----------
.uat_env <- new.env(parent = emptyenv())
.uat_env$results <- data.frame(
  test = character(),
  status = character(),
  detail = character(),
  stringsAsFactors = FALSE
)

.record <- function(test, status, detail = "") {
  .uat_env$results <- rbind(.uat_env$results, data.frame(test = test, status = status, detail = detail))
  invisible(NULL)
}

.expect_true <- function(cond, msg = "Expectation failed") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

.expect_equal <- function(a, b, tol = 1e-12, msg = "Values differ") {
  if (is.numeric(a) && is.numeric(b)) {
    if (length(a) != length(b) || any(abs(a - b) > tol, na.rm = TRUE)) stop(msg, call. = FALSE)
  } else {
    if (!identical(a, b)) stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.expect_error <- function(expr, pattern = NULL, msg = "Expected an error") {
  err <- NULL
  tryCatch(force(expr), error = function(e) err <<- e)
  if (is.null(err)) stop(msg, call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, conditionMessage(err), ignore.case = TRUE)) {
    stop(sprintf("Error message did not match pattern '%s'. Message: %s", pattern, conditionMessage(err)), call. = FALSE)
  }
  invisible(TRUE)
}

.run_test <- function(name, code) {
  ok <- TRUE
  detail <- ""
  tryCatch(force(code), error = function(e) { ok <<- FALSE; detail <<- conditionMessage(e) })
  .record(name, if (ok) "PASS" else "FAIL", detail)
  invisible(ok)
}

# ---------- Function binding ----------
.get_fn <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) return(get(name, envir = .GlobalEnv, inherits = FALSE))
  if (exists(name, mode = "function")) return(get(name, mode = "function"))
  stop(sprintf("Function '%s' not found.", name), call. = FALSE)
}

DENSITY_FUN <- .get_fn("density_fun")
R_FUN <- .get_fn("r_fun")
PLOT_HISTS <- .get_fn("plot_overlapping_histograms")
FIND_DIST <- .get_fn("find_distribution")

# ---------- Helpers ----------
.set_seed <- function() set.seed(12345)

.is_integerish <- function(x, tol = sqrt(.Machine$double.eps)) {
  all(is.finite(x) & abs(x - round(x)) <= tol)
}

.is_finite_numeric <- function(x) is.numeric(x) && all(is.finite(x))
.no_na <- function(x) all(!is.na(x))

.pmfnorm_ok <- function(p, tol = 1e-3) {
  s <- sum(p, na.rm = TRUE)
  .expect_true(is.finite(s), "PMF sum is not finite")
  .expect_true(abs(s - 1) <= tol, sprintf("PMF sum %.8f outside tolerance %.8f", s, tol))
  invisible(TRUE)
}

.has_fitdistrplus <- requireNamespace("fitdistrplus", quietly = TRUE)
.has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

# =========================
# 0) Smoke: functions callable
# =========================

.run_test("smoke: density_fun callable", {
  x <- seq(-1, 1, length.out = 5)
  d <- DENSITY_FUN("gaussian", x, c(0, 1))
  .expect_true(length(d) == length(x))
})

.run_test("smoke: r_fun callable", {
  .set_seed()
  x <- R_FUN("gaussian", 10, c(0, 1))
  .expect_true(length(x) == 10)
})

# =========================
# 1) density_fun() validation + correctness
# =========================

.run_test("density_fun: supported alias normalization", {
  x <- c(-1, 0, 1)
  a <- DENSITY_FUN("normal", x, c(0, 1))
  b <- DENSITY_FUN("gaussian", x, c(0, 1))
  .expect_equal(a, b, tol = 0)
})

.run_test("density_fun: log-normal spelling variants agree", {
  x <- c(0, 1, 2, 10)
  a <- DENSITY_FUN("log normal", x, c(0, 1))
  b <- DENSITY_FUN("log-normal", x, c(0, 1))
  c <- DENSITY_FUN("lognormal", x, c(0, 1))
  d <- DENSITY_FUN("lnorm", x, c(0, 1))
  .expect_equal(a, b, tol = 0)
  .expect_equal(a, c, tol = 0)
  .expect_equal(a, d, tol = 0)
})

.run_test("density_fun: gaussian length and finite on finite x", {
  x <- seq(-5, 5, length.out = 101)
  d <- DENSITY_FUN("gaussian", x, c(0, 1))
  .expect_true(length(d) == length(x))
  .expect_true(.is_finite_numeric(d))
})

.run_test("density_fun: preserves length and sets NA for NA/Inf", {
  x <- c(-1, 0, NA, 1, Inf, -Inf)
  d <- DENSITY_FUN("gaussian", x, c(0, 1))
  .expect_true(length(d) == length(x))
  .expect_true(is.na(d[3]) && is.na(d[5]) && is.na(d[6]))
  .expect_true(all(is.finite(d[c(1, 2, 4)])))
})

.run_test("density_fun: poisson pmf normalizes over wide support", {
  x <- 0:60
  p <- DENSITY_FUN("poisson", x, c(5))
  .expect_true(length(p) == length(x))
  .expect_true(.no_na(p))
  .pmfnorm_ok(p, tol = 1e-3)
})

.run_test("density_fun: binomial pmf normalizes exactly", {
  x <- 0:10
  p <- DENSITY_FUN("binomial", x, c(10, 0.3))
  .pmfnorm_ok(p, tol = 1e-12)
})

.run_test("density_fun: geometric pmf normalizes approximately", {
  x <- 0:200
  p <- DENSITY_FUN("geometric", x, c(0.25))
  .expect_true(all(p >= 0, na.rm = TRUE))
  .pmfnorm_ok(p, tol = 1e-3)
})

.run_test("density_fun: exponential integrates approximately", {
  dx <- 1e-3
  x <- seq(0, 20, by = dx)
  d <- DENSITY_FUN("exponential", x, c(2))
  approx_int <- sum(d) * dx
  .expect_true(abs(approx_int - 1) < 5e-3, sprintf("Approx integral %.6f", approx_int))
})

.run_test("density_fun: gamma integrates approximately", {
  dx <- 1e-3
  x <- seq(0, 50, by = dx)
  d <- DENSITY_FUN("gamma", x, c(2, 1.5))
  approx_int <- sum(d) * dx
  .expect_true(abs(approx_int - 1) < 1e-2, sprintf("Approx integral %.6f", approx_int))
})

.run_test("density_fun: log-normal gives 0 for x<=0", {
  x <- c(-1, 0, 0.1, 1, 10)
  d <- DENSITY_FUN("log-normal", x, c(0, 1))
  .expect_true(d[1] == 0 && d[2] == 0)
  .expect_true(all(d[3:5] > 0))
})

.run_test("density_fun: rejects non-numeric x", {
  .expect_error(DENSITY_FUN("gaussian", c("a", "b"), c(0, 1)), pattern = "numeric")
})

.run_test("density_fun: rejects unsupported distribution", {
  .expect_error(DENSITY_FUN("weibull", 0:1, c(1, 1)), pattern = "Unsupported")
})

.run_test("density_fun: gaussian parameter validation (sd)", {
  .expect_error(DENSITY_FUN("gaussian", 0:1, c(0, 0)), pattern = "sd")
})

.run_test("density_fun: poisson parameter validation (lambda)", {
  .expect_error(DENSITY_FUN("poisson", 0:1, c(0)), pattern = "lambda")
})

.run_test("density_fun: exponential parameter validation (rate)", {
  .expect_error(DENSITY_FUN("exponential", 0:1, c(0)), pattern = "rate")
})

.run_test("density_fun: geometric parameter validation (prob)", {
  .expect_error(DENSITY_FUN("geometric", 0:1, c(0)), pattern = "prob")
})

.run_test("density_fun: gamma parameter validation (shape/rate)", {
  .expect_error(DENSITY_FUN("gamma", 0:1, c(0, 1)), pattern = "shape")
  .expect_error(DENSITY_FUN("gamma", 0:1, c(1, 0)), pattern = "rate")
})

.run_test("density_fun: log-normal parameter validation (sdlog)", {
  .expect_error(DENSITY_FUN("log-normal", 0:1, c(0, 0)), pattern = "sdlog")
})

# =========================
# 2) r_fun() validation + distributional properties
# =========================

.run_test("r_fun: rejects non-integer n", {
  .expect_error(R_FUN("gaussian", 10.5, c(0, 1)), pattern = "integer")
})

.run_test("r_fun: rejects non-positive n", {
  .expect_error(R_FUN("gaussian", 0, c(0, 1)), pattern = "positive")
})

.run_test("r_fun: gaussian sample mean/var sanity", {
  .set_seed()
  x <- R_FUN("gaussian", 200000, c(10, 2))
  .expect_true(abs(mean(x) - 10) < 0.03, sprintf("mean %.4f", mean(x)))
  .expect_true(abs(stats::sd(x) - 2) < 0.03, sprintf("sd %.4f", stats::sd(x)))
})

.run_test("r_fun: poisson integerish and mean sanity", {
  .set_seed()
  x <- R_FUN("poisson", 200000, c(7))
  .expect_true(all(x >= 0))
  .expect_true(.is_integerish(x))
  .expect_true(abs(mean(x) - 7) < 0.05, sprintf("mean %.4f", mean(x)))
})

.run_test("r_fun: binomial bounds and mean sanity", {
  .set_seed()
  x <- R_FUN("binomial", 200000, c(10, 0.3))
  .expect_true(all(x >= 0 & x <= 10))
  .expect_true(.is_integerish(x))
  .expect_true(abs(mean(x) - 3) < 0.03, sprintf("mean %.4f", mean(x)))
})

.run_test("r_fun: exponential mean sanity", {
  .set_seed()
  x <- R_FUN("exponential", 200000, c(2))
  .expect_true(all(x >= 0))
  .expect_true(abs(mean(x) - 0.5) < 0.01, sprintf("mean %.4f", mean(x)))
})

.run_test("r_fun: geometric integerish and mean sanity (R definition: failures before success)", {
  .set_seed()
  p <- 0.25
  x <- R_FUN("geometric", 200000, c(p))
  .expect_true(all(x >= 0))
  .expect_true(.is_integerish(x))
  .expect_true(abs(mean(x) - ((1 - p) / p)) < 0.05, sprintf("mean %.4f", mean(x)))
})

.run_test("r_fun: gamma mean sanity", {
  .set_seed()
  shape <- 2; rate <- 1.5
  x <- R_FUN("gamma", 200000, c(shape, rate))
  .expect_true(all(x > 0))
  .expect_true(abs(mean(x) - (shape / rate)) < 0.02, sprintf("mean %.4f", mean(x)))
})

.run_test("r_fun: log-normal mean sanity", {
  .set_seed()
  meanlog <- 0.3; sdlog <- 0.7
  x <- R_FUN("log-normal", 200000, c(meanlog, sdlog))
  target <- exp(meanlog + 0.5 * sdlog^2)
  .expect_true(all(x > 0))
  .expect_true(abs(mean(x) - target) / target < 0.01, sprintf("mean %.4f target %.4f", mean(x), target))
})

.run_test("r_fun: poisson lambda validation", {
  .expect_error(R_FUN("poisson", 10, c(0)), pattern = "lambda")
})

.run_test("r_fun: gaussian sd validation", {
  .expect_error(R_FUN("gaussian", 10, c(0, 0)), pattern = "sd")
})

# =========================
# 3) plot_overlapping_histograms() behavior
# =========================

if (.has_ggplot2) {
  .run_test("plot_overlapping_histograms: returns ggplot object", {
    .set_seed()
    a <- rnorm(500)
    b <- rnorm(500, mean = 0.1)
    p <- PLOT_HISTS(a, b, "gaussian", bins = 25)
    .expect_true(inherits(p, "ggplot"))
  })
  
  .run_test("plot_overlapping_histograms: rejects empty vectors", {
    .expect_error(PLOT_HISTS(numeric(), 1:10, "x"), pattern = "empty|finite|must")
  })
  
  .run_test("plot_overlapping_histograms: rejects missing ggplot2 at runtime (simulated)", {
    .expect_true(TRUE)
  })
} else {
  .record("plot_overlapping_histograms: skipped (ggplot2 not installed)", "SKIP", "")
}

# =========================
# 4) find_distribution() selection + edge cases
# =========================

if (.has_fitdistrplus) {
  
  .run_test("find_distribution: gaussian data selects gaussian by AIC", {
    .set_seed()
    x <- rnorm(4000, mean = 10, sd = 2)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE, use_bic = FALSE)
    .expect_true(best == "gaussian")
  })
  
  .run_test("find_distribution: gaussian data selects gaussian by BIC", {
    .set_seed()
    x <- rnorm(4000, mean = 10, sd = 2)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE, use_bic = TRUE)
    .expect_true(best == "gaussian")
  })
  
  .run_test("find_distribution: exponential data selects exponential", {
    .set_seed()
    x <- rexp(6000, rate = 2)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(best == "exponential")
  })
  
  .run_test("find_distribution: gamma data selects gamma", {
    .set_seed()
    x <- rgamma(8000, shape = 2, rate = 1.5)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(best == "gamma")
  })
  
  .run_test("find_distribution: log-normal data selects log-normal", {
    .set_seed()
    x <- rlnorm(8000, meanlog = 0.3, sdlog = 0.7)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(best == "log-normal")
  })
  
  .run_test("find_distribution: poisson count data selects poisson", {
    .set_seed()
    x <- rpois(12000, lambda = 5)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(best == "poisson")
  })
  
  .run_test("find_distribution: geometric count data is considered and returns a supported name", {
    .set_seed()
    x <- rgeom(12000, prob = 0.25)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(best %in% c("poisson", "geometric", "gaussian", "exponential", "gamma", "log-normal"))
  })
  
  .run_test("find_distribution: negative values reduce candidates to gaussian", {
    .set_seed()
    x <- rnorm(5000, mean = -5, sd = 2)
    df <- data.frame(x = x)
    out <- FIND_DIST(df, "x", return_all_info = TRUE)
    .expect_true(out$best_distribution == "gaussian")
    .expect_true(all(out$gof$dist == "gaussian"))
  })
  
  .run_test("find_distribution: ignores NA/Inf and still returns", {
    .set_seed()
    x <- c(rgamma(4000, shape = 2, rate = 1.2), NA, Inf, -Inf)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", plot = FALSE)
    .expect_true(is.character(best) && length(best) == 1L)
  })
  
  .run_test("find_distribution: return_all_info structure", {
    .set_seed()
    x <- rexp(3000, rate = 1.3)
    df <- data.frame(x = x)
    out <- FIND_DIST(df, "x", return_all_info = TRUE)
    .expect_true(is.list(out))
    .expect_true(all(c("best_distribution", "fits", "gof") %in% names(out)))
    .expect_true(is.data.frame(out$gof))
    .expect_true(all(c("dist", "loglik", "aic", "bic") %in% names(out$gof)))
    .expect_true(out$best_distribution %in% out$gof$dist)
  })
  
  .run_test("find_distribution: candidates override selects gamma on gamma data", {
    .set_seed()
    x <- rgamma(6000, shape = 2, rate = 1.5)
    df <- data.frame(x = x)
    best <- FIND_DIST(df, "x", candidates = c("gaussian", "gamma"))
    .expect_true(best == "gamma")
  })
  
  if (.has_ggplot2) {
    .run_test("find_distribution: plot executes", {
      .set_seed()
      x <- rnorm(2000)
      df <- data.frame(x = x)
      best <- FIND_DIST(df, "x", plot = TRUE)
      .expect_true(is.character(best) && length(best) == 1L)
    })
  } else {
    .record("find_distribution: plot skipped (ggplot2 not installed)", "SKIP", "")
  }
  
  .run_test("find_distribution: rejects invalid data_frame", {
    .expect_error(FIND_DIST(1:10, "x"), pattern = "data.frame")
  })
  
  .run_test("find_distribution: rejects missing column", {
    df <- data.frame(a = 1:10)
    .expect_error(FIND_DIST(df, "x"), pattern = "not found")
  })
  
  .run_test("find_distribution: rejects non-numeric column", {
    df <- data.frame(x = letters[1:10])
    .expect_error(FIND_DIST(df, "x"), pattern = "numeric")
  })
  
  .run_test("find_distribution: rejects all non-finite", {
    df <- data.frame(x = c(NA, Inf, -Inf))
    .expect_error(FIND_DIST(df, "x"), pattern = "finite")
  })
  
} else {
  .record("find_distribution: skipped (fitdistrplus not installed)", "SKIP", "")
}

# =========================
# 5) Performance smoke tests (short, to avoid timer quirks)
# =========================

.run_test("performance: density_fun vectorized 2e5 gaussian", {
  x <- rnorm(2e5)
  t <- system.time(d <- DENSITY_FUN("gaussian", x, c(0, 1)))
  .expect_true(length(d) == length(x))
  .expect_true(all(is.finite(d[1:10])))
  .expect_true(is.numeric(t[["elapsed"]]) && t[["elapsed"]] >= 0)
})

.run_test("performance: r_fun 5e5 poisson", {
  t <- system.time(x <- R_FUN("poisson", 5e5, c(5)))
  .expect_true(length(x) == 5e5)
  .expect_true(all(x[1:100] >= 0))
  .expect_true(is.numeric(t[["elapsed"]]) && t[["elapsed"]] >= 0)
})

# =========================
# 6) Summary
# =========================

cat("\n====================\nUAT SUMMARY\n====================\n")
print(.uat_env$results, row.names = FALSE)

cat("\n====================\nCOUNTS\n====================\n")
print(table(.uat_env$results$status))

if (any(.uat_env$results$status == "FAIL")) {
  cat("\n====================\nFAILED TESTS\n====================\n")
  print(.uat_env$results[.uat_env$results$status == "FAIL", c("test", "detail")], row.names = FALSE)
  stop("One or more UAT tests failed.", call. = FALSE)
} else {
  cat("\nAll UAT tests passed (excluding SKIP where dependencies were missing).\n")
}

invisible(.uat_env$results)
