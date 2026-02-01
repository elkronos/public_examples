# Brown (double) exponential smoothing with trend (single alpha)
# Refactored for robustness + maintainability (no backward-compat wrapper).
#
# Public API:
#   - brown_es_fit()
#   - brown_es_forecast()  (optional convenience; fit() already returns insample+future)
#   - plot.brown_es()      (S3 plot)
#   - print.brown_es()     (S3 print)

# -----------------------------
# Utilities (internal)
# -----------------------------

.bs_as_numeric <- function(x) {
  a <- as.numeric(x)
  a <- as.vector(a)
  if (length(a) == 0L) stop("Input series is empty.", call. = FALSE)
  a
}

.bs_handle_na <- function(y, na_action = c("fail", "omit")) {
  na_action <- match.arg(na_action)
  if (anyNA(y)) {
    if (na_action == "fail") {
      stop("Series contains NA/NaN. Use na_action='omit' or clean the data.", call. = FALSE)
    }
    y <- y[!is.na(y)]
  }
  if (length(y) == 0L) stop("No valid data points after NA handling.", call. = FALSE)
  y
}

.bs_clamp_alpha <- function(alpha, eps = 1e-6) {
  alpha <- as.numeric(alpha)
  alpha <- pmin(pmax(alpha, eps), 1 - eps)
  alpha
}

.bs_alpha_candidates <- function(alpha,
                                 bounds = c(0.02, 0.98),
                                 grid = 200L,
                                 eps = 1e-6) {
  if (is.null(alpha)) alpha <- "auto"
  
  if (is.character(alpha)) {
    if (length(alpha) != 1L || tolower(alpha) != "auto") {
      stop("alpha as character must be exactly 'auto'.", call. = FALSE)
    }
    if (!is.numeric(bounds) || length(bounds) != 2L || anyNA(bounds)) {
      stop("bounds must be numeric length-2 (low, high).", call. = FALSE)
    }
    grid <- as.integer(grid)
    if (grid < 2L) stop("grid must be >= 2 when alpha='auto'.", call. = FALSE)
    
    lo <- .bs_clamp_alpha(bounds[1], eps = eps)
    hi <- .bs_clamp_alpha(bounds[2], eps = eps)
    if (lo >= hi) stop("bounds must satisfy low < high (after clamping).", call. = FALSE)
    
    return(seq(lo, hi, length.out = grid))
  }
  
  if (is.numeric(alpha)) {
    if (anyNA(alpha)) stop("alpha contains NA/NaN.", call. = FALSE)
    if (length(alpha) == 1L) return(.bs_clamp_alpha(alpha, eps = eps))
    # explicit candidate set
    a <- unique(.bs_clamp_alpha(alpha, eps = eps))
    if (length(a) == 0L) stop("alpha candidates are empty.", call. = FALSE)
    return(a)
  }
  
  stop("alpha must be numeric (scalar or candidate vector) or 'auto'.", call. = FALSE)
}

# Fast OLS for y ~ 1 + x (x = 1..k) without lm()
.bs_ols_line <- function(y, k) {
  k <- as.integer(k)
  if (k < 2L) k <- 2L
  if (k > length(y)) k <- length(y)
  
  x <- seq_len(k)
  yy <- y[seq_len(k)]
  
  mx <- mean(x)
  my <- mean(yy)
  vx <- mean((x - mx)^2)  # population var; cancels consistently in slope ratio
  if (vx == 0) {
    beta1 <- 0
  } else {
    beta1 <- mean((x - mx) * (yy - my)) / vx
  }
  beta0 <- my - beta1 * mx
  c(beta0 = beta0, beta1 = beta1)
}

.bs_init_states <- function(y, alpha, init = c("regression", "first"), k = 10L) {
  init <- match.arg(init)
  
  if (length(y) == 1L) {
    beta0 <- y[1]
    beta1 <- 0
  } else if (init == "first") {
    beta0 <- y[1]
    beta1 <- y[2] - y[1]
  } else {
    coefs <- .bs_ols_line(y, k = k)
    beta0 <- unname(coefs["beta0"])
    beta1 <- unname(coefs["beta1"])
  }
  
  # Brown double smoothing initial state conversion
  S1_0 <- beta0 - ((1 - alpha) / alpha) * beta1
  S2_0 <- beta0 - ((2 * (1 - alpha)) / alpha) * beta1
  c(S1_0 = as.numeric(S1_0), S2_0 = as.numeric(S2_0))
}

.bs_compute_states <- function(y, alpha, S1_0, S2_0) {
  n <- length(y)
  S1 <- numeric(n)
  S2 <- numeric(n)
  a  <- numeric(n)
  b  <- numeric(n)
  
  S1[1] <- S1_0
  S2[1] <- S2_0
  a[1]  <- 2 * S1[1] - S2[1]
  b[1]  <- (alpha / (1 - alpha)) * (S1[1] - S2[1])
  
  if (n >= 2L) {
    # tight loop; keep indexing simple
    one_minus <- 1 - alpha
    ab_factor <- alpha / (1 - alpha)
    
    for (t in 2:n) {
      s1 <- alpha * y[t] + one_minus * S1[t - 1]
      s2 <- alpha * s1     + one_minus * S2[t - 1]
      S1[t] <- s1
      S2[t] <- s2
      a[t]  <- 2 * s1 - s2
      b[t]  <- ab_factor * (s1 - s2)
    }
  }
  
  list(a = a, b = b)
}

.bs_insample_aligned <- function(y, a, b, lead) {
  n <- length(y)
  lead <- as.integer(lead)
  if (lead < 1L) stop("lead must be >= 1.", call. = FALSE)
  
  f <- rep(NA_real_, n)
  se <- rep(NA_real_, n)
  
  if (n <= lead) return(list(forecast = f, se = se))
  
  t <- seq_len(n - lead)
  idx <- t + lead
  fh <- a[t] + b[t] * lead
  
  f[idx] <- fh
  se[idx] <- (y[idx] - fh)^2
  list(forecast = f, se = se)
}

.bs_future_forecast <- function(a_last, b_last, horizon) {
  horizon <- as.integer(horizon)
  if (horizon < 1L) return(numeric(0))
  h <- seq_len(horizon)
  a_last + b_last * h
}

.bs_z <- function(ci_level) {
  stats::qnorm(0.5 + ci_level / 2)
}

.bs_score_for_alpha <- function(y, alpha, lead_eff, init, k, objective) {
  st0 <- .bs_init_states(y, alpha = alpha, init = init, k = k)
  st  <- .bs_compute_states(y, alpha = alpha, S1_0 = st0["S1_0"], S2_0 = st0["S2_0"])
  
  se <- .bs_insample_aligned(y, st$a, st$b, lead = lead_eff)$se
  ok <- !is.na(se)
  if (!any(ok)) return(list(score = Inf, states = NULL))
  
  if (objective == "sse") {
    score <- sum(se[ok])
  } else {
    score <- mean(se[ok])
  }
  list(score = as.numeric(score), states = st)
}

# -----------------------------
# Public API
# -----------------------------

brown_es_fit <- function(series,
                         alpha = "auto",
                         horizon = 1L,
                         lead = 1L,
                         init = c("regression", "first"),
                         init_k = 10L,
                         bounds = c(0.02, 0.98),
                         grid = 200L,
                         objective = c("sse", "mse"),
                         ci_level = 0.90,
                         na_action = c("fail", "omit")) {
  y0 <- .bs_as_numeric(series)
  y  <- .bs_handle_na(y0, na_action = na_action)
  
  n <- length(y)
  horizon <- as.integer(horizon)
  lead <- as.integer(lead)
  if (horizon < 0L) stop("horizon must be >= 0.", call. = FALSE)
  if (lead < 1L) stop("lead must be >= 1.", call. = FALSE)
  
  init <- match.arg(init)
  objective <- match.arg(objective)
  
  # alpha candidates
  alphas <- .bs_alpha_candidates(alpha, bounds = bounds, grid = grid, eps = 1e-6)
  
  # n==1: cannot score; choose deterministic alpha and proceed
  if (n == 1L) {
    chosen <- if (length(alphas) == 1L) alphas[1] else alphas[ceiling(length(alphas) / 2)]
    st0 <- .bs_init_states(y, alpha = chosen, init = "first", k = init_k)
    st  <- .bs_compute_states(y, alpha = chosen, S1_0 = st0["S1_0"], S2_0 = st0["S2_0"])
    ins <- .bs_insample_aligned(y, st$a, st$b, lead = lead)$forecast
    fut <- .bs_future_forecast(st$a[n], st$b[n], horizon = horizon)
    
    res <- list(
      alpha = chosen,
      insample = ins,
      future = fut,
      criterion = NA_real_,
      objective = objective,
      sigma = NA_real_,
      intervals = NULL,
      n = n,
      lead = lead,
      horizon = horizon,
      init = init
    )
    class(res) <- "brown_es"
    return(res)
  }
  
  # lead_eff for scoring only (ensures at least one scored point)
  lead_eff <- min(lead, n - 1L)
  
  best_score <- Inf
  best_alpha <- NA_real_
  best_states <- NULL
  
  # Tune alpha
  for (a in alphas) {
    fit <- .bs_score_for_alpha(
      y, alpha = a, lead_eff = lead_eff,
      init = init, k = init_k,
      objective = objective
    )
    if (is.finite(fit$score) && fit$score < best_score) {
      best_score <- fit$score
      best_alpha <- a
      best_states <- fit$states
    }
  }
  
  if (is.null(best_states) || !is.finite(best_score)) {
    stop("Unable to fit model (no valid in-sample points to score).", call. = FALSE)
  }
  
  # Produce outputs for requested lead (may be all NA when lead >= n)
  ins_obj <- .bs_insample_aligned(y, best_states$a, best_states$b, lead = lead)
  insample <- ins_obj$forecast
  future <- .bs_future_forecast(best_states$a[n], best_states$b[n], horizon = horizon)
  
  # Confidence intervals (constant-width, matches your Python logic)
  sigma <- NA_real_
  intervals <- NULL
  if (!is.null(ci_level)) {
    ci_level <- as.numeric(ci_level)
    if (!is.finite(ci_level) || ci_level <= 0 || ci_level >= 1) {
      stop("ci_level must be in (0, 1), or NULL to disable intervals.", call. = FALSE)
    }
    
    ins_eff <- .bs_insample_aligned(y, best_states$a, best_states$b, lead = lead_eff)$forecast
    ok <- !is.na(ins_eff)
    resid <- y[ok] - ins_eff[ok]
    
    if (length(resid) >= 2L) {
      sigma <- stats::sd(resid)
      z <- .bs_z(ci_level)
      
      intervals <- list()
      
      # insample bands only where insample exists for requested lead
      ok_req <- !is.na(insample)
      if (any(ok_req)) {
        lo <- insample
        hi <- insample
        lo[ok_req] <- insample[ok_req] - z * sigma
        hi[ok_req] <- insample[ok_req] + z * sigma
        intervals$insample <- list(lo = lo, hi = hi)
      }
      
      intervals$future <- list(lo = future - z * sigma, hi = future + z * sigma)
    }
  }
  
  res <- list(
    alpha = as.numeric(best_alpha),
    insample = insample,
    future = future,
    criterion = as.numeric(best_score),
    objective = objective,
    sigma = sigma,
    intervals = intervals,
    n = n,
    lead = lead,
    horizon = horizon,
    init = init
  )
  class(res) <- "brown_es"
  res
}

# Optional helper if you want to forecast from an existing fit (here it just returns what fit already has)
brown_es_forecast <- function(fit) {
  if (!inherits(fit, "brown_es")) stop("fit must be a 'brown_es' object.", call. = FALSE)
  list(insample = fit$insample, future = fit$future, intervals = fit$intervals)
}

# -----------------------------
# S3 methods
# -----------------------------

print.brown_es <- function(x, ...) {
  cat("Brown double exponential smoothing (single alpha)\n")
  cat(sprintf("  alpha: %.6f\n", x$alpha))
  cat(sprintf("  n: %d | lead: %d | horizon: %d\n", x$n, x$lead, x$horizon))
  cat(sprintf("  objective: %s | criterion: %s\n",
              x$objective,
              ifelse(is.na(x$criterion), "NA", format(x$criterion, digits = 6))))
  if (!is.null(x$intervals) && is.finite(x$sigma)) {
    cat(sprintf("  sigma (residual): %.6f\n", x$sigma))
  }
  invisible(x)
}

plot.brown_es <- function(x, y = NULL, main = "Brown exponential smoothing", ...) {
  if (is.null(y)) stop("Provide the original series as 'y' to plot().", call. = FALSE)
  yy <- .bs_as_numeric(y)
  yy <- .bs_handle_na(yy, na_action = "omit") # plotting uses same effective series if user omitted
  
  n <- length(yy)
  tt <- seq_len(n)
  
  graphics::plot(tt, yy, type = "o", pch = 16, xlab = "Time", ylab = "Value", main = main, ...)
  graphics::lines(tt, x$insample, lty = 2)
  
  # insample CI
  if (!is.null(x$intervals) && !is.null(x$intervals$insample)) {
    lo <- x$intervals$insample$lo
    hi <- x$intervals$insample$hi
    ok <- !is.na(x$insample)
    if (any(ok)) {
      t2 <- tt[ok]
      graphics::polygon(
        c(t2, rev(t2)),
        c(lo[ok], rev(hi[ok])),
        col = grDevices::adjustcolor("grey60", alpha.f = 0.25),
        border = NA
      )
      graphics::lines(tt, x$insample, lty = 2)
      graphics::points(tt, yy, pch = 16)
    }
  }
  
  # future + CI
  if (length(x$future) > 0L) {
    tf <- (n + 1):(n + length(x$future))
    graphics::lines(tf, x$future, lty = 3, lwd = 2)
    
    if (!is.null(x$intervals) && !is.null(x$intervals$future)) {
      loF <- x$intervals$future$lo
      hiF <- x$intervals$future$hi
      graphics::polygon(
        c(tf, rev(tf)),
        c(loF, rev(hiF)),
        col = grDevices::adjustcolor("grey60", alpha.f = 0.25),
        border = NA
      )
      graphics::lines(tf, x$future, lty = 3, lwd = 2)
    }
  }
  
  graphics::legend(
    "topleft",
    legend = c("Actual", "In-sample", "Future"),
    lty = c(1, 2, 3),
    pch = c(16, NA, NA),
    bty = "n"
  )
  
  invisible(NULL)
}

# -----------------------------
# Example
# -----------------------------
# Pt <- c(12, 15, 14, 16, 19, 20, 22, 25, 24, 23)
# fit <- brown_es_fit(Pt, alpha = "auto", horizon = 3, lead = 2, init = "regression", ci_level = 0.90)
# print(fit)
# plot(fit, y = Pt)


# ============================
# UAT / Smoke + Behavior Tests
# for brown_es_fit() module
# ============================
# Assumes the following are already loaded in your R session:
#   - brown_es_fit()
#   - brown_es_forecast()
#   - S3 methods: print.brown_es, plot.brown_es
#
# Run:
#   results <- run_all_tests()
#   results$summary
#   results$details

# ----------------------------
# Minimal test framework
# ----------------------------

assert_true <- function(cond, msg = "Assertion failed") {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

assert_equal <- function(a, b, msg = NULL) {
  if (!identical(a, b)) {
    if (is.null(msg)) msg <- sprintf("Not equal: %s vs %s", deparse(substitute(a)), deparse(substitute(b)))
    stop(msg, call. = FALSE)
  }
}

assert_near <- function(a, b, tol = 1e-10, msg = "Not close") {
  if (length(a) != 1L || length(b) != 1L) stop("assert_near expects scalar values", call. = FALSE)
  if (!is.finite(a) || !is.finite(b) || abs(a - b) > tol) {
    stop(sprintf("%s: %0.17g vs %0.17g (tol=%g)", msg, a, b, tol), call. = FALSE)
  }
}

assert_all_finite <- function(x, msg = "Contains non-finite values") {
  x <- as.numeric(x)
  if (!all(is.finite(x))) stop(msg, call. = FALSE)
}

assert_shape <- function(x, n, msg = "Wrong length") {
  if (length(x) != n) stop(sprintf("%s: expected %d, got %d", msg, n, length(x)), call. = FALSE)
}

expect_error <- function(expr, pattern = NULL) {
  ok <- FALSE
  err <- NULL
  tryCatch(
    { force(expr); },
    error = function(e) { ok <<- TRUE; err <<- e$message }
  )
  if (!ok) stop("Expected an error, but none occurred.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, err, fixed = FALSE)) {
    stop(sprintf("Error message did not match pattern.\nPattern: %s\nMessage: %s", pattern, err),
         call. = FALSE)
  }
  TRUE
}

run_test <- function(name, fn) {
  started <- Sys.time()
  out <- tryCatch(
    { fn(); list(pass = TRUE, error = NULL) },
    error = function(e) list(pass = FALSE, error = e$message)
  )
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (out$pass) {
    cat(sprintf("PASS: %s (%.3fs)\n", name, elapsed))
  } else {
    cat(sprintf("FAIL: %s (%.3fs)\n  -> %s\n", name, elapsed, out$error))
  }
  c(list(name = name, elapsed_s = elapsed), out)
}

# ----------------------------
# Test data helpers
# ----------------------------

toy_series <- function() c(12, 15, 14, 16, 19, 20, 22, 25, 24, 23)

trend_series <- function(n = 40, slope = 3, intercept = 5) {
  intercept + slope * (0:(n - 1))
}

noisy_trend <- function(n = 80, slope = 0.5, intercept = 20, noise = 2, seed = 42) {
  set.seed(seed)
  intercept + slope * (0:(n - 1)) + rnorm(n, mean = 0, sd = noise)
}

series_with_nas <- function() c(1, 2, NA, 4, 5)

# ----------------------------
# Sanity checks on loaded funcs
# ----------------------------

test_exports_exist <- function() {
  assert_true(exists("brown_es_fit", mode = "function"), "brown_es_fit() is not loaded.")
  assert_true(exists("brown_es_forecast", mode = "function"), "brown_es_forecast() is not loaded.")
  # S3 methods existence is optional; but if present should be callable by generic plot/print
}

# ----------------------------
# UAT tests
# ----------------------------

test_basic_auto_runs_and_shapes <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = "auto", horizon = 3, lead = 2, ci_level = 0.90)
  assert_true(inherits(fit, "brown_es"), "fit should have class 'brown_es'")
  assert_true(is.finite(fit$alpha) && fit$alpha > 0 && fit$alpha < 1, "alpha should be in (0,1)")
  assert_shape(fit$insample, length(y), "insample length mismatch")
  assert_shape(fit$future, 3, "future length mismatch")
  # criterion should be finite because n>1 and we can score with lead_eff
  assert_true(is.finite(fit$criterion), "criterion should be finite for n>1")
  assert_all_finite(fit$future, "future should be finite")
}

test_alpha_fixed_runs_exact <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = 0.35, horizon = 4, lead = 1, init = "first", ci_level = NULL)
  assert_near(fit$alpha, 0.35, tol = 1e-12, msg = "fixed alpha should match exactly")
  assert_shape(fit$future, 4, "future horizon mismatch")
  assert_true(is.null(fit$intervals), "intervals should be NULL when ci_level=NULL")
}

test_alpha_candidate_vector_selects_one <- function() {
  y <- toy_series()
  candidates <- c(0.2, 0.4, 0.6)
  fit <- brown_es_fit(y, alpha = candidates, horizon = 2, lead = 1, ci_level = NULL)
  assert_true(fit$alpha %in% candidates, "alpha should be one of provided candidates")
}

test_objective_mse_runs <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = "auto", objective = "mse", horizon = 2, lead = 1, ci_level = NULL)
  assert_true(is.finite(fit$criterion), "criterion should be finite for mse")
}

test_lead_too_large_insample_all_na_future_ok <- function() {
  y <- toy_series()
  lead <- length(y) + 5
  fit <- brown_es_fit(y, alpha = 0.4, horizon = 3, lead = lead, ci_level = NULL)
  assert_true(all(is.na(fit$insample)), "insample should be all NA when lead >= n")
  assert_shape(fit$future, 3, "future length mismatch")
  assert_all_finite(fit$future, "future should be finite")
  # criterion still finite because scoring uses lead_eff
  assert_true(is.finite(fit$criterion), "criterion should still be finite via lead_eff scoring")
}

test_intervals_present_when_residuals_exist <- function() {
  y <- noisy_trend(n = 80, noise = 2, seed = 123)
  fit <- brown_es_fit(y, alpha = "auto", horizon = 3, lead = 1, ci_level = 0.90)
  # intervals may be NULL only if residuals < 2 points (shouldn't happen here)
  assert_true(!is.null(fit$intervals), "intervals should be present for typical data")
  assert_true(!is.null(fit$intervals$future), "future intervals missing")
  assert_shape(fit$intervals$future$lo, 3, "future interval lo length mismatch")
  assert_shape(fit$intervals$future$hi, 3, "future interval hi length mismatch")
  assert_true(all(fit$intervals$future$hi - fit$intervals$future$lo >= 0), "future CI width must be non-negative")
  
  # insample CI exists only where insample exists for requested lead
  if (!is.null(fit$intervals$insample)) {
    assert_shape(fit$intervals$insample$lo, length(y), "insample interval lo length mismatch")
    assert_shape(fit$intervals$insample$hi, length(y), "insample interval hi length mismatch")
  }
}

test_ci_disabled <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = "auto", horizon = 2, lead = 1, ci_level = NULL)
  assert_true(is.null(fit$intervals), "intervals should be NULL when ci_level=NULL")
}

test_na_action_fail_and_omit <- function() {
  y <- series_with_nas()
  expect_error(brown_es_fit(y, alpha = 0.5, horizon = 2, lead = 1, na_action = "fail", ci_level = NULL),
               pattern = "contains NA|NA/NaN")
  
  fit <- brown_es_fit(y, alpha = 0.5, horizon = 2, lead = 1, na_action = "omit", ci_level = NULL)
  # After omit, series length should be 4
  assert_equal(fit$n, 4L, "n should reflect post-omit length")
  assert_shape(fit$insample, 4, "insample length should match post-omit series length")
  assert_shape(fit$future, 2, "future length mismatch")
  assert_all_finite(fit$future, "future should be finite")
}

test_invalid_parameters_raise <- function() {
  y <- toy_series()
  
  expect_error(brown_es_fit(y, alpha = "bogus", horizon = 2, lead = 1), pattern = "auto")
  expect_error(brown_es_fit(y, alpha = "auto", horizon = 2, lead = 0), pattern = "lead must be")
  expect_error(brown_es_fit(y, alpha = "auto", horizon = -1, lead = 1), pattern = "horizon must be")
  expect_error(brown_es_fit(y, alpha = "auto", bounds = c(0.9, 0.1)), pattern = "bounds must satisfy")
  expect_error(brown_es_fit(y, alpha = "auto", grid = 1), pattern = "grid must be")
  expect_error(brown_es_fit(y, alpha = c(NA, 0.5)), pattern = "alpha contains NA")
  expect_error(brown_es_fit(numeric(0), alpha = "auto"), pattern = "empty")
  expect_error(brown_es_fit(y, alpha = 0.5, ci_level = 1.0), pattern = "ci_level")
  expect_error(brown_es_fit(y, alpha = 0.5, ci_level = 0.0), pattern = "ci_level")
}

test_alpha_clamping_applies <- function() {
  y <- toy_series()
  fit0 <- brown_es_fit(y, alpha = 0, horizon = 1, lead = 1, ci_level = NULL)
  fit1 <- brown_es_fit(y, alpha = 1, horizon = 1, lead = 1, ci_level = NULL)
  assert_true(fit0$alpha > 0 && fit0$alpha < 1, "alpha=0 should be clamped into (0,1)")
  assert_true(fit1$alpha > 0 && fit1$alpha < 1, "alpha=1 should be clamped into (0,1)")
}

test_n_equals_1_behavior <- function() {
  y <- c(10)
  fit <- brown_es_fit(y, alpha = "auto", horizon = 3, lead = 1, ci_level = 0.9)
  assert_true(is.finite(fit$alpha), "alpha should still be finite for n=1")
  assert_shape(fit$insample, 1, "insample length mismatch for n=1")
  assert_shape(fit$future, 3, "future length mismatch for n=1")
  assert_true(is.na(fit$criterion), "criterion should be NA for n=1")
}

test_horizon_zero <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = "auto", horizon = 0, lead = 1, ci_level = NULL)
  assert_shape(fit$future, 0, "future should be length 0 when horizon=0")
}

test_reasonable_on_perfect_trend <- function() {
  y <- trend_series(n = 50, slope = 3, intercept = 5)
  fit <- brown_es_fit(y, alpha = "auto", horizon = 5, lead = 1, ci_level = NULL)
  
  # Expected continuation: last value + slope*h
  last <- y[length(y)]
  expected <- last + 3 * (1:5)
  
  # Brown ES should track a perfect linear trend well; allow small tolerance
  max_abs_err <- max(abs(fit$future - expected))
  assert_true(max_abs_err < 2.0,
              sprintf("Forecast deviates too much from perfect trend continuation (max_abs_err=%g)", max_abs_err))
}

test_brown_es_forecast_helper <- function() {
  y <- toy_series()
  fit <- brown_es_fit(y, alpha = 0.4, horizon = 2, lead = 1, ci_level = NULL)
  out <- brown_es_forecast(fit)
  assert_true(is.list(out) && all(c("insample", "future", "intervals") %in% names(out)),
              "brown_es_forecast() output structure unexpected")
  assert_shape(out$future, 2, "forecast helper future length mismatch")
}

# ----------------------------
# Runner
# ----------------------------

run_all_tests <- function() {
  tests <- list(
    "exports exist"                               = test_exports_exist,
    "basic auto runs + shapes"                    = test_basic_auto_runs_and_shapes,
    "fixed alpha runs exact"                      = test_alpha_fixed_runs_exact,
    "candidate alpha selects one"                 = test_alpha_candidate_vector_selects_one,
    "objective mse runs"                          = test_objective_mse_runs,
    "lead too large: insample all NA; future ok"  = test_lead_too_large_insample_all_na_future_ok,
    "intervals present when residuals exist"      = test_intervals_present_when_residuals_exist,
    "CI disabled"                                 = test_ci_disabled,
    "NA action fail/omit"                         = test_na_action_fail_and_omit,
    "invalid parameters raise"                    = test_invalid_parameters_raise,
    "alpha clamping applies"                      = test_alpha_clamping_applies,
    "n==1 behavior"                               = test_n_equals_1_behavior,
    "horizon zero"                                = test_horizon_zero,
    "reasonable on perfect trend"                 = test_reasonable_on_perfect_trend,
    "forecast helper"                             = test_brown_es_forecast_helper
  )
  
  details <- lapply(names(tests), function(nm) run_test(nm, tests[[nm]]))
  names(details) <- names(tests)
  
  pass <- vapply(details, function(x) isTRUE(x$pass), logical(1))
  summary <- list(
    passed = sum(pass),
    failed = sum(!pass),
    total = length(pass)
  )
  
  cat("\n============================\n")
  cat(sprintf("UAT Summary: %d/%d passed (%d failed)\n",
              summary$passed, summary$total, summary$failed))
  cat("============================\n")
  
  list(summary = summary, details = details)
}

# ---- Run now (uncomment) ----
# results <- run_all_tests()
# results$summary
