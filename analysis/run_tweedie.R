# -----------------------------
# Tweedie GLM Workflow (modular functions + UAT)
# -----------------------------
# Packages:
# - statmod: provides statmod::tweedie() GLM family
# - tweedie: provides tweedie::rtweedie() and tweedie::AICtweedie()
# - glm2: optional GLM fitting engine (falls back to stats::glm)
# - cvTools: optional fold utilities (this script uses its own fold ids)
# - boot: bootstrap resampling
# - car: diagnostics plots

suppressPackageStartupMessages({
  library(statmod)
  library(tweedie)
  library(glm2)
  library(boot)
  library(car)
})

# -----------------------------
# Utilities: validation, warnings capture, and safety helpers
# -----------------------------

capture_warnings <- function(expr) {
  w <- character(0)
  val <- withCallingHandlers(
    expr,
    warning = function(e) {
      w <<- c(w, conditionMessage(e))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = w)
}

positive_eps <- function(y) {
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  if (length(y) == 0) return(1e-6)
  if (any(y > 0)) return(0.1 * mean(y[y > 0]))
  1e-6
}

is_valid_mu <- function(mu, family) {
  all(is.finite(mu)) && all(family$validmu(mu))
}

# -----------------------------
# Data simulation (useful for testing and examples)
# -----------------------------

simulate_tweedie_data <- function(n = 200,
                                  seed = 123,
                                  beta = c(`(Intercept)` = 1, x1 = 0.5, x2 = -0.3),
                                  phi = 2,
                                  power = 1.5) {
  set.seed(seed)
  x1 <- runif(n, 0, 10)
  x2 <- rnorm(n, 5, 2)
  
  eta <- beta["(Intercept)"] + beta["x1"] * x1 + beta["x2"] * x2
  mu <- exp(eta)
  
  y <- tweedie::rtweedie(n, mu = mu, phi = phi, power = power)
  data.frame(y = y, x1 = x1, x2 = x2)
}

# -----------------------------
# Starting values: construction that yields valid means
# -----------------------------

make_safe_start <- function(formula, data, family) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  y <- model.response(mf)
  X <- model.matrix(formula, data = mf)
  
  eps <- positive_eps(y)
  mu0 <- mean(pmax(y, eps))
  
  beta <- rep(0, ncol(X))
  names(beta) <- colnames(X)
  
  if ("(Intercept)" %in% names(beta)) {
    beta["(Intercept)"] <- family$linkfun(mu0)
  }
  
  beta
}

shrink_slopes_until_valid <- function(beta, X, family, max_iter = 30) {
  if (!length(beta)) return(beta)
  
  has_int <- "(Intercept)" %in% names(beta)
  slopes <- setdiff(names(beta), if (has_int) "(Intercept)" else character(0))
  if (!length(slopes)) return(beta)
  
  for (i in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    mu <- family$linkinv(eta)
    if (is_valid_mu(mu, family)) return(beta)
    beta[slopes] <- 0.5 * beta[slopes]
  }
  
  beta[slopes] <- 0
  beta
}

make_start <- function(formula, data, family,
                       link_power,
                       strategy = c("safe", "link_lm")) {
  strategy <- match.arg(strategy)
  
  mf <- model.frame(formula, data = data, na.action = na.omit)
  X <- model.matrix(formula, data = mf)
  y <- model.response(mf)
  
  if (strategy == "safe") {
    return(make_safe_start(formula, mf, family))
  }
  
  # Link-consistent linear-model approximation for common link powers, then validation on mu
  eps <- positive_eps(y)
  y_pos <- pmax(y, eps)
  
  # Build a temporary data frame for LM fitting without altering original columns
  tmp <- mf
  tmp$.y_work <- y_pos
  
  beta_hat <- NULL
  
  if (isTRUE(all.equal(link_power, 0))) {
    beta_hat <- coef(lm(log(.y_work) ~ x1 + x2, data = tmp))
  } else if (isTRUE(all.equal(link_power, 1))) {
    beta_hat <- coef(lm(.y_work ~ x1 + x2, data = tmp))
  } else if (isTRUE(all.equal(link_power, 0.5))) {
    beta_hat <- coef(lm(sqrt(.y_work) ~ x1 + x2, data = tmp))
  } else if (isTRUE(all.equal(link_power, -1))) {
    beta_hat <- coef(lm(I(1 / .y_work) ~ x1 + x2, data = tmp))
  } else if (isTRUE(all.equal(link_power, -0.5))) {
    beta_hat <- coef(lm(I(1 / sqrt(.y_work)) ~ x1 + x2, data = tmp))
  } else {
    beta_hat <- make_safe_start(formula, mf, family)
  }
  
  beta <- rep(0, ncol(X))
  names(beta) <- colnames(X)
  beta[names(beta_hat)] <- beta_hat
  
  eta <- as.numeric(X %*% beta)
  mu <- family$linkinv(eta)
  if (!is_valid_mu(mu, family)) {
    beta <- shrink_slopes_until_valid(beta, X, family)
  }
  
  beta
}

# -----------------------------
# Fitting: engine wrapper with warning capture and fallback
# -----------------------------

fit_tweedie <- function(formula, data,
                        var_power,
                        link_power,
                        engine = c("glm2", "glm"),
                        start_strategy = c("safe", "link_lm"),
                        maxit = 100) {
  engine <- match.arg(engine)
  start_strategy <- match.arg(start_strategy)
  
  mf <- model.frame(formula, data = data, na.action = na.omit)
  family <- statmod::tweedie(var.power = var_power, link.power = link_power)
  start <- make_start(formula, mf, family, link_power = link_power, strategy = start_strategy)
  
  fit_one <- function(which_engine) {
    capture_warnings({
      if (which_engine == "glm2") {
        glm2::glm2(
          formula,
          data = mf,
          family = family,
          start = start,
          control = glm.control(maxit = maxit)
        )
      } else {
        stats::glm(
          formula,
          data = mf,
          family = family,
          start = start,
          control = glm.control(maxit = maxit)
        )
      }
    })
  }
  
  out <- tryCatch(fit_one(engine), error = function(e) list(value = NULL, warnings = character(0), error = e))
  
  if (!is.null(out$value)) {
    return(list(
      fit = out$value,
      engine = engine,
      family = family,
      start = start,
      warnings = out$warnings
    ))
  }
  
  alt <- if (engine == "glm2") "glm" else "glm2"
  out2 <- tryCatch(fit_one(alt), error = function(e) list(value = NULL, warnings = character(0), error = e))
  
  if (is.null(out2$value)) {
    msg1 <- if (!is.null(out$error)) conditionMessage(out$error) else "fit failed"
    msg2 <- if (!is.null(out2$error)) conditionMessage(out2$error) else "fallback fit failed"
    stop(sprintf("Model fit failed. Primary engine: %s (%s). Fallback engine: %s (%s).",
                 engine, msg1, alt, msg2), call. = FALSE)
  }
  
  list(
    fit = out2$value,
    engine = alt,
    family = family,
    start = start,
    warnings = out2$warnings
  )
}

# -----------------------------
# Scoring: Tweedie-appropriate AIC and a deviance alternative
# -----------------------------

score_tweedie_fit <- function(fit, metric = c("aic", "deviance")) {
  metric <- match.arg(metric)
  
  if (metric == "deviance") {
    dv <- tryCatch(stats::deviance(fit), error = function(e) NA_real_)
    return(if (is.finite(dv)) dv else Inf)
  }
  
  aic <- suppressWarnings(
    tryCatch(tweedie::AICtweedie(fit), error = function(e) NA_real_)
  )
  
  if (length(aic) != 1 || !is.finite(aic)) Inf else as.numeric(aic)
}

# -----------------------------
# Tuning: grid search with failure-aware bookkeeping
# -----------------------------

make_tweedie_grid <- function(var_power = seq(1.05, 1.95, by = 0.05),
                              link_power = c(0, 1, 0.5, -1, -0.5)) {
  expand.grid(var.power = var_power, link.power = link_power, KEEP.OUT.ATTRS = FALSE)
}

tune_tweedie <- function(formula, data,
                         grid = make_tweedie_grid(),
                         engine = c("glm2", "glm"),
                         start_strategy = c("safe", "link_lm"),
                         maxit = 100,
                         metric = c("aic", "deviance")) {
  engine <- match.arg(engine)
  start_strategy <- match.arg(start_strategy)
  metric <- match.arg(metric)
  
  results <- grid
  results$score <- Inf
  results$fit_ok <- FALSE
  results$converged <- FALSE
  results$engine_used <- NA_character_
  results$warning_count <- NA_integer_
  results$error_msg <- NA_character_
  
  for (i in seq_len(nrow(grid))) {
    vp <- grid$var.power[i]
    lp <- grid$link.power[i]
    
    attempt <- tryCatch({
      fit_obj <- fit_tweedie(
        formula, data,
        var_power = vp,
        link_power = lp,
        engine = engine,
        start_strategy = start_strategy,
        maxit = maxit
      )
      
      f <- fit_obj$fit
      sc <- score_tweedie_fit(f, metric = metric)
      
      list(
        score = sc,
        fit_ok = TRUE,
        converged = isTRUE(f$converged),
        engine_used = fit_obj$engine,
        warning_count = length(fit_obj$warnings),
        error_msg = NA_character_
      )
    }, error = function(e) {
      list(
        score = Inf,
        fit_ok = FALSE,
        converged = FALSE,
        engine_used = NA_character_,
        warning_count = NA_integer_,
        error_msg = conditionMessage(e)
      )
    })
    
    results$score[i] <- attempt$score
    results$fit_ok[i] <- attempt$fit_ok
    results$converged[i] <- attempt$converged
    results$engine_used[i] <- attempt$engine_used
    results$warning_count[i] <- attempt$warning_count
    results$error_msg[i] <- attempt$error_msg
  }
  
  best_i <- which.min(results$score)
  best <- results[best_i, , drop = FALSE]
  
  list(best = best, results = results, metric = metric)
}

# -----------------------------
# Cross-validation: optional tuning inside folds
# -----------------------------

make_kfold_ids <- function(n, K = 5, seed = 202) {
  set.seed(seed)
  sample(rep(seq_len(K), length.out = n))
}

cross_validate_tweedie <- function(formula, data,
                                   K = 5,
                                   seed = 202,
                                   grid = make_tweedie_grid(),
                                   tune_each_fold = TRUE,
                                   engine = c("glm2", "glm"),
                                   start_strategy = c("safe", "link_lm"),
                                   maxit = 100,
                                   metric = c("aic", "deviance")) {
  engine <- match.arg(engine)
  start_strategy <- match.arg(start_strategy)
  metric <- match.arg(metric)
  
  mf_all <- model.frame(formula, data = data, na.action = na.omit)
  n <- nrow(mf_all)
  fold_id <- make_kfold_ids(n, K = K, seed = seed)
  
  if (!tune_each_fold) {
    tuned0 <- tune_tweedie(
      formula, mf_all,
      grid = grid,
      engine = engine,
      start_strategy = start_strategy,
      maxit = maxit,
      metric = metric
    )
    vp0 <- tuned0$best$var.power
    lp0 <- tuned0$best$link.power
  }
  
  mse_by_fold <- rep(NA_real_, K)
  
  for (k in seq_len(K)) {
    test_idx <- which(fold_id == k)
    train_idx <- setdiff(seq_len(n), test_idx)
    
    train <- mf_all[train_idx, , drop = FALSE]
    test <- mf_all[test_idx, , drop = FALSE]
    
    if (tune_each_fold) {
      tuned <- tune_tweedie(
        formula, train,
        grid = grid,
        engine = engine,
        start_strategy = start_strategy,
        maxit = maxit,
        metric = metric
      )
      vp <- tuned$best$var.power
      lp <- tuned$best$link.power
    } else {
      vp <- vp0
      lp <- lp0
    }
    
    fit_obj <- fit_tweedie(
      formula, train,
      var_power = vp,
      link_power = lp,
      engine = engine,
      start_strategy = start_strategy,
      maxit = maxit
    )
    
    preds <- predict(fit_obj$fit, newdata = test, type = "response")
    y_true <- model.response(model.frame(formula, data = test))
    mse_by_fold[k] <- mean((preds - y_true)^2)
  }
  
  list(mse_by_fold = mse_by_fold, mse = mean(mse_by_fold))
}

# -----------------------------
# Final model: tune then fit on all data
# -----------------------------

fit_final_tweedie <- function(formula, data,
                              grid = make_tweedie_grid(),
                              engine = c("glm2", "glm"),
                              start_strategy = c("safe", "link_lm"),
                              maxit = 100,
                              metric = c("aic", "deviance")) {
  engine <- match.arg(engine)
  start_strategy <- match.arg(start_strategy)
  metric <- match.arg(metric)
  
  tuned <- tune_tweedie(
    formula, data,
    grid = grid,
    engine = engine,
    start_strategy = start_strategy,
    maxit = maxit,
    metric = metric
  )
  
  if (!is.finite(tuned$best$score)) {
    stop("Tuning did not produce a finite score for any grid candidate.", call. = FALSE)
  }
  
  vp <- tuned$best$var.power
  lp <- tuned$best$link.power
  
  fit_obj <- fit_tweedie(
    formula, data,
    var_power = vp,
    link_power = lp,
    engine = engine,
    start_strategy = start_strategy,
    maxit = maxit
  )
  
  list(
    fit = fit_obj$fit,
    tuned = tuned,
    engine_used = fit_obj$engine,
    var_power = vp,
    link_power = lp,
    metric = tuned$metric,
    best_score = tuned$best$score,
    warnings = fit_obj$warnings
  )
}

# -----------------------------
# Bootstrap: coefficient resampling with alignment to model matrix columns
# -----------------------------

bootstrap_tweedie_coef <- function(formula, data,
                                   var_power,
                                   link_power,
                                   R = 1000,
                                   seed = 777,
                                   engine = c("glm2", "glm"),
                                   start_strategy = c("safe", "link_lm"),
                                   maxit = 100) {
  engine <- match.arg(engine)
  start_strategy <- match.arg(start_strategy)
  
  set.seed(seed)
  
  mf <- model.frame(formula, data = data, na.action = na.omit)
  X <- model.matrix(formula, data = mf)
  coef_names <- colnames(X)
  
  family <- statmod::tweedie(var.power = var_power, link.power = link_power)
  
  stat_fun <- function(d, idx) {
    dd <- d[idx, , drop = FALSE]
    start <- make_start(formula, dd, family, link_power = link_power, strategy = start_strategy)
    
    fit <- tryCatch({
      if (engine == "glm2") {
        glm2::glm2(
          formula, data = dd, family = family,
          start = start,
          control = glm.control(maxit = maxit)
        )
      } else {
        stats::glm(
          formula, data = dd, family = family,
          start = start,
          control = glm.control(maxit = maxit)
        )
      }
    }, error = function(e) NULL)
    
    if (is.null(fit)) {
      out <- rep(NA_real_, length(coef_names))
      names(out) <- coef_names
      return(out)
    }
    
    b <- coef(fit)
    out <- rep(NA_real_, length(coef_names))
    names(out) <- coef_names
    out[names(b)] <- b
    out
  }
  
  boot::boot(data = mf, statistic = stat_fun, R = R)
}

# -----------------------------
# Diagnostics: quick residual and QQ plots
# -----------------------------

plot_tweedie_diagnostics <- function(fit) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  dev_resid <- residuals(fit, type = "deviance")
  fitted_vals <- fitted(fit)
  
  par(mfrow = c(1, 2))
  plot(fitted_vals, dev_resid,
       main = "Residuals vs Fitted",
       xlab = "Fitted values",
       ylab = "Deviance residuals")
  abline(h = 0, col = "red")
  
  hist(dev_resid, breaks = 30, main = "Histogram of Deviance Residuals", col = "skyblue")
  car::qqPlot(dev_resid, main = "QQ Plot of Deviance Residuals")
}

# -----------------------------
# UAT: end-to-end test suite for interactive use in one R session
# -----------------------------

run_tweedie_uat <- function(profile = c("full", "quick"),
                            seed = 20260201,
                            verbose = TRUE) {
  profile <- match.arg(profile)
  
  uat_results <- list()
  uat_add <- function(name, ok, detail = NULL) {
    uat_results[[length(uat_results) + 1]] <<- list(test = name, ok = isTRUE(ok), detail = detail)
    if (verbose) {
      message(sprintf("%s: %s", if (ok) "PASS" else "FAIL", name))
      if (!ok && !is.null(detail)) message(sprintf("      %s", detail))
    }
  }
  uat_expect_no_error <- function(expr) {
    tryCatch({ list(ok = TRUE, value = force(expr), err = NULL) },
             error = function(e) list(ok = FALSE, value = NULL, err = conditionMessage(e)))
  }
  uat_assert <- function(cond, msg) {
    if (!isTRUE(cond)) stop(msg, call. = FALSE)
    TRUE
  }
  
  required_pkgs <- c("statmod", "tweedie", "glm2", "boot", "car")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  uat_add("Required packages are installed",
          length(missing_pkgs) == 0,
          if (length(missing_pkgs)) paste("Missing:", paste(missing_pkgs, collapse = ", ")) else NULL)
  if (length(missing_pkgs)) stop("Install the missing packages, then rerun UAT.", call. = FALSE)
  
  required_fns <- c(
    "simulate_tweedie_data", "make_tweedie_grid", "tune_tweedie", "fit_tweedie",
    "fit_final_tweedie", "cross_validate_tweedie", "bootstrap_tweedie_coef",
    "plot_tweedie_diagnostics", "make_start", "score_tweedie_fit"
  )
  missing_fns <- required_fns[!vapply(required_fns, exists, logical(1), mode = "function")]
  uat_add("Workflow functions are loaded in the session",
          length(missing_fns) == 0,
          if (length(missing_fns)) paste("Missing:", paste(missing_fns, collapse = ", ")) else NULL)
  if (length(missing_fns)) stop("Load the workflow functions, then rerun UAT.", call. = FALSE)
  
  set.seed(seed)
  
  if (profile == "quick") {
    n <- 200
    grid <- make_tweedie_grid(var_power = seq(1.2, 1.8, by = 0.2), link_power = c(0, 1, 0.5))
    K <- 3
    boot_R <- 100
    tune_each_fold <- FALSE
    metric <- "deviance"
  } else {
    n <- 400
    grid <- make_tweedie_grid(var_power = seq(1.05, 1.95, by = 0.1), link_power = c(0, 1, 0.5, -1, -0.5))
    K <- 5
    boot_R <- 300
    tune_each_fold <- TRUE
    metric <- "aic"
  }
  
  formula <- y ~ x1 + x2
  
  sim <- uat_expect_no_error({
    dat <- simulate_tweedie_data(n = n, seed = seed)
    uat_assert(is.data.frame(dat), "simulate_tweedie_data() did not return a data.frame.")
    uat_assert(all(c("y", "x1", "x2") %in% names(dat)), "Simulated data missing required columns.")
    uat_assert(nrow(dat) == n, "Simulated data row count does not match n.")
    uat_assert(all(is.finite(dat$y)) && all(dat$y >= 0), "y contains invalid values.")
    dat
  })
  uat_add("Simulation produces expected structure and values", sim$ok, sim$err)
  if (!sim$ok) stop("UAT aborted at simulation stage.", call. = FALSE)
  dat <- sim$value
  
  start_check <- uat_expect_no_error({
    for (lp in unique(grid$link_power)) {
      fam <- statmod::tweedie(var.power = 1.5, link.power = lp)
      st <- make_start(formula, dat, fam, link_power = lp, strategy = "safe")
      X <- model.matrix(formula, data = dat)
      mu <- fam$linkinv(as.numeric(X %*% st))
      uat_assert(all(is.finite(st)), sprintf("Non-finite start coefficients for link.power=%s", lp))
      uat_assert(all(is.finite(mu)) && all(fam$validmu(mu)), sprintf("Invalid mean from start for link.power=%s", lp))
    }
    TRUE
  })
  uat_add("Starting values yield valid means across link powers (safe strategy)", start_check$ok, start_check$err)
  if (!start_check$ok) stop("UAT aborted at starting-values stage.", call. = FALSE)
  
  tune <- uat_expect_no_error({
    tuned <- tune_tweedie(
      formula, dat,
      grid = grid,
      engine = "glm2",
      start_strategy = "safe",
      maxit = 100,
      metric = metric
    )
    uat_assert(is.finite(tuned$best$score), "Best score is not finite; all candidates failed.")
    tuned
  })
  uat_add("Tuning finds at least one finite-score candidate", tune$ok, tune$err)
  if (!tune$ok) {
    # Provide a compact error summary for grid candidates that did not fit
    tuned_try <- tune_tweedie(formula, dat, grid = grid, engine = "glm2", start_strategy = "safe", maxit = 100, metric = "deviance")
    res <- tuned_try$results
    if (verbose) {
      message("------ Tuning diagnostics (deviance metric) ------")
      message(sprintf("Fit OK count: %d / %d", sum(res$fit_ok), nrow(res)))
      if (any(!res$fit_ok)) {
        top_err <- sort(table(res$error_msg[!res$fit_ok]), decreasing = TRUE)
        message("Top error messages:")
        print(utils::head(top_err, 5))
      }
    }
    stop("UAT aborted at tuning stage.", call. = FALSE)
  }
  tuned <- tune$value
  
  final_fit <- uat_expect_no_error({
    final <- fit_final_tweedie(
      formula, dat,
      grid = grid,
      engine = "glm2",
      start_strategy = "safe",
      maxit = 100,
      metric = metric
    )
    fit <- final$fit
    fv <- fitted(fit)
    uat_assert(all(is.finite(fv)) && all(fv >= 0), "Fitted means contain invalid values.")
    pr <- predict(fit, newdata = dat, type = "response")
    uat_assert(all(is.finite(pr)) && all(pr >= 0), "Predictions contain invalid values.")
    final
  })
  uat_add("Final model fit and prediction behave as expected", final_fit$ok, final_fit$err)
  if (!final_fit$ok) stop("UAT aborted at final fit stage.", call. = FALSE)
  final <- final_fit$value
  
  cv_run <- uat_expect_no_error({
    cv <- cross_validate_tweedie(
      formula, dat,
      K = K,
      seed = seed,
      grid = grid,
      tune_each_fold = tune_each_fold,
      engine = "glm2",
      start_strategy = "safe",
      maxit = 100,
      metric = metric
    )
    uat_assert(is.finite(cv$mse) && cv$mse >= 0, "CV MSE is invalid.")
    cv
  })
  uat_add("Cross-validation returns finite MSE values", cv_run$ok, cv_run$err)
  
  boot_run <- uat_expect_no_error({
    boot_obj <- bootstrap_tweedie_coef(
      formula, dat,
      var_power = final$var_power,
      link_power = final$link_power,
      R = boot_R,
      seed = seed,
      engine = "glm2",
      start_strategy = "safe",
      maxit = 100
    )
    na_rate <- mean(!is.finite(boot_obj$t))
    uat_assert(na_rate < 0.25, sprintf("Bootstrap non-finite coefficient rate=%.2f", na_rate))
    boot_obj
  })
  uat_add("Bootstrap produces a valid boot object with acceptable NA rate", boot_run$ok, boot_run$err)
  
  diag_run <- uat_expect_no_error({
    plot_tweedie_diagnostics(final$fit)
    TRUE
  })
  uat_add("Diagnostics plotting executes without error", diag_run$ok, diag_run$err)
  
  summary <- data.frame(
    test = vapply(uat_results, `[[`, character(1), "test"),
    ok = vapply(uat_results, `[[`, logical(1), "ok"),
    detail = vapply(uat_results, function(x) if (is.null(x$detail)) "" else x$detail, character(1)),
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    message("--------------------------------------------------")
    message(sprintf("UAT Summary: %d/%d passed", sum(summary$ok), nrow(summary)))
    if (any(!summary$ok)) {
      message("Failed tests:")
      print(summary[!summary$ok, c("test", "detail")], row.names = FALSE)
    }
  }
  
  invisible(list(results = summary, tuned = tuned, final = final))
}

# -----------------------------
# Example usage (commented)
# -----------------------------
# dat <- simulate_tweedie_data()
# final <- fit_final_tweedie(y ~ x1 + x2, dat, metric = "aic")
# print(final$tuned$best)
# plot_tweedie_diagnostics(final$fit)
# cv <- cross_validate_tweedie(y ~ x1 + x2, dat, K = 5, tune_each_fold = TRUE, metric = "aic")
# print(cv$mse)
# b <- bootstrap_tweedie_coef(y ~ x1 + x2, dat, final$var_power, final$link_power, R = 500)
# print(b)

# -----------------------------
# UAT run (commented)
# -----------------------------
# uat_out <- run_tweedie_uat(profile = "full", verbose = TRUE)
# View(uat_out$results)
