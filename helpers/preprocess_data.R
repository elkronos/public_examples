# Global debug flag and logger function
DEBUG <- TRUE
log_debug <- function(fmt, ...) {
  if (DEBUG) {
    cat(sprintf(paste0("[DEBUG] ", fmt, "\n"), ...))
  }
}

# Load required packages
library(caret)      # For data partitioning and dummyVars
library(mice)       # For missing data imputation (and parallel version)
library(dplyr)      # For data manipulation
library(ranger)     # For random forest (used in feature selection and tuning)
library(magrittr)   # For the pipe operator
library(parallel)   # For parallel processing (for parlmice)
library(Boruta)     # For Boruta feature selection
library(glmnet)     # For LASSO feature selection

### ============================================================
### Helper: Remove Constant or Nearly Constant Columns
### ============================================================
remove_constant_columns <- function(data, tol_var = 1e-6) {
  log_debug("Checking for constant/nearly constant columns (tol_var=%.1e).", tol_var)
  removed <- list()
  keep <- c()
  for (col in names(data)) {
    vec <- data[[col]]
    if (is.numeric(vec)) {
      var_val <- var(vec, na.rm = TRUE)
      if (is.na(var_val) || var_val < tol_var) {
        const_val <- mean(vec, na.rm = TRUE)
        removed[[col]] <- const_val
        log_debug("Numeric column '%s' is nearly constant (variance=%.3e); value=%.3f.", 
                  col, var_val, const_val)
      } else {
        keep <- c(keep, col)
      }
    } else {
      unique_vals <- unique(vec[!is.na(vec)])
      if (length(unique_vals) < 2) {
        removed[[col]] <- unique_vals
        log_debug("Column '%s' is constant (value: %s).", col, paste(unique_vals, collapse = ","))
      } else {
        keep <- c(keep, col)
      }
    }
  }
  new_data <- data[, keep, drop = FALSE]
  list(new_data = new_data, removed = removed)
}

### ============================================================
### 1. Impute Missing Data (with Parallel Option and Fallback)
### ============================================================
impute_missing_data <- function(data,
                                numeric_method = "pmm",
                                binary_factor_method = "logreg",
                                multiclass_factor_method = "polyreg",
                                m = 5,
                                maxit = 50,
                                seed = 500,
                                suppress_warnings = TRUE,
                                parallel = FALSE,
                                ncores = detectCores() - 1) {
  log_debug("Starting impute_missing_data (parallel=%s)", parallel)
  
  # Remove constant/nearly constant columns.
  rc <- remove_constant_columns(data, tol_var = 1e-6)
  data_for_impute <- rc$new_data
  removed_cols <- rc$removed
  
  methods <- sapply(data_for_impute, function(x) {
    if (is.numeric(x)) {
      return(numeric_method)
    } else if (is.factor(x)) {
      if (length(levels(x)) == 2) {
        return(binary_factor_method)
      } else {
        return(multiclass_factor_method)
      }
    } else {
      return("")
    }
  })
  
  set.seed(seed)
  log_debug("Calling mice for imputation on %d columns.", ncol(data_for_impute))
  mice_result <- try({
    if (parallel) {
      # Note: parlmice is deprecated; consider futuremice for production use.
      cl <- makeCluster(ncores)
      clusterEvalQ(cl, { library(mice) })
      res <- parlmice(data_for_impute, m = m, maxit = maxit, method = methods, seed = seed, printFlag = FALSE, cluster = cl)
      stopCluster(cl)
      res
    } else {
      if (suppress_warnings) {
        suppressWarnings(mice(data_for_impute, m = m, maxit = maxit, method = methods, seed = seed, printFlag = FALSE))
      } else {
        mice(data_for_impute, m = m, maxit = maxit, method = methods, seed = seed, printFlag = FALSE)
      }
    }
  }, silent = TRUE)
  
  if (inherits(mice_result, "try-error")) {
    log_debug("MICE imputation failed; using fallback imputation.")
    complete_data <- data_for_impute
    for (col in names(complete_data)) {
      if (any(is.na(complete_data[[col]]))) {
        if (is.numeric(complete_data[[col]])) {
          replacement <- mean(complete_data[[col]], na.rm = TRUE)
          log_debug("Fallback: Numeric column '%s', mean=%.3f", col, replacement)
          complete_data[[col]][is.na(complete_data[[col]])] <- replacement
        } else if (is.factor(complete_data[[col]])) {
          tab <- table(complete_data[[col]])
          mode_val <- names(tab)[which.max(tab)]
          log_debug("Fallback: Factor column '%s', mode=%s", col, mode_val)
          complete_data[[col]][is.na(complete_data[[col]])] <- mode_val
        }
      }
    }
  } else {
    complete_data <- complete(mice_result, 1)
  }
  
  # Post-process any remaining NAs
  if (any(is.na(complete_data))) {
    for (col in names(complete_data)) {
      if (any(is.na(complete_data[[col]]))) {
        if (is.numeric(complete_data[[col]])) {
          replacement <- mean(complete_data[[col]], na.rm = TRUE)
          log_debug("Post-process: Numeric column '%s', replacing NA with mean=%.3f", col, replacement)
          complete_data[[col]][is.na(complete_data[[col]])] <- replacement
        } else if (is.factor(complete_data[[col]])) {
          tab <- table(complete_data[[col]])
          mode_val <- names(tab)[which.max(tab)]
          log_debug("Post-process: Factor column '%s', replacing NA with mode=%s", col, mode_val)
          complete_data[[col]][is.na(complete_data[[col]])] <- mode_val
        }
      }
    }
  }
  
  # Reinsert constant/nearly constant columns.
  if (length(removed_cols) > 0) {
    for (col in names(removed_cols)) {
      log_debug("Reinserting constant column '%s' with value: %s", col, paste(removed_cols[[col]], collapse = ","))
      complete_data[[col]] <- rep(removed_cols[[col]], nrow(complete_data))
    }
  }
  
  log_debug("impute_missing_data complete.")
  return(complete_data)
}

### ============================================================
### 2. Scaling Functions
### ============================================================
fit_scaler <- function(data, scaling_method = "standard") {
  log_debug("Starting fit_scaler with method=%s", scaling_method)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (scaling_method == "standard") {
    params <- lapply(data[numeric_cols], function(x) {
      list(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
    })
  } else if (scaling_method == "minmax") {
    params <- lapply(data[numeric_cols], function(x) {
      list(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
    })
  } else {
    stop("Unsupported scaling_method. Use 'standard' or 'minmax'.")
  }
  log_debug("fit_scaler complete. Numeric columns: %s", paste(numeric_cols, collapse = ", "))
  return(list(params = params, numeric_cols = numeric_cols, method = scaling_method))
}

apply_scaler <- function(data, scaler) {
  log_debug("Starting apply_scaler with method=%s", scaler$method)
  for (col in scaler$numeric_cols) {
    log_debug("Processing column: %s", col)
    if (col %in% names(data)) {
      if (scaler$method == "standard") {
        mean_val <- scaler$params[[col]]$mean
        sd_val <- scaler$params[[col]]$sd
        log_debug("Standard scaling for %s: mean=%.3f, sd=%.3f", col, mean_val, sd_val)
        if (sd_val == 0) {
          data[[col]] <- data[[col]] - mean_val
        } else {
          data[[col]] <- (data[[col]] - mean_val) / sd_val
        }
        log_debug("After standard scaling, %s: %s", col, paste(data[[col]], collapse = ", "))
      } else if (scaler$method == "minmax") {
        min_val <- scaler$params[[col]]$min
        max_val <- scaler$params[[col]]$max
        log_debug("Minmax scaling for %s: min=%.3f, max=%.3f", col, min_val, max_val)
        if (max_val == min_val) {
          data[[col]] <- data[[col]] - min_val
        } else {
          data[[col]] <- as.numeric((data[[col]] - min_val) / (max_val - min_val))
        }
        log_debug("After minmax scaling, %s: %s", col, paste(data[[col]], collapse = ", "))
      }
    }
  }
  log_debug("apply_scaler complete.")
  return(data)
}

### ============================================================
### 3. One–Hot Encoding
### ============================================================
fit_one_hot_encoder <- function(data, outcome_var, fullRank = FALSE) {
  log_debug("Starting fit_one_hot_encoder (excluding outcome: %s)", outcome_var)
  formula <- as.formula(paste("~ . -", outcome_var))
  dummy_model <- caret::dummyVars(formula, data = data, fullRank = fullRank)
  log_debug("fit_one_hot_encoder complete.")
  return(dummy_model)
}

apply_one_hot_encoder <- function(data, dummy_model, outcome_var) {
  log_debug("Starting apply_one_hot_encoder")
  features_encoded <- predict(dummy_model, newdata = data)
  features_encoded <- as.data.frame(features_encoded)
  outcome <- data[[outcome_var]]
  result <- cbind(features_encoded, outcome)
  names(result)[ncol(result)] <- outcome_var
  log_debug("apply_one_hot_encoder complete.")
  return(result)
}

### ============================================================
### 4. Outlier Removal
### ============================================================
fit_outlier_removal <- function(data, multiplier = 1.5) {
  log_debug("Starting fit_outlier_removal with multiplier=%.3f", multiplier)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  thresholds <- list()
  for (col in numeric_cols) {
    x <- data[[col]]
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
    IQR_val <- qnt[2] - qnt[1]
    H <- multiplier * IQR_val
    lower_threshold <- qnt[1] - H
    upper_threshold <- qnt[2] + H
    thresholds[[col]] <- structure(c(lower_threshold, upper_threshold), names = c("lower", "upper"))
    log_debug("Column '%s': Q1=%.3f, Q3=%.3f, IQR=%.3f, H=%.3f, Lower=%.3f, Upper=%.3f",
              col, qnt[1], qnt[2], IQR_val, H, lower_threshold, upper_threshold)
  }
  log_debug("fit_outlier_removal complete.")
  return(thresholds)
}

apply_outlier_removal <- function(data, thresholds) {
  log_debug("Starting apply_outlier_removal")
  for (col in names(thresholds)) {
    if (col %in% names(data) && is.numeric(data[[col]])) {
      lower <- thresholds[[col]]["lower"]
      upper <- thresholds[[col]]["upper"]
      log_debug("Processing column '%s': Lower=%.3f, Upper=%.3f", col, lower, upper)
      idx <- which(data[[col]] < lower | data[[col]] > upper)
      if (length(idx) > 0) {
        log_debug("Column '%s' - indices flagged: %s", col, paste(idx, collapse = ", "))
        log_debug("Column '%s' - values before replacement: %s", col, paste(data[[col]][idx], collapse = ", "))
        data[[col]][idx] <- NA
        log_debug("Column '%s' - values after replacement: %s", col, paste(data[[col]][idx], collapse = ", "))
      } else {
        log_debug("Column '%s' - no outliers flagged.", col)
      }
    }
  }
  log_debug("apply_outlier_removal complete.")
  return(data)
}

### ============================================================
### 5. Generate Interaction Terms
### ============================================================
generate_interaction_terms <- function(data, degree = 2) {
  log_debug("Starting generate_interaction_terms with degree=%d", degree)
  if (degree != 2) stop("Only pairwise (degree = 2) interactions are supported.")
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) < 2) return(data)
  interactions <- list()
  for (i in 1:(length(numeric_cols) - 1)) {
    for (j in (i + 1):length(numeric_cols)) {
      new_col_name <- paste(numeric_cols[i], numeric_cols[j], sep = "_x_")
      interactions[[new_col_name]] <- data[[numeric_cols[i]]] * data[[numeric_cols[j]]]
      log_debug("Created interaction column '%s'", new_col_name)
    }
  }
  interactions_df <- as.data.frame(interactions)
  log_debug("generate_interaction_terms complete.")
  return(cbind(data, interactions_df))
}

### ============================================================
### 6. Apply Transformations to Data
### ============================================================
apply_transformations <- function(data, preproc_params) {
  log_debug("Starting apply_transformations")
  if (!is.data.frame(data)) stop("data must be a dataframe")
  
  if (!is.null(preproc_params$scaler)) {
    data <- apply_scaler(data, preproc_params$scaler)
  }
  if (!is.null(preproc_params$outlier_params)) {
    if (!is.null(preproc_params$outlier_params$thresholds)) {
      data <- apply_outlier_removal(data, preproc_params$outlier_params$thresholds)
    } else {
      thresholds <- fit_outlier_removal(data, multiplier = preproc_params$outlier_params$multiplier)
      data <- apply_outlier_removal(data, thresholds)
    }
  }
  if (!is.null(preproc_params$imputer_params)) {
    data <- impute_missing_data(data,
                                numeric_method = preproc_params$imputer_params$numeric_method,
                                binary_factor_method = preproc_params$imputer_params$binary_factor_method,
                                multiclass_factor_method = preproc_params$imputer_params$multiclass_factor_method,
                                m = preproc_params$imputer_params$m,
                                maxit = preproc_params$imputer_params$maxit,
                                seed = preproc_params$imputer_params$seed,
                                parallel = preproc_params$imputer_params$parallel,
                                ncores = preproc_params$imputer_params$ncores)
  }
  if (!is.null(preproc_params$one_hot_encoder_params)) {
    data <- apply_one_hot_encoder(data,
                                  preproc_params$one_hot_encoder_params$dummy_model,
                                  preproc_params$one_hot_encoder_params$outcome_var)
  }
  if (!is.null(preproc_params$interaction_params)) {
    data <- generate_interaction_terms(data, degree = preproc_params$interaction_params$degree)
  }
  if (!is.null(preproc_params$custom_transform) && is.function(preproc_params$custom_transform)) {
    data <- preproc_params$custom_transform(data)
  }
  log_debug("apply_transformations complete.")
  return(as.data.frame(data))
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
                            feature_selection_method = "ranger",  # Options: "ranger", "Boruta", "LASSO"
                            num_selected_features = 3,
                            ordinal_encoding = FALSE,
                            scale_data_flag = TRUE,
                            scaling_method = "standard",
                            numeric_impute_method = "pmm",
                            binary_factor_impute_method = "logreg",
                            multiclass_factor_impute_method = "polyreg",
                            m = 5,
                            maxit = 50,
                            seed = 500,
                            one_hot_fullRank = FALSE,
                            impute_parallel = FALSE,
                            impute_ncores = detectCores() - 1) {
  log_debug("Starting preprocess_data")
  if (!is.data.frame(data)) stop("data must be a dataframe")
  if (!outcome_var %in% names(data)) stop("outcome_var not found in data")
  if (!is.null(date_vars) && !all(date_vars %in% names(data))) stop("Some date_vars not found in data")
  
  if (ordinal_encoding) {
    orig_levels <- unique(data[[outcome_var]])
    encoding <- setNames(seq_along(orig_levels), orig_levels)
    data[[outcome_var]] <- as.numeric(encoding[as.character(data[[outcome_var]])])
  }
  
  trainIndex <- caret::createDataPartition(data[[outcome_var]], p = partition_ratio, list = FALSE)
  train <- data[trainIndex, , drop = FALSE]
  test  <- data[-trainIndex, , drop = FALSE]
  log_debug("Data partitioned: %d training rows, %d test rows", nrow(train), nrow(test))
  
  if (!is.null(date_vars)) {
    for (var in date_vars) {
      train[[var]] <- as.POSIXct(train[[var]], format = "%Y-%m-%d")
      test[[var]]  <- as.POSIXct(test[[var]], format = "%Y-%m-%d")
    }
  }
  
  if (scale_data_flag) {
    scaler <- fit_scaler(train, scaling_method = scaling_method)
    train <- apply_scaler(train, scaler)
    test  <- apply_scaler(test, scaler)
  }
  
  dummy_model <- fit_one_hot_encoder(train, outcome_var, fullRank = one_hot_fullRank)
  train <- apply_one_hot_encoder(train, dummy_model, outcome_var)
  test  <- apply_one_hot_encoder(test, dummy_model, outcome_var)
  
  thresholds <- fit_outlier_removal(train, multiplier = outlier_multiplier)
  train <- apply_outlier_removal(train, thresholds)
  test  <- apply_outlier_removal(test, thresholds)
  
  train <- impute_missing_data(train,
                               numeric_method = numeric_impute_method,
                               binary_factor_method = binary_factor_impute_method,
                               multiclass_factor_method = multiclass_factor_impute_method,
                               m = m, maxit = maxit, seed = seed,
                               parallel = impute_parallel, ncores = impute_ncores)
  test <- impute_missing_data(test,
                              numeric_method = numeric_impute_method,
                              binary_factor_method = binary_factor_impute_method,
                              multiclass_factor_method = multiclass_factor_impute_method,
                              m = m, maxit = maxit, seed = seed,
                              parallel = impute_parallel, ncores = impute_ncores)
  
  if (interaction_degree > 1) {
    train <- generate_interaction_terms(train, degree = interaction_degree)
    test  <- generate_interaction_terms(test, degree = interaction_degree)
  }
  
  if (!is.null(custom_transform) && is.function(custom_transform)) {
    train <- custom_transform(train)
    test  <- custom_transform(test)
  }
  
  # Feature Selection Step
  if (feature_selection) {
    if (feature_selection_method == "ranger") {
      log_debug("Performing feature selection with ranger.")
      predictors <- setdiff(names(train), outcome_var)
      if (length(predictors) > 0) {
        formula <- as.formula(paste(outcome_var, "~", paste(predictors, collapse = " + ")))
        ranger_mode <- if (is.factor(train[[outcome_var]])) "classification" else "regression"
        rf_model <- ranger(formula, data = train, importance = "impurity",
                           num.trees = 100, classification = (ranger_mode == "classification"))
        imp <- rf_model$variable.importance
        imp <- sort(imp, decreasing = TRUE)
        selected_predictors <- names(imp)[1:min(num_selected_features, length(imp))]
        log_debug("Selected predictors (ranger): %s", paste(selected_predictors, collapse = ", "))
        train <- train[, c(selected_predictors, outcome_var), drop = FALSE]
        test  <- test[, c(selected_predictors, outcome_var), drop = FALSE]
      }
    } else if (feature_selection_method == "Boruta") {
      log_debug("Performing feature selection with Boruta.")
      form <- as.formula(paste(outcome_var, "~ ."))
      boruta_out <- Boruta(form, data = train, doTrace = 0)
      final_vars <- getSelectedAttributes(boruta_out, withTentative = FALSE)
      if (length(final_vars) == 0) {
        final_vars <- setdiff(names(train), outcome_var)
        log_debug("Boruta did not select any variable; using all predictors.")
      }
      log_debug("Selected predictors (Boruta): %s", paste(final_vars, collapse = ", "))
      train <- train[, c(final_vars, outcome_var), drop = FALSE]
      test  <- test[, c(final_vars, outcome_var), drop = FALSE]
    } else if (feature_selection_method == "LASSO") {
      log_debug("Performing feature selection with LASSO (glmnet).")
      predictors <- setdiff(names(train), outcome_var)
      x <- model.matrix(as.formula(paste("~", paste(predictors, collapse = " + "))), data = train)[, -1]
      y <- train[[outcome_var]]
      if (is.factor(y)) {
        if (length(levels(y)) > 2) {
          family <- "multinomial"
        } else {
          family <- "binomial"
        }
      } else {
        family <- "gaussian"
      }
      cvfit <- cv.glmnet(x, y, family = family, alpha = 1, standardize = FALSE)
      if (family == "multinomial") {
        coefs <- coef(cvfit, s = "lambda.min")
        # coefs is a list; take the union of variables with nonzero coefficients across classes
        selected_predictors <- unique(unlist(lapply(coefs, function(mat) {
          rownames(mat)[which(as.numeric(mat) != 0)]
        })))
        selected_predictors <- setdiff(selected_predictors, "(Intercept)")
      } else {
        coefs <- coef(cvfit, s = "lambda.min")
        selected_predictors <- rownames(coefs)[which(as.numeric(coefs) != 0)]
        selected_predictors <- setdiff(selected_predictors, "(Intercept)")
      }
      if (length(selected_predictors) == 0) {
        selected_predictors <- predictors
        log_debug("LASSO did not select any variables; using all predictors.")
      }
      log_debug("Selected predictors (LASSO): %s", paste(selected_predictors, collapse = ", "))
      train <- train[, c(selected_predictors, outcome_var), drop = FALSE]
      test  <- test[, c(selected_predictors, outcome_var), drop = FALSE]
    } else {
      stop("Unknown feature_selection_method. Choose from 'ranger', 'Boruta', or 'LASSO'.")
    }
  }
  
  log_debug("preprocess_data complete.")
  return(list(train = train, test = test))
}

### ============================================================
### 8. Hyperparameter Tuning for Preprocessing
### ============================================================
tune_preprocessing_params <- function(data, outcome_var,
                                      grid = list(outlier_multiplier = c(1.5, 2.0, 2.5),
                                                  scaling_method = c("standard", "minmax"),
                                                  imputation_method = c("pmm")),
                                      feature_selection_method = "ranger",
                                      cv_folds = 3,
                                      metric = NULL,
                                      seed = 123) {
  set.seed(seed)
  log_debug("Starting hyperparameter tuning over grid: %s",
            paste(sapply(grid, paste, collapse = ","), collapse = "; "))
  
  outcome_is_factor <- is.factor(data[[outcome_var]])
  if (is.null(metric)) {
    metric <- if (outcome_is_factor) "Accuracy" else "RMSE"
  }
  
  best_score <- if (outcome_is_factor) -Inf else Inf
  best_params <- list()
  
  train_control <- trainControl(method = "cv", number = cv_folds)
  
  for (om in grid$outlier_multiplier) {
    for (sm in grid$scaling_method) {
      for (im in grid$imputation_method) {
        log_debug("Testing: outlier_multiplier=%.2f, scaling_method=%s, imputation_method=%s", om, sm, im)
        preproc_params <- list(
          scaler = fit_scaler(data, scaling_method = sm),
          outlier_params = list(multiplier = om),
          imputer_params = list(numeric_method = im,
                                binary_factor_method = "logreg",
                                multiclass_factor_method = "polyreg",
                                m = 5, maxit = 50, seed = seed, parallel = FALSE),
          one_hot_encoder_params = list(dummy_model = fit_one_hot_encoder(data, outcome_var, fullRank = TRUE),
                                        outcome_var = outcome_var),
          interaction_params = list(degree = 2),
          custom_transform = function(x) { x }
        )
        data_pp <- impute_missing_data(data, parallel = FALSE)
        splits <- createDataPartition(data_pp[[outcome_var]], p = 0.7, list = FALSE)
        train_data <- data_pp[splits, ]
        train_pp <- apply_transformations(train_data, preproc_params)
        form <- as.formula(paste(outcome_var, "~ ."))
        model <- try(ranger(form, data = train_pp, num.trees = 50, classification = outcome_is_factor), silent = TRUE)
        if (inherits(model, "try-error")) next
        cv_model <- train(form, data = train_pp, method = "ranger",
                          trControl = train_control,
                          tuneLength = 1,
                          metric = metric,
                          num.trees = 50)
        score <- cv_model$results[[metric]]
        log_debug("Score: %s", score)
        if ((outcome_is_factor && score > best_score) ||
            (!outcome_is_factor && score < best_score)) {
          best_score <- score
          best_params <- list(outlier_multiplier = om, scaling_method = sm, imputation_method = im)
          log_debug("New best: %s", paste(names(best_params), best_params, sep = "=", collapse = ", "))
        }
      }
    }
  }
  log_debug("Tuning complete. Best score: %s, Parameters: %s", best_score,
            paste(names(best_params), best_params, sep = "=", collapse = ", "))
  return(list(best_params = best_params, best_score = best_score))
}

### ============================================================
### 9. User Acceptance Testing (UAT) with Improved Synthetic Data
### ============================================================
run_uat <- function() {
  message("Running User Acceptance Tests ...")
  
  # Create synthetic data with increased variability
  set.seed(123)
  n <- 100
  df <- data.frame(
    num1 = c(rnorm(n - 1, mean = 50, sd = 10), 120),  # Deliberate outlier at end
    num2 = rnorm(n, mean = 30, sd = 5),
    fac1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    fac2 = factor(sample(c("X", "Y"), n, replace = TRUE))
  )
  # Introduce random missing values
  df$num1[sample(1:n, 5)] <- NA
  df$num2[sample(1:n, 3)] <- NA
  df$fac1[sample(1:n, 2)] <- NA
  df$fac2[sample(1:n, 2)] <- NA
  
  # Imputation test
  df_imputed <- impute_missing_data(df, parallel = TRUE, ncores = 2)
  stopifnot(all(!is.na(df_imputed)))
  message("impute_missing_data: PASS")
  
  # Scaling tests
  scaler_std <- fit_scaler(df_imputed, scaling_method = "standard")
  df_scaled_std <- apply_scaler(df_imputed, scaler_std)
  stopifnot(is.numeric(df_scaled_std$num1), is.numeric(df_scaled_std$num2))
  
  scaler_mm <- fit_scaler(df_imputed, scaling_method = "minmax")
  df_scaled_mm <- apply_scaler(df_imputed, scaler_mm)
  tol <- 1e-8
  if (!all(df_scaled_mm$num1 >= -tol & df_scaled_mm$num1 <= 1 + tol, na.rm = TRUE))
    stop("Scaling (minmax) for num1 out of bounds.")
  message("Scaling functions: PASS")
  
  # One-hot encoding test
  dummy_model <- fit_one_hot_encoder(df_imputed, outcome_var = "fac1", fullRank = TRUE)
  df_ohe <- apply_one_hot_encoder(df_imputed, dummy_model, outcome_var = "fac1")
  stopifnot("fac1" %in% names(df_ohe))
  message("One-hot encoding functions: PASS")
  
  # Outlier removal test
  log_debug("Original 'num1' values: %s", paste(df$num1, collapse = ", "))
  thresholds_orig <- fit_outlier_removal(df, multiplier = 1.5)
  df_no_outliers_orig <- apply_outlier_removal(df, thresholds_orig)
  if (!is.na(df_no_outliers_orig$num1[nrow(df_no_outliers_orig)]))
    stop("Expected the last value of num1 to be NA after outlier removal.")
  message("Outlier removal functions: PASS")
  
  # Interaction terms test
  df_interact <- generate_interaction_terms(df_imputed, degree = 2)
  stopifnot("num1_x_num2" %in% names(df_interact))
  message("generate_interaction_terms: PASS")
  
  # Test apply_transformations
  preproc_params <- list(
    scaler = scaler_std,
    outlier_params = list(multiplier = 1.5),
    imputer_params = list(numeric_method = "pmm", binary_factor_method = "logreg",
                          multiclass_factor_method = "polyreg", m = 5, maxit = 50, seed = 500, parallel = TRUE, ncores = 2),
    one_hot_encoder_params = list(dummy_model = dummy_model, outcome_var = "fac1"),
    interaction_params = list(degree = 2),
    custom_transform = function(x) { x }
  )
  df_transformed <- apply_transformations(df_imputed, preproc_params)
  stopifnot(is.data.frame(df_transformed))
  message("apply_transformations: PASS")
  
  # Test preprocess_data with each feature selection method
  for (fs_method in c("ranger", "Boruta", "LASSO")) {
    log_debug("Testing preprocess_data with feature_selection_method=%s", fs_method)
    preprocessed <- preprocess_data(df,
                                    outcome_var = "fac1",
                                    partition_ratio = 0.7,
                                    outlier_multiplier = 1.5,
                                    interaction_degree = 2,
                                    feature_selection = TRUE,
                                    feature_selection_method = fs_method,
                                    num_selected_features = 3,
                                    scaling_method = "standard",
                                    numeric_impute_method = "pmm",
                                    binary_factor_impute_method = "logreg",
                                    multiclass_factor_impute_method = "polyreg",
                                    m = 5,
                                    maxit = 50,
                                    seed = 500,
                                    one_hot_fullRank = FALSE,
                                    impute_parallel = TRUE, impute_ncores = 2)
    stopifnot(is.list(preprocessed),
              "train" %in% names(preprocessed),
              "test" %in% names(preprocessed))
    message(sprintf("preprocess_data (%s): PASS", fs_method))
  }
  
  # Test hyperparameter tuning
  tuning_results <- tune_preprocessing_params(df, outcome_var = "fac1",
                                              grid = list(outlier_multiplier = c(1.5, 2.0),
                                                          scaling_method = c("standard", "minmax"),
                                                          imputation_method = c("pmm")))
  message(sprintf("Hyperparameter tuning: Best parameters: %s with score: %s", 
                  paste(names(tuning_results$best_params), tuning_results$best_params, sep = "=", collapse = ", "),
                  tuning_results$best_score))
  
  message("All tests passed!")
}

# Run UAT if in interactive mode
if (interactive()) {
  run_uat()
}
