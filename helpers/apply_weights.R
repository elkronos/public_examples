#############################
# Begin: Weighting Workflow
#############################

#----------------------------
# Small utilities
#----------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

aw_stop <- function(...) stop(paste0(...), call. = FALSE)

aw_is_scalar_logical <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
aw_is_scalar_numeric  <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
aw_is_nonempty_string <- function(x) is.character(x) && length(x) == 1L && nzchar(x)

aw_as_chr_vec_or_null <- function(x, arg_name = "value") {
  if (is.null(x)) return(NULL)
  if (is.character(x)) return(as.character(x))
  aw_stop("`", arg_name, "` must be a character vector (or single string) or NULL.")
}

aw_mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

aw_as_named_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x)) return(x)
  if (is.atomic(x) && !is.null(names(x))) return(as.list(x))
  aw_stop("`fill_values` must be a named list or a named atomic vector.")
}

aw_unique_names_or_stop <- function(x, arg_name) {
  if (anyNA(x) || any(!nzchar(x))) {
    aw_stop("`", arg_name, "` contains NA/empty names.")
  }
  if (any(duplicated(x))) {
    aw_stop("`", arg_name, "` contains duplicate names: ", paste(unique(x[duplicated(x)]), collapse = ", "))
  }
  invisible(TRUE)
}

#----------------------------
# Column name resolution
#----------------------------

aw_resolve_columns_to_weight <- function(df, spec) {
  if (is.null(spec)) {
    return(names(df)[vapply(df, is.numeric, logical(1))])
  }
  if (is.function(spec)) {
    cols <- spec(df)
    if (!is.character(cols)) aw_stop("`columns_to_weight` function must return a character vector of column names.")
    return(as.character(cols))
  }
  if (is.character(spec)) return(as.character(spec))
  aw_stop("`columns_to_weight` must be NULL, a string/character vector, or a function(df)->character.")
}

aw_validate_columns_to_weight <- function(df, cols) {
  cols_existing <- intersect(cols, names(df))
  if (!length(cols_existing)) aw_stop("None of the specified `columns_to_weight` exist in `data`.")
  cols_numeric <- cols_existing[vapply(cols_existing, function(nm) is.numeric(df[[nm]]), logical(1))]
  if (!length(cols_numeric)) aw_stop("No numeric columns were found among the specified `columns_to_weight`.")
  cols_numeric
}

aw_resolve_output_names <- function(cols, spec, value_suffix) {
  if (is.null(spec)) {
    out <- paste0(cols, value_suffix)
    aw_unique_names_or_stop(out, "new_column_names")
    return(out)
  }
  
  if (is.function(spec)) {
    out <- vapply(cols, function(cn) as.character(spec(cn)), character(1))
    aw_unique_names_or_stop(out, "new_column_names")
    return(unname(out))
  }
  
  if (is.character(spec) && !is.null(names(spec)) && any(nzchar(names(spec)))) {
    out <- vapply(cols, function(cn) {
      if (cn %in% names(spec)) {
        val <- spec[[cn]]
        val_chr <- as.character(val)
        if (!is.na(val_chr) && nzchar(val_chr)) return(val_chr)
      }
      paste0(cn, value_suffix)
    }, character(1))
    aw_unique_names_or_stop(out, "new_column_names")
    return(out)
  }
  
  if (is.character(spec)) {
    if (length(spec) != length(cols)) {
      aw_stop("Length of `new_column_names` must equal length of `columns_to_weight`.")
    }
    out <- as.character(spec)
    aw_unique_names_or_stop(out, "new_column_names")
    return(out)
  }
  
  aw_stop("`new_column_names` must be NULL, a character vector, a named character mapping, or a function(col)->name.")
}

#----------------------------
# Weights validation
#----------------------------

aw_validate_weight_vec_structure <- function(var, w) {
  if (!is.numeric(w) || is.null(names(w))) {
    aw_stop("`demographic_weights` for '", var, "' must be a named numeric vector.")
  }
  nm <- names(w)
  if (any(!nzchar(nm))) {
    aw_stop("`demographic_weights` for '", var, "' has empty names; names must match levels.")
  }
  if (any(duplicated(nm))) {
    aw_stop("`demographic_weights` for '", var, "' has duplicate names: ", paste(unique(nm[duplicated(nm)]), collapse = ", "))
  }
  if (anyNA(w) || any(!is.finite(w))) {
    aw_stop("`demographic_weights` for '", var, "' contains NA or non-finite values.")
  }
  invisible(TRUE)
}

aw_validate_demographic_weights_structure <- function(demographic_weights) {
  if (!is.list(demographic_weights) || is.null(names(demographic_weights))) {
    aw_stop("`demographic_weights` must be a *named* list of named numeric vectors.")
  }
  if (any(!nzchar(names(demographic_weights)))) {
    aw_stop("`demographic_weights` must have non-empty names.")
  }
  for (var in names(demographic_weights)) {
    aw_validate_weight_vec_structure(var, demographic_weights[[var]])
  }
  invisible(TRUE)
}

aw_check_missing_levels <- function(var, df, wvec) {
  observed <- unique(as.character(df[[var]]))
  observed <- observed[!is.na(observed)]
  missing_levels <- setdiff(observed, names(wvec))
  if (length(missing_levels) > 0) {
    aw_stop(
      "`demographic_weights` for '", var, "' is missing weights for levels: ",
      paste(missing_levels, collapse = ", ")
    )
  }
  invisible(TRUE)
}

aw_validate_weights_against_data <- function(df,
                                             demographic_vars,
                                             demographic_weights,
                                             require_all_levels = TRUE,
                                             validate_all_weights = TRUE) {
  if (!aw_is_scalar_logical(require_all_levels)) aw_stop("`require_all_levels` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(validate_all_weights)) aw_stop("`validate_all_weights` must be TRUE/FALSE.")
  
  vars_to_check <- if (isTRUE(validate_all_weights)) names(demographic_weights) else demographic_vars
  
  for (var in vars_to_check) {
    if (!var %in% names(df)) next
    wvec <- demographic_weights[[var]]
    if (is.null(wvec)) next
    
    aw_validate_weight_vec_structure(var, wvec)
    if (isTRUE(require_all_levels)) aw_check_missing_levels(var, df, wvec)
  }
  
  invisible(TRUE)
}

#----------------------------
# NA handling
#----------------------------

aw_fill_na_cols <- function(df, cols, fill_values = NULL) {
  fill_values <- aw_as_named_list(fill_values)
  
  for (nm in cols) {
    if (!nm %in% names(df)) next
    v <- df[[nm]]
    if (!anyNA(v)) next
    
    if (!is.null(fill_values[[nm]])) {
      df[[nm]][is.na(df[[nm]])] <- fill_values[[nm]]
      next
    }
    
    if (is.numeric(v)) {
      mu <- mean(v, na.rm = TRUE)
      if (!is.nan(mu)) df[[nm]][is.na(df[[nm]])] <- mu
    } else {
      m <- aw_mode1(v)
      if (!is.na(m)) df[[nm]][is.na(df[[nm]])] <- m
    }
  }
  
  df
}

aw_handle_na <- function(df,
                         handle_na,
                         demographic_vars,
                         cols_to_weight,
                         na_subset = NULL,
                         fill_values = NULL,
                         fill_subset = NULL) {
  if (!handle_na %in% c("drop", "fill", "ignore")) {
    aw_stop("`handle_na` must be one of: 'drop', 'fill', 'ignore'.")
  }
  
  if (handle_na == "drop") {
    subset_cols <- na_subset %||% unique(c(demographic_vars, cols_to_weight))
    subset_cols <- intersect(subset_cols, names(df))
    if (length(subset_cols)) {
      df <- df[stats::complete.cases(df[, subset_cols, drop = FALSE]), , drop = FALSE]
    }
    return(df)
  }
  
  if (handle_na == "fill") {
    cols <- fill_subset %||% unique(c(demographic_vars, cols_to_weight))
    cols <- intersect(cols, names(df))
    if (length(cols)) df <- aw_fill_na_cols(df, cols, fill_values)
    return(df)
  }
  
  df
}

#----------------------------
# Weight computation
#----------------------------

aw_compute_weight_vectors <- function(df,
                                      demographic_vars,
                                      demographic_weights,
                                      default_weight = 1,
                                      require_all_levels = TRUE) {
  if (!aw_is_scalar_numeric(default_weight)) aw_stop("`default_weight` must be a single non-NA numeric value.")
  
  n <- nrow(df)
  weight_by_var <- list()
  
  for (var in demographic_vars) {
    levels_chr <- as.character(df[[var]])
    
    if (!is.null(demographic_weights[[var]])) {
      wvec <- demographic_weights[[var]]
      
      if (isTRUE(require_all_levels)) aw_check_missing_levels(var, df, wvec)
      
      w <- unname(wvec[levels_chr])
      w[is.na(w)] <- default_weight
      weight_by_var[[var]] <- as.numeric(w)
    } else {
      weight_by_var[[var]] <- rep(default_weight, n)
    }
  }
  
  total_weight <- if (length(weight_by_var)) Reduce(`*`, weight_by_var) else rep(1, n)
  
  list(weight_by_var = weight_by_var, total_weight = total_weight)
}

#----------------------------
# Output assembly
#----------------------------

aw_required_output_columns <- function(cols_to_weight,
                                       out_names,
                                       demographic_vars,
                                       weight_suffix,
                                       verbose,
                                       verbose_mode,
                                       add_total_weight_per_column,
                                       total_weight_col) {
  cols <- character(0)
  
  if (!is.null(total_weight_col)) cols <- c(cols, total_weight_col)
  
  if (isTRUE(verbose) && identical(verbose_mode, "per_variable")) {
    cols <- c(cols, paste0(demographic_vars, weight_suffix))
  }
  
  for (i in seq_along(cols_to_weight)) {
    col <- cols_to_weight[i]
    cols <- c(cols, out_names[i])
    
    if (isTRUE(add_total_weight_per_column)) {
      cols <- c(cols, paste0(col, "_total", weight_suffix))
    }
    
    if (isTRUE(verbose) && identical(verbose_mode, "per_column")) {
      cols <- c(cols, paste0(col, "_", demographic_vars, weight_suffix))
    }
  }
  
  cols
}

aw_apply_to_empty_df <- function(df,
                                 cols_to_weight,
                                 out_names,
                                 demographic_vars,
                                 weight_suffix,
                                 verbose,
                                 verbose_mode,
                                 add_total_weight_per_column,
                                 total_weight_col) {
  out <- df
  
  if (!is.null(total_weight_col)) out[[total_weight_col]] <- numeric(0)
  
  if (isTRUE(verbose) && identical(verbose_mode, "per_variable")) {
    for (var in demographic_vars) out[[paste0(var, weight_suffix)]] <- numeric(0)
  }
  
  for (i in seq_along(cols_to_weight)) {
    col <- cols_to_weight[i]
    
    if (isTRUE(add_total_weight_per_column)) {
      out[[paste0(col, "_total", weight_suffix)]] <- numeric(0)
    }
    
    out[[out_names[i]]] <- numeric(0)
    
    if (isTRUE(verbose) && identical(verbose_mode, "per_column")) {
      for (var in demographic_vars) {
        out[[paste0(col, "_", var, weight_suffix)]] <- numeric(0)
      }
    }
  }
  
  out
}

aw_add_weighted_columns <- function(df,
                                    cols_to_weight,
                                    out_names,
                                    weight_by_var,
                                    total_weight,
                                    weight_suffix,
                                    verbose,
                                    verbose_mode,
                                    add_total_weight_per_column,
                                    total_weight_col) {
  out <- df
  
  if (!is.null(total_weight_col)) out[[total_weight_col]] <- total_weight
  
  if (isTRUE(verbose) && identical(verbose_mode, "per_variable")) {
    for (var in names(weight_by_var)) out[[paste0(var, weight_suffix)]] <- weight_by_var[[var]]
  }
  
  for (i in seq_along(cols_to_weight)) {
    col <- cols_to_weight[i]
    out_col <- out_names[i]
    
    if (isTRUE(verbose) && identical(verbose_mode, "per_column")) {
      for (var in names(weight_by_var)) {
        out[[paste0(col, "_", var, weight_suffix)]] <- weight_by_var[[var]]
      }
    }
    
    if (isTRUE(add_total_weight_per_column)) {
      out[[paste0(col, "_total", weight_suffix)]] <- total_weight
    }
    
    out[[out_col]] <- out[[col]] * total_weight
  }
  
  out
}

aw_check_overwrite <- function(existing_names, new_names, overwrite) {
  if (!aw_is_scalar_logical(overwrite)) aw_stop("`overwrite` must be TRUE/FALSE.")
  if (isTRUE(overwrite)) return(invisible(TRUE))
  
  dup <- intersect(existing_names, new_names)
  if (length(dup)) aw_stop("The following output columns already exist: ", paste(dup, collapse = ", "))
  invisible(TRUE)
}

#----------------------------
# Plan object
#----------------------------

#' Create a weighting plan
#'
#' Stores weighting settings so the same configuration can be applied repeatedly.
#'
#' @param demographic_vars Character vector (or single string) of demographic variables to use for weighting.
#'   If NULL, uses names(demographic_weights).
#' @param demographic_weights Named list of named numeric vectors: list(var = c(level = weight, ...), ...)
#' @param columns_to_weight NULL (default = all numeric columns), character vector/string, or function(df)->character.
#' @param new_column_names NULL (default = paste0(col, value_suffix)), character vector, named mapping, or function(col)->name.
#' @param weight_suffix Suffix for weight columns. Default "_weight".
#' @param value_suffix Suffix for weighted value columns. Default "_weighted".
#' @param verbose If TRUE, add columns for individual demographic weights.
#' @param drop_na Backward-compatible flag. If TRUE (default), drop rows with NA in relevant columns.
#' @param handle_na One of c("drop","fill","ignore"). If NULL, derived from drop_na.
#' @param na_subset Optional character vector used when handle_na=="drop".
#' @param fill_values Optional named list/vector used when handle_na=="fill".
#' @param fill_subset Optional character vector to limit which columns are filled when handle_na=="fill".
#' @param default_weight Numeric. Weight used when mappings are missing. Default 1.
#' @param require_all_levels Logical. If TRUE, error when observed non-NA levels lack weights. Default TRUE.
#' @param validate_all_weights Logical. If TRUE, validates all demographic_weights entries that appear in data. Default TRUE.
#' @param total_weight_col Optional string. If set, adds a single combined weight column.
#' @param verbose_mode One of c("per_column","per_variable"). Default "per_column".
#' @param add_total_weight_per_column Logical. If TRUE (default), adds <col>_total<weight_suffix> per weighted column.
#' @param overwrite Logical. If FALSE, stops when output columns already exist.
#'
#' @return An object of class "weighting_plan".
#' @export
make_weighting_plan <- function(
    demographic_vars,
    demographic_weights,
    columns_to_weight = NULL,
    new_column_names = NULL,
    weight_suffix = "_weight",
    value_suffix = "_weighted",
    verbose = FALSE,
    drop_na = TRUE,
    handle_na = NULL,
    na_subset = NULL,
    fill_values = NULL,
    fill_subset = NULL,
    default_weight = 1,
    require_all_levels = TRUE,
    validate_all_weights = TRUE,
    total_weight_col = NULL,
    verbose_mode = c("per_column", "per_variable"),
    add_total_weight_per_column = TRUE,
    overwrite = TRUE
) {
  verbose_mode <- match.arg(verbose_mode)
  
  aw_validate_demographic_weights_structure(demographic_weights)
  
  if (!aw_is_scalar_numeric(default_weight)) aw_stop("`default_weight` must be a single non-NA numeric value.")
  if (!aw_is_scalar_logical(verbose)) aw_stop("`verbose` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(drop_na)) aw_stop("`drop_na` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(require_all_levels)) aw_stop("`require_all_levels` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(validate_all_weights)) aw_stop("`validate_all_weights` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(add_total_weight_per_column)) aw_stop("`add_total_weight_per_column` must be TRUE/FALSE.")
  if (!aw_is_scalar_logical(overwrite)) aw_stop("`overwrite` must be TRUE/FALSE.")
  
  if (!is.null(total_weight_col) && !aw_is_nonempty_string(total_weight_col)) {
    aw_stop("`total_weight_col` must be a non-empty string or NULL.")
  }
  
  # Derive handle_na from drop_na unless explicitly provided
  if (is.null(handle_na)) handle_na <- if (isTRUE(drop_na)) "drop" else "ignore"
  if (!handle_na %in% c("drop", "fill", "ignore")) aw_stop("`handle_na` must be one of: 'drop', 'fill', 'ignore'.")
  
  plan <- list(
    demographic_vars = aw_as_chr_vec_or_null(demographic_vars, "demographic_vars"),
    demographic_weights = demographic_weights,
    columns_to_weight = columns_to_weight,
    new_column_names = new_column_names,
    weight_suffix = weight_suffix,
    value_suffix = value_suffix,
    verbose = verbose,
    drop_na = drop_na,
    handle_na = handle_na,
    na_subset = aw_as_chr_vec_or_null(na_subset, "na_subset"),
    fill_values = fill_values,
    fill_subset = aw_as_chr_vec_or_null(fill_subset, "fill_subset"),
    default_weight = default_weight,
    require_all_levels = require_all_levels,
    validate_all_weights = validate_all_weights,
    total_weight_col = total_weight_col,
    verbose_mode = verbose_mode,
    add_total_weight_per_column = add_total_weight_per_column,
    overwrite = overwrite
  )
  
  class(plan) <- "weighting_plan"
  plan
}

#' Apply a weighting plan to a data frame
#'
#' @param data A data frame.
#' @param plan A "weighting_plan" created by make_weighting_plan().
#' @param copy Logical. If TRUE (default), work on a copy.
#'
#' @return Data frame with added weight columns and weighted numeric columns.
#' @export
apply_weighting_plan <- function(data, plan, copy = TRUE) {
  if (!inherits(plan, "weighting_plan")) aw_stop("`plan` must be a weighting_plan object.")
  if (!is.data.frame(data)) aw_stop("`data` must be a data frame.")
  if (!aw_is_scalar_logical(copy)) aw_stop("`copy` must be TRUE/FALSE.")
  
  df <- if (isTRUE(copy)) data else data
  
  demographic_vars <- plan$demographic_vars
  if (is.null(demographic_vars)) demographic_vars <- names(plan$demographic_weights)
  
  missing_demos <- setdiff(demographic_vars, names(df))
  if (length(missing_demos) > 0) {
    aw_stop("The following demographic variables are missing in data: ", paste(missing_demos, collapse = ", "))
  }
  
  cols_to_weight_raw <- aw_resolve_columns_to_weight(df, plan$columns_to_weight)
  cols_to_weight <- aw_validate_columns_to_weight(df, cols_to_weight_raw)
  out_names <- aw_resolve_output_names(cols_to_weight, plan$new_column_names, plan$value_suffix)
  
  aw_validate_weights_against_data(
    df = df,
    demographic_vars = demographic_vars,
    demographic_weights = plan$demographic_weights,
    require_all_levels = plan$require_all_levels,
    validate_all_weights = plan$validate_all_weights
  )
  
  df <- aw_handle_na(
    df = df,
    handle_na = plan$handle_na,
    demographic_vars = demographic_vars,
    cols_to_weight = cols_to_weight,
    na_subset = plan$na_subset,
    fill_values = plan$fill_values,
    fill_subset = plan$fill_subset
  )
  
  n <- nrow(df)
  if (n == 0L) {
    empty_out <- aw_apply_to_empty_df(
      df = df,
      cols_to_weight = cols_to_weight,
      out_names = out_names,
      demographic_vars = demographic_vars,
      weight_suffix = plan$weight_suffix,
      verbose = plan$verbose,
      verbose_mode = plan$verbose_mode,
      add_total_weight_per_column = plan$add_total_weight_per_column,
      total_weight_col = plan$total_weight_col
    )
    return(empty_out)
  }
  
  w <- aw_compute_weight_vectors(
    df = df,
    demographic_vars = demographic_vars,
    demographic_weights = plan$demographic_weights,
    default_weight = plan$default_weight,
    require_all_levels = plan$require_all_levels
  )
  
  new_cols <- aw_required_output_columns(
    cols_to_weight = cols_to_weight,
    out_names = out_names,
    demographic_vars = demographic_vars,
    weight_suffix = plan$weight_suffix,
    verbose = plan$verbose,
    verbose_mode = plan$verbose_mode,
    add_total_weight_per_column = plan$add_total_weight_per_column,
    total_weight_col = plan$total_weight_col
  )
  
  aw_check_overwrite(names(df), new_cols, plan$overwrite)
  
  out <- aw_add_weighted_columns(
    df = df,
    cols_to_weight = cols_to_weight,
    out_names = out_names,
    weight_by_var = w$weight_by_var,
    total_weight = w$total_weight,
    weight_suffix = plan$weight_suffix,
    verbose = plan$verbose,
    verbose_mode = plan$verbose_mode,
    add_total_weight_per_column = plan$add_total_weight_per_column,
    total_weight_col = plan$total_weight_col
  )
  
  out
}

#' Apply demographic weights to numeric columns in a data frame
#'
#' Convenience wrapper around make_weighting_plan() + apply_weighting_plan().
#'
#' @param data A data frame.
#' @param demographic_vars Character vector (or single string) of demographic variables to use for weighting.
#'   If NULL, uses names(demographic_weights).
#' @param demographic_weights Named list of named numeric vectors: list(var = c(level = weight, ...), ...)
#' @param columns_to_weight NULL (default = all numeric columns), character vector/string, or function(df)->character.
#' @param new_column_names NULL (default = paste0(col, value_suffix)), character vector, named mapping, or function(col)->name.
#' @param weight_suffix Suffix for weight columns. Default "_weight".
#' @param value_suffix Suffix for weighted value columns. Default "_weighted".
#' @param verbose If TRUE, add columns for individual demographic weights.
#' @param drop_na Backward-compatible flag. If TRUE (default), drop rows with NA in relevant columns.
#' @param handle_na One of c("drop","fill","ignore"). If NULL, derived from drop_na.
#' @param na_subset Optional character vector used when handle_na=="drop".
#' @param fill_values Optional named list/vector used when handle_na=="fill".
#' @param fill_subset Optional character vector to limit which columns are filled when handle_na=="fill".
#' @param default_weight Numeric. Weight used when mappings are missing. Default 1.
#' @param require_all_levels Logical. If TRUE, error when observed non-NA levels lack weights. Default TRUE.
#' @param validate_all_weights Logical. If TRUE, validates all demographic_weights entries that appear in data. Default TRUE.
#' @param total_weight_col Optional string. If set, adds a single combined weight column.
#' @param verbose_mode One of c("per_column","per_variable"). Default "per_column".
#' @param add_total_weight_per_column Logical. If TRUE (default), adds <col>_total<weight_suffix> per weighted column.
#' @param overwrite Logical. If FALSE, stops when output columns already exist.
#' @param copy Logical. If TRUE (default), work on a copy.
#'
#' @return Data frame with added weight columns and weighted numeric columns.
#' @export
apply_weights <- function(
    data,
    demographic_vars,
    demographic_weights,
    columns_to_weight = NULL,
    new_column_names = NULL,
    weight_suffix = "_weight",
    value_suffix = "_weighted",
    verbose = FALSE,
    drop_na = TRUE,
    handle_na = NULL,
    na_subset = NULL,
    fill_values = NULL,
    fill_subset = NULL,
    default_weight = 1,
    require_all_levels = TRUE,
    validate_all_weights = TRUE,
    total_weight_col = NULL,
    verbose_mode = c("per_column", "per_variable"),
    add_total_weight_per_column = TRUE,
    overwrite = TRUE,
    copy = TRUE
) {
  plan <- make_weighting_plan(
    demographic_vars = demographic_vars,
    demographic_weights = demographic_weights,
    columns_to_weight = columns_to_weight,
    new_column_names = new_column_names,
    weight_suffix = weight_suffix,
    value_suffix = value_suffix,
    verbose = verbose,
    drop_na = drop_na,
    handle_na = handle_na,
    na_subset = na_subset,
    fill_values = fill_values,
    fill_subset = fill_subset,
    default_weight = default_weight,
    require_all_levels = require_all_levels,
    validate_all_weights = validate_all_weights,
    total_weight_col = total_weight_col,
    verbose_mode = verbose_mode,
    add_total_weight_per_column = add_total_weight_per_column,
    overwrite = overwrite
  )
  
  apply_weighting_plan(data = data, plan = plan, copy = copy)
}

#----------------------------
# Optional: printing
#----------------------------

#' @export
print.weighting_plan <- function(x, ...) {
  cat("<weighting_plan>\n")
  cat("  demographic_vars: ",
      if (is.null(x$demographic_vars)) "<from weights>" else paste(x$demographic_vars, collapse = ", "),
      "\n", sep = "")
  cat("  handle_na: ", x$handle_na, "\n", sep = "")
  cat("  verbose_mode: ", x$verbose_mode, "\n", sep = "")
  cat("  total_weight_col: ", if (is.null(x$total_weight_col)) "<none>" else x$total_weight_col, "\n", sep = "")
  invisible(x)
}

#############################
# End: Weighting Workflow
#############################
