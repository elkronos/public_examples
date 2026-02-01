# scatter_norm.R
# Dependencies: ggplot2, rlang, plotly

# ---------------------------
# Internal helpers
# ---------------------------

.sn_abort <- function(msg) rlang::abort(msg, call = NULL)

.sn_assert_df <- function(x) {
  if (!is.data.frame(x)) .sn_abort("`data` must be a data frame.")
}

.sn_assert_col <- function(df, nm) {
  if (!nm %in% names(df)) .sn_abort(paste0("Column `", nm, "` not found in `data`."))
}

.sn_assert_numeric <- function(v, nm) {
  if (!is.numeric(v)) .sn_abort(paste0("Column `", nm, "` must be numeric."))
}

.sn_zscore <- function(v, nm) {
  m <- mean(v)
  s <- stats::sd(v)
  if (is.na(s) || s == 0) .sn_abort(paste0("Standard deviation is zero/NA for `", nm, "`. Cannot rescale."))
  (v - m) / s
}

.sn_quo_to_colname <- function(q, arg_name = "aesthetic") {
  if (rlang::quo_is_null(q)) return(NULL)
  expr <- rlang::get_expr(q)
  if (rlang::is_symbol(expr)) return(rlang::as_name(expr))
  if (is.character(expr) && length(expr) == 1) return(expr)
  .sn_abort(paste0("`", arg_name, "` must be an unquoted column name or a single string."))
}

.sn_capture_optional_mapping <- function(data, color_q, fill_q, shape_q) {
  color_name <- .sn_quo_to_colname(color_q, "color")
  fill_name  <- .sn_quo_to_colname(fill_q,  "fill")
  shape_name <- .sn_quo_to_colname(shape_q, "shape")
  
  if (!is.null(color_name)) .sn_assert_col(data, color_name)
  if (!is.null(fill_name))  .sn_assert_col(data, fill_name)
  if (!is.null(shape_name)) .sn_assert_col(data, shape_name)
  
  list(color = color_name, fill = fill_name, shape = shape_name)
}

.sn_drop_complete_xy <- function(data, x_name, y_name) {
  keep <- stats::complete.cases(data[[x_name]], data[[y_name]])
  out <- data[keep, , drop = FALSE]
  if (nrow(out) < 2) .sn_abort("Not enough complete cases for analysis (need at least 2 rows).")
  out
}

.sn_make_tooltip <- function(x_name, y_name, x_use, y_use, xz, yz) {
  paste0(
    x_name, ": ", signif(x_use, 4),
    "<br>", y_name, ": ", signif(y_use, 4),
    "<br>", "x_z: ", signif(xz, 4),
    "<br>", "y_z: ", signif(yz, 4)
  )
}

.sn_prepare_plot_df <- function(data_complete, x_name, y_name) {
  x_use <- data_complete[[x_name]]
  y_use <- data_complete[[y_name]]
  
  xz <- .sn_zscore(x_use, x_name)
  yz <- .sn_zscore(y_use, y_name)
  
  df_plot <- data_complete
  df_plot$.row       <- seq_along(xz)
  df_plot$x_rescaled <- xz
  df_plot$y_rescaled <- yz
  df_plot$.tooltip   <- .sn_make_tooltip(x_name, y_name, x_use, y_use, xz, yz)
  
  list(df_plot = df_plot, x_use = x_use, y_use = y_use, xz = xz, yz = yz)
}

.sn_quadrant_cut <- function(xz, yz, quadrant_cut) {
  if (quadrant_cut == "median") {
    list(cut_x = stats::median(xz), cut_y = stats::median(yz))
  } else {
    list(cut_x = 0, cut_y = 0)
  }
}

.sn_quadrant_stats <- function(xz, yz, cut_x, cut_y, digits = 1L) {
  q1 <- sum(xz >  cut_x & yz >  cut_y)
  q2 <- sum(xz <= cut_x & yz >  cut_y)
  q3 <- sum(xz <= cut_x & yz <= cut_y)
  q4 <- sum(xz >  cut_x & yz <= cut_y)
  
  counts <- c(q1, q2, q3, q4)
  total <- sum(counts)
  pct <- if (total == 0) rep(0, 4) else round((counts / total) * 100, digits)
  
  data.frame(
    quadrant = c("Q1", "Q2", "Q3", "Q4"),
    count = counts,
    pct = pct,
    label = paste0(c("Q1: ", "Q2: ", "Q3: ", "Q4: "), pct, "%"),
    stringsAsFactors = FALSE
  )
}

.sn_quadrant_label_positions <- function(xz, yz) {
  data.frame(
    x = c(stats::quantile(xz, 0.75), stats::quantile(xz, 0.25),
          stats::quantile(xz, 0.25), stats::quantile(xz, 0.75)),
    y = c(stats::quantile(yz, 0.75), stats::quantile(yz, 0.75),
          stats::quantile(yz, 0.25), stats::quantile(yz, 0.25))
  )
}

.sn_compute_cor_stats <- function(x_use, y_use, r_method, r_digits, r_show_p, r_show_n) {
  ct <- stats::cor.test(x_use, y_use, method = r_method)
  r_val <- unname(ct$estimate)
  r_txt <- format(round(r_val, r_digits), nsmall = r_digits)
  
  p_txt <- NULL
  if (isTRUE(r_show_p)) {
    p <- ct$p.value
    p_txt <- if (is.na(p)) "NA" else if (p < 0.001) "< 0.001" else format(round(p, 3), nsmall = 3)
  }
  
  lab <- paste0("r = ", r_txt)
  if (isTRUE(r_show_p)) lab <- paste0(lab, "  ·  p ", p_txt)
  if (isTRUE(r_show_n)) lab <- paste0(lab, "  ·  n = ", length(x_use))
  
  list(label = lab, r = r_val, p_value = ct$p.value)
}

.sn_quosure <- function(expr) rlang::new_quosure(expr, env = rlang::empty_env())

.sn_build_mapping <- function(map_names, include_text = FALSE) {
  m <- list(
    x = .sn_quosure(rlang::sym("x_rescaled")),
    y = .sn_quosure(rlang::sym("y_rescaled"))
  )
  
  if (isTRUE(include_text)) {
    m$text <- .sn_quosure(rlang::sym(".tooltip"))
  }
  
  if (!is.null(map_names$color)) m$colour <- .sn_quosure(rlang::sym(map_names$color))
  if (!is.null(map_names$fill))  m$fill   <- .sn_quosure(rlang::sym(map_names$fill))
  if (!is.null(map_names$shape)) m$shape  <- .sn_quosure(rlang::sym(map_names$shape))
  
  class(m) <- "uneval"
  m
}

.sn_apply_style_preset <- function(style, theme, point_args, smooth_args, fill_mapped) {
  if (style != "clean") {
    if (isTRUE(fill_mapped) && is.null(point_args$shape))  point_args$shape  <- 21
    if (isTRUE(fill_mapped) && is.null(point_args$stroke)) point_args$stroke <- 0.4
    return(list(theme = theme, point_args = point_args, smooth_args = smooth_args))
  }
  
  if (missing(theme)) {
    theme <- ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(linewidth = 0.25),
        axis.title       = ggplot2::element_text(face = "bold"),
        plot.margin      = ggplot2::margin(10, 12, 10, 12)
      )
  }
  
  point_args <- utils::modifyList(list(size = 2.2, alpha = 0.85), point_args)
  
  if (isTRUE(fill_mapped) && is.null(point_args$shape))  point_args$shape  <- 21
  if (isTRUE(fill_mapped) && is.null(point_args$stroke)) point_args$stroke <- 0.4
  
  smooth_args <- utils::modifyList(list(se = TRUE), smooth_args)
  
  list(theme = theme, point_args = point_args, smooth_args = smooth_args)
}

.sn_resolve_smoother <- function(
    smooth, smooth_method, smooth_args,
    loess_span,
    loess_family, loess_family_provided,
    style_clean,
    smooth_group
) {
  if (!isTRUE(smooth) || smooth_method == "none") return(NULL)
  
  if (is.null(smooth_args$method)) smooth_args$method <- smooth_method
  method <- smooth_args$method
  
  if (identical(method, "loess")) {
    if (!is.null(loess_span) && is.null(smooth_args$span)) smooth_args$span <- loess_span
    if (isTRUE(style_clean) && is.null(smooth_args$span) && is.null(loess_span)) smooth_args$span <- 0.9
  }
  
  method_args <- smooth_args$method.args
  if (is.null(method_args)) method_args <- list()
  
  if (!is.null(smooth_args$family)) {
    method_args$family <- smooth_args$family
    smooth_args$family <- NULL
  }
  
  if (identical(method, "loess")) {
    if (isTRUE(loess_family_provided)) {
      fam <- match.arg(loess_family, c("gaussian", "symmetric"))
      if (is.null(method_args$family)) method_args$family <- fam
    } else if (isTRUE(style_clean) && is.null(method_args$family)) {
      method_args$family <- "symmetric"
    }
  }
  
  if (length(method_args) > 0) smooth_args$method.args <- method_args
  
  if (is.null(smooth_args$linewidth)) smooth_args$linewidth <- 1.0
  smooth_args$size <- NULL
  
  if (smooth_group == "overall") {
    if (is.null(smooth_args$inherit.aes)) smooth_args$inherit.aes <- FALSE
    smooth_args$mapping <- ggplot2::aes(x = x_rescaled, y = y_rescaled, group = 1)
  }
  
  smooth_args
}

.sn_add_annotation_layer <- function(p, data, mapping, interactive, ...) {
  if (isTRUE(interactive)) {
    p + ggplot2::geom_text(data = data, mapping = mapping, inherit.aes = FALSE, ...)
  } else {
    p + ggplot2::geom_label(data = data, mapping = mapping, inherit.aes = FALSE, label.size = 0, ...)
  }
}

.sn_add_ref_lines <- function(p, cut_x, cut_y) {
  p +
    ggplot2::geom_hline(yintercept = cut_y, linewidth = 0.6, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = cut_x, linewidth = 0.6, linetype = "dashed")
}

.sn_save_plot <- function(p, save_path, save_args) {
  args <- utils::modifyList(list(filename = save_path, plot = p), save_args)
  tryCatch(
    do.call(ggplot2::ggsave, args),
    error = function(e) warning("Could not save the plot: ", e$message, call. = FALSE)
  )
}

# ---------------------------
# Public function
# ---------------------------

scatter_norm <- function(
    data, x, y,
    r_label = TRUE,
    save_path = NULL,
    interactive = FALSE,
    annotate_quadrants = FALSE,
    r_method = c("pearson", "spearman", "kendall"),
    r_digits = 2L,
    r_show_p = FALSE,
    r_show_n = FALSE,
    
    smooth = TRUE,
    smooth_method = c("loess", "lm", "gam", "none"),
    loess_span = NULL,
    loess_family = c("gaussian", "symmetric"),
    smooth_args = list(method = "loess", se = TRUE),
    smooth_group = c("overall", "mapped"),
    
    point_args = list(),
    color = NULL,
    fill = NULL,
    shape = NULL,
    
    ref_lines = TRUE,
    quadrant_cut = c("zero", "median"),
    quadrant_digits = 1L,
    
    theme = ggplot2::theme_minimal(),
    coord_equal = TRUE,
    style = c("default", "clean"),
    
    save_args = list(),
    return = c("plot", "all")
) {
  .sn_assert_df(data)
  
  x_name <- rlang::as_string(rlang::ensym(x))
  y_name <- rlang::as_string(rlang::ensym(y))
  
  .sn_assert_col(data, x_name); .sn_assert_col(data, y_name)
  .sn_assert_numeric(data[[x_name]], x_name)
  .sn_assert_numeric(data[[y_name]], y_name)
  
  r_method      <- match.arg(r_method)
  quadrant_cut  <- match.arg(quadrant_cut)
  smooth_group  <- match.arg(smooth_group)
  smooth_method <- match.arg(smooth_method)
  style         <- match.arg(style)
  return        <- match.arg(return)
  
  color_q <- rlang::enquo(color)
  fill_q  <- rlang::enquo(fill)
  shape_q <- rlang::enquo(shape)
  
  map_names <- .sn_capture_optional_mapping(data, color_q, fill_q, shape_q)
  
  preset <- .sn_apply_style_preset(
    style = style,
    theme = theme,
    point_args = point_args,
    smooth_args = smooth_args,
    fill_mapped = !is.null(map_names$fill)
  )
  theme <- preset$theme
  point_args <- preset$point_args
  smooth_args <- preset$smooth_args
  
  data_complete <- .sn_drop_complete_xy(data, x_name, y_name)
  prep <- .sn_prepare_plot_df(data_complete, x_name, y_name)
  df_plot <- prep$df_plot
  
  cuts <- .sn_quadrant_cut(prep$xz, prep$yz, quadrant_cut)
  
  cor_stats <- .sn_compute_cor_stats(
    prep$x_use, prep$y_use,
    r_method = r_method,
    r_digits = r_digits,
    r_show_p = r_show_p,
    r_show_n = r_show_n
  )
  
  q_stats <- NULL
  if (isTRUE(annotate_quadrants) && nrow(df_plot) >= 4) {
    q_stats <- .sn_quadrant_stats(prep$xz, prep$yz, cuts$cut_x, cuts$cut_y, digits = quadrant_digits)
    pos <- .sn_quadrant_label_positions(prep$xz, prep$yz)
    q_stats$x <- pos$x; q_stats$y <- pos$y
  } else if (isTRUE(annotate_quadrants) && nrow(df_plot) < 4) {
    warning("Not enough points to reliably annotate quadrants (need >= 4).", call. = FALSE)
  }
  
  stats_out <- list(
    n = nrow(df_plot),
    r = cor_stats$r,
    r_method = r_method,
    p_value = cor_stats$p_value,
    quadrant_cut = quadrant_cut,
    cut_x = cuts$cut_x,
    cut_y = cuts$cut_y,
    quadrants = q_stats
  )
  
  mapping <- .sn_build_mapping(map_names, include_text = isTRUE(interactive))
  p <- ggplot2::ggplot(df_plot, mapping)
  
  p <- p + do.call(ggplot2::geom_point, point_args)
  
  smooth_layer_args <- .sn_resolve_smoother(
    smooth = smooth,
    smooth_method = smooth_method,
    smooth_args = smooth_args,
    loess_span = loess_span,
    loess_family = loess_family,
    loess_family_provided = !missing(loess_family),
    style_clean = (style == "clean"),
    smooth_group = smooth_group
  )
  if (!is.null(smooth_layer_args)) {
    p <- p + do.call(ggplot2::geom_smooth, smooth_layer_args)
  }
  
  if (isTRUE(ref_lines)) {
    p <- .sn_add_ref_lines(p, cuts$cut_x, cuts$cut_y)
  }
  
  if (isTRUE(r_label)) {
    r_df <- data.frame(label = cor_stats$label)
    p <- .sn_add_annotation_layer(
      p,
      data = r_df,
      mapping = ggplot2::aes(x = -Inf, y = Inf, label = label),
      interactive = interactive,
      hjust = -0.05, vjust = 1.1, alpha = 0.6
    )
  }
  
  if (!is.null(q_stats) && nrow(q_stats) == 4) {
    p <- .sn_add_annotation_layer(
      p,
      data = q_stats,
      mapping = ggplot2::aes(x = x, y = y, label = label),
      interactive = interactive,
      alpha = 0.5
    )
  }
  
  p <- p +
    ggplot2::labs(
      x = paste0("Rescaled ", x_name, " (z)"),
      y = paste0("Rescaled ", y_name, " (z)")
    ) +
    theme
  
  if (isTRUE(coord_equal)) p <- p + ggplot2::coord_equal()
  
  if (!is.null(save_path)) .sn_save_plot(p, save_path, save_args)
  
  out_plot <- if (isTRUE(interactive)) plotly::ggplotly(p, tooltip = "text") else p
  
  if (return == "all") return(list(plot = out_plot, data = df_plot, stats = stats_out))
  out_plot
}
