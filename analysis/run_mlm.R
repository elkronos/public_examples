# ---- Package helper ----------------------------------------------------------
require_packages <- function(pkgs, install_if_missing = TRUE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) && install_if_missing) {
    install.packages(missing, dependencies = TRUE)
  }
  still_missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(still_missing)) {
    stop("Missing packages: ", paste(still_missing, collapse = ", "),
         ". Install them or set install_if_missing = TRUE.")
  }
  invisible(TRUE)
}

# ---- 1) Numeric descriptives -------------------------------------------------
compute_numeric_descriptives <- function(
    df,
    vars,
    digits = 2,
    na.rm = TRUE
) {
  stopifnot(is.data.frame(df))
  if (length(vars) == 0) stop("vars must contain at least one variable name.")
  missing <- setdiff(vars, names(df))
  if (length(missing)) stop("These vars are not in df: ", paste(missing, collapse = ", "))
  
  out <- lapply(vars, function(v) {
    x <- df[[v]]
    if (!is.numeric(x)) x <- suppressWarnings(as.numeric(x))
    
    n_missing <- sum(is.na(x))
    x2 <- if (na.rm) x[!is.na(x)] else x
    n <- length(x2)
    
    data.frame(
      variable = v,
      n = n,
      missing = n_missing,
      mean = if (n) mean(x2) else NA_real_,
      sd = if (n) stats::sd(x2) else NA_real_,
      median = if (n) stats::median(x2) else NA_real_,
      min = if (n) min(x2) else NA_real_,
      max = if (n) max(x2) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, out)
  
  # rounding
  num_cols <- c("mean", "sd", "median", "min", "max")
  out[num_cols] <- lapply(out[num_cols], function(x) round(x, digits))
  
  out
}

# ---- 2) Categorical descriptives ---------------------------------------------
compute_categorical_descriptives <- function(
    df,
    vars,
    include_na = TRUE,
    digits = 3
) {
  stopifnot(is.data.frame(df))
  if (length(vars) == 0) return(list())
  missing <- setdiff(vars, names(df))
  if (length(missing)) stop("These vars are not in df: ", paste(missing, collapse = ", "))
  
  res <- lapply(vars, function(v) {
    x <- df[[v]]
    if (!is.factor(x)) x <- factor(x, exclude = if (include_na) NULL else NA)
    
    tab <- as.data.frame(table(x, useNA = if (include_na) "ifany" else "no"),
                         stringsAsFactors = FALSE)
    names(tab) <- c("level", "n")
    tab$prop <- round(tab$n / sum(tab$n), digits)
    tab$variable <- v
    tab <- tab[, c("variable", "level", "n", "prop")]
    tab
  })
  names(res) <- vars
  res
}

# ---- 3) Export to Word via officer+flextable ---------------------------------
export_descriptives_docx <- function(
    numeric_desc,
    categorical_desc = NULL,
    path = "descriptives_table.docx",
    table_number = 1,
    title = "Descriptive statistics",
    note = NULL,
    font = "Times New Roman",
    font_size = 12
) {
  require_packages(c("officer", "flextable"))
  
  doc <- officer::read_docx()
  
  # Title block (simple, predictable formatting)
  doc <- officer::body_add_par(doc, paste0("Table ", table_number), style = "Normal")
  doc <- officer::body_add_par(doc, title, style = "Normal")
  
  ft_num <- flextable::flextable(numeric_desc)
  ft_num <- flextable::theme_booktabs(ft_num)
  ft_num <- flextable::font(ft_num, fontname = font, part = "all")
  ft_num <- flextable::fontsize(ft_num, size = font_size, part = "all")
  ft_num <- flextable::autofit(ft_num)
  
  doc <- flextable::body_add_flextable(doc, ft_num)
  
  # Optional categorical sections
  if (!is.null(categorical_desc) && length(categorical_desc)) {
    for (nm in names(categorical_desc)) {
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_par(doc, paste0("Frequencies: ", nm), style = "Normal")
      
      ft_cat <- flextable::flextable(categorical_desc[[nm]])
      ft_cat <- flextable::theme_booktabs(ft_cat)
      ft_cat <- flextable::font(ft_cat, fontname = font, part = "all")
      ft_cat <- flextable::fontsize(ft_cat, size = font_size, part = "all")
      ft_cat <- flextable::autofit(ft_cat)
      
      doc <- flextable::body_add_flextable(doc, ft_cat)
    }
  }
  
  if (!is.null(note)) {
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_par(doc, paste0("Note. ", note), style = "Normal")
  }
  
  print(doc, target = path)
  invisible(path)
}

# ---- 4) Optional: use apaStyle::apa.descriptives() ----------------------------
# This mirrors the function you *tried* to call, but from the correct package.
export_descriptives_apastyle <- function(
    df,
    variables,
    path = "descriptives_table.docx",
    title = "APA Table",
    report = c("M", "SD"),   # avoids the correlation matrix unless you include "r"
    merge = FALSE,
    landscape = FALSE
) {
  # apa.descriptives is from apaStyle, not apaTables. :contentReference[oaicite:2]{index=2}
  require_packages("apaStyle")
  apaStyle::apa.descriptives(
    data = df,
    variables = variables,
    report = report,
    title = title,
    filename = path,
    merge = merge,
    landscape = landscape,
    save = TRUE
  )
  invisible(path)
}
