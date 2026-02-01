#' Workbook Utilities for Styled Excel Exports (openxlsx)
#'
#' Main entry points:
#' * addStyledTable() — write a data frame to a worksheet with styles and options
#' * addSummarySheet() — add a summary worksheet with per-sheet row/column counts + hyperlinks
#' * createStyledWorkbook() — build a complete workbook from main and additional data frames
#'
#' @keywords Excel openxlsx export table formatting workbook
NULL

# For package code, prefer Imports + openxlsx:: prefixes (no library()).
# For script usage, load openxlsx externally if desired.

# ---- Internal helpers ------------------------------------------------------

.validate_workbook <- function(wb) {
  if (!inherits(wb, "Workbook")) {
    stop("Invalid workbook object. Must be an object of class 'Workbook' from openxlsx.", call. = FALSE)
  }
  invisible(NULL)
}

.assert_single_string <- function(x, arg = "value") {
  if (!is.character(x) || length(x) != 1L || !nzchar(trimws(x))) {
    stop(sprintf("%s must be a single non-empty string.", arg), call. = FALSE)
  }
  invisible(NULL)
}

.sanitize_sheet_name <- function(x) {
  .assert_single_string(x, "sheet_name")
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("[:\\\\/\\?\\*\\[\\]]", "_", x, perl = TRUE)
  x <- gsub("[[:cntrl:]]", "", x, perl = TRUE)
  x <- substr(x, 1L, 31L)
  x <- sub("'+$", "", x)
  if (!nzchar(x) || !nzchar(gsub("_|\\s", "", x))) x <- "Sheet"
  x
}

# Generate a unique sheet name while respecting Excel's 31-character limit.
# Suffix space is reserved so "(n)" can be appended without exceeding the limit.
.ensure_unique_sheet <- function(wb, base_name) {
  existing <- names(wb)
  base_name <- substr(base_name, 1L, 31L)
  
  name <- base_name
  i <- 1L
  while (name %in% existing) {
    suffix <- sprintf(" (%d)", i)
    max_base <- max(1L, 31L - nchar(suffix))
    name <- paste0(substr(base_name, 1L, max_base), suffix)
    i <- i + 1L
  }
  name
}

.resolve_sheet_name <- function(wb, desired_name) {
  nm <- .sanitize_sheet_name(desired_name)
  .ensure_unique_sheet(wb, nm)
}

.safe_seq_rows <- function(from, to) {
  if (is.na(from) || is.na(to) || to < from) integer(0) else seq.int(from, to)
}

# Apply freeze panes relative to the table's header row and start column.
# When freezing the header only, no columns are frozen.
.maybe_freeze <- function(wb, sheet, header_row, start_col, freeze_header, freeze_first_col = FALSE) {
  if (!isTRUE(freeze_header) && !isTRUE(freeze_first_col)) return(invisible(NULL))
  
  firstActiveRow <- if (isTRUE(freeze_header)) header_row + 1L else 1L
  firstActiveCol <- if (isTRUE(freeze_first_col)) start_col + 1L else 1L
  
  openxlsx::freezePane(
    wb, sheet = sheet,
    firstActiveRow = firstActiveRow,
    firstActiveCol = firstActiveCol
  )
  invisible(NULL)
}

.set_col_widths <- function(wb, sheet, cols, col_widths) {
  if (is.null(col_widths)) return(invisible(NULL))
  if (identical(col_widths, "auto")) {
    openxlsx::setColWidths(wb, sheet = sheet, cols = cols, widths = "auto")
    return(invisible(NULL))
  }
  openxlsx::setColWidths(wb, sheet = sheet, cols = cols, widths = col_widths)
  invisible(NULL)
}

.apply_inferred_formats <- function(wb, sheet, df, data_rows, sheet_cols,
                                    number_fmt = "0.00",
                                    date_fmt = "yyyy-mm-dd",
                                    datetime_fmt = "yyyy-mm-dd hh:mm") {
  if (length(data_rows) == 0L) return(invisible(NULL))
  if (!is.data.frame(df) || ncol(df) == 0L) return(invisible(NULL))
  
  # Detect column types once
  is_posix <- vapply(df, function(x) inherits(x, c("POSIXct", "POSIXlt", "POSIXt")), logical(1))
  is_date  <- vapply(df, function(x) inherits(x, "Date"), logical(1)) & !is_posix
  is_num   <- vapply(df, is.numeric, logical(1)) & !is_date & !is_posix
  
  # Create styles once
  style_num  <- openxlsx::createStyle(numFmt = number_fmt)
  style_date <- openxlsx::createStyle(numFmt = date_fmt)
  style_dt   <- openxlsx::createStyle(numFmt = datetime_fmt)
  
  if (any(is_num)) {
    openxlsx::addStyle(
      wb, sheet,
      style = style_num,
      rows = data_rows,
      cols = sheet_cols[which(is_num)],
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  if (any(is_date)) {
    openxlsx::addStyle(
      wb, sheet,
      style = style_date,
      rows = data_rows,
      cols = sheet_cols[which(is_date)],
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  if (any(is_posix)) {
    openxlsx::addStyle(
      wb, sheet,
      style = style_dt,
      rows = data_rows,
      cols = sheet_cols[which(is_posix)],
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  invisible(NULL)
}

# ---- Public API ------------------------------------------------------------

#' Add a styled table to a worksheet
#'
#' @param wb openxlsx Workbook
#' @param sheet_name Desired sheet name (sanitized + uniquified)
#' @param data data.frame
#' @param start_row,start_col Where to write the table (defaults 1,1)
#' @param overwrite_sheet If TRUE and sheet exists, it will be replaced
#' @param as_table If TRUE uses writeDataTable; otherwise writeData (still with filters)
#' @param table_style Excel table style (when as_table = TRUE)
#' @param header_style,cell_style openxlsx styles
#' @param apply_header_style,apply_cell_style Whether to apply header/body styles
#' @param col_widths "auto", numeric vector, or NULL
#' @param freeze_header Freeze the header row for this table
#' @param freeze_first_col Freeze the first column (useful for wide tables)
#' @param apply_inferred_formats Apply number/date/datetime formats inferred from column classes
#' @param number_fmt,date_fmt,datetime_fmt Formats used when apply_inferred_formats = TRUE
#'
#' @return Invisibly returns the final sheet name
#' @export
addStyledTable <- function(wb, sheet_name, data,
                           start_row = 1L,
                           start_col = 1L,
                           overwrite_sheet = FALSE,
                           table_style = "TableStyleMedium9",
                           header_style = openxlsx::createStyle(
                             fontSize = 12, textDecoration = "bold",
                             halign = "center", fgFill = "#DCE6F1",
                             border = "TopBottomLeftRight"
                           ),
                           cell_style = openxlsx::createStyle(
                             halign = "center",
                             border = "TopBottomLeftRight"
                           ),
                           col_widths = "auto",
                           apply_header_style = TRUE,
                           apply_cell_style = TRUE,
                           freeze_header = TRUE,
                           freeze_first_col = FALSE,
                           as_table = TRUE,
                           apply_inferred_formats = TRUE,
                           number_fmt = "0.00",
                           date_fmt = "yyyy-mm-dd",
                           datetime_fmt = "yyyy-mm-dd hh:mm") {
  .validate_workbook(wb)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  if (ncol(data) < 1) stop("data must have at least one column.", call. = FALSE)
  .assert_single_string(sheet_name, "sheet_name")
  
  desired_sanitized <- .sanitize_sheet_name(sheet_name)
  existing <- names(wb)
  
  # Determine final sheet name with overwrite behavior.
  if (isTRUE(overwrite_sheet) && desired_sanitized %in% existing) {
    openxlsx::removeWorksheet(wb, desired_sanitized)
    final_name <- desired_sanitized
  } else {
    final_name <- .ensure_unique_sheet(wb, desired_sanitized)
  }
  
  openxlsx::addWorksheet(wb, sheetName = final_name)
  
  if (isTRUE(as_table)) {
    openxlsx::writeDataTable(
      wb, sheet = final_name, x = data,
      startRow = start_row, startCol = start_col,
      tableStyle = table_style, withFilter = TRUE
    )
  } else {
    openxlsx::writeData(
      wb, sheet = final_name, x = data,
      startRow = start_row, startCol = start_col,
      withFilter = TRUE
    )
  }
  
  header_row  <- start_row
  header_cols <- start_col + seq_len(ncol(data)) - 1L
  data_rows   <- .safe_seq_rows(start_row + 1L, start_row + nrow(data))
  sheet_cols  <- header_cols
  
  if (isTRUE(apply_header_style)) {
    openxlsx::addStyle(
      wb, sheet = final_name, style = header_style,
      rows = header_row, cols = header_cols,
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  if (isTRUE(apply_cell_style) && length(data_rows) > 0L) {
    openxlsx::addStyle(
      wb, sheet = final_name, style = cell_style,
      rows = data_rows, cols = header_cols,
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  if (isTRUE(apply_inferred_formats)) {
    .apply_inferred_formats(
      wb, final_name, data,
      data_rows = data_rows,
      sheet_cols = sheet_cols,
      number_fmt = number_fmt,
      date_fmt = date_fmt,
      datetime_fmt = datetime_fmt
    )
  }
  
  .set_col_widths(wb, final_name, cols = header_cols, col_widths = col_widths)
  .maybe_freeze(
    wb, final_name,
    header_row = header_row,
    start_col = start_col,
    freeze_header = freeze_header,
    freeze_first_col = freeze_first_col
  )
  
  invisible(final_name)
}

#' Add a summary worksheet
#'
#' @param wb openxlsx Workbook
#' @param data_frames Named list of data frames (labels for summary)
#' @param sheet_map Optional named character vector mapping labels -> actual sheet names.
#'   This is useful when sheet names were sanitized/uniquified.
#' @param summary_sheet_name Summary sheet name (sanitized + uniquified)
#' @export
addSummarySheet <- function(wb, data_frames,
                            sheet_map = NULL,
                            summary_sheet_name = "Summary",
                            table_style = "TableStyleLight9",
                            header_style = openxlsx::createStyle(
                              fontSize = 12, textDecoration = "bold",
                              halign = "center", fgFill = "#DCE6F1",
                              border = "TopBottomLeftRight"
                            ),
                            link_style = openxlsx::createStyle(
                              fontColour = "#0563C1",
                              textDecoration = "underline"
                            ),
                            col_widths = "auto",
                            freeze_header = TRUE) {
  .validate_workbook(wb)
  if (!is.list(data_frames) || length(data_frames) == 0L) {
    stop("data_frames must be a non-empty list of data frames.", call. = FALSE)
  }
  if (is.null(names(data_frames)) || any(names(data_frames) == "")) {
    stop("data_frames must be a named list (names used as labels).", call. = FALSE)
  }
  
  ssn <- .resolve_sheet_name(wb, summary_sheet_name)
  openxlsx::addWorksheet(wb, sheetName = ssn)
  
  summaryData <- data.frame(
    Sheet = names(data_frames),
    Rows = vapply(data_frames, function(df) if (is.data.frame(df)) nrow(df) else NA_integer_, integer(1)),
    Columns = vapply(data_frames, function(df) if (is.data.frame(df)) ncol(df) else NA_integer_, integer(1)),
    stringsAsFactors = FALSE
  )
  
  openxlsx::writeDataTable(
    wb, sheet = ssn, x = summaryData,
    tableStyle = table_style, withFilter = TRUE
  )
  openxlsx::addStyle(
    wb, sheet = ssn, style = header_style,
    rows = 1, cols = 1:ncol(summaryData),
    gridExpand = TRUE, stack = TRUE
  )
  
  available <- names(wb)
  
  # Determine actual targets for each label.
  resolve_target <- function(label) {
    if (!is.null(sheet_map) && label %in% names(sheet_map)) return(sheet_map[[label]])
    if (label %in% available) return(label)
    s <- .sanitize_sheet_name(label)
    if (s %in% available) return(s)
    NA_character_
  }
  
  for (i in seq_len(nrow(summaryData))) {
    label <- summaryData$Sheet[i]
    target <- resolve_target(label)
    if (!is.na(target) && target %in% available) {
      safe_target <- gsub("'", "''", target, fixed = TRUE)
      safe_label  <- gsub("\"", "\"\"", label, fixed = TRUE)
      link_formula <- sprintf('=HYPERLINK("#\'%s\'!A1","%s")', safe_target, safe_label)
      openxlsx::writeFormula(wb, ssn, x = link_formula, startCol = 1, startRow = i + 1L)
      openxlsx::addStyle(wb, ssn, style = link_style, rows = i + 1L, cols = 1, stack = TRUE)
    }
  }
  
  .set_col_widths(wb, ssn, cols = 1:ncol(summaryData), col_widths = col_widths)
  .maybe_freeze(wb, ssn, header_row = 1L, start_col = 1L,
                freeze_header = freeze_header, freeze_first_col = FALSE)
  
  invisible(ssn)
}

#' Create a styled workbook from data frames
#'
#' Builds an Excel workbook with a main sheet, optional additional sheets, and
#' an optional summary sheet.
#'
#' Notes:
#' - Attaches a 'sheet_map' attribute (label -> actual sheet name) for hyperlink resolution.
#' - Can include the main sheet in the summary list when requested.
#'
#' @param main_data data.frame
#' @param additional_data_frames named list of data.frames
#' @param include_main_in_summary logical
#' @return Workbook (with attribute 'sheet_map')
#' @export
createStyledWorkbook <- function(main_data,
                                 additional_data_frames = list(),
                                 summary_sheet_name = "Summary",
                                 main_sheet_options = list(),
                                 additional_sheet_options = list(),
                                 summary_sheet_options = list(),
                                 main_sheet_name = "Sheet1",
                                 create_summary_if_multiple = TRUE,
                                 include_main_in_summary = TRUE) {
  if (!is.data.frame(main_data)) stop("main_data must be a data frame.", call. = FALSE)
  if (length(additional_data_frames) > 0L) {
    if (is.null(names(additional_data_frames)) || any(names(additional_data_frames) == "")) {
      stop("additional_data_frames must be a named list, where names will be used as sheet labels.", call. = FALSE)
    }
  }
  
  wb <- openxlsx::createWorkbook()
  
  # Track label -> actual sheet name for hyperlink targets.
  sheet_map <- character(0)
  
  main_actual <- do.call(
    addStyledTable,
    c(list(wb = wb, sheet_name = main_sheet_name, data = main_data),
      main_sheet_options)
  )
  sheet_map[main_sheet_name] <- main_actual
  
  if (length(additional_data_frames) > 0L) {
    # Supports either:
    # - a single option list applied to all additional sheets, OR
    # - a named list of option lists per sheet label (each element is a list of args).
    is_per_sheet_opts <- is.list(additional_sheet_options) &&
      length(additional_sheet_options) > 0L &&
      !is.null(names(additional_sheet_options)) &&
      all(nzchar(names(additional_sheet_options))) &&
      all(vapply(additional_sheet_options, is.list, logical(1)))
    
    for (label in names(additional_data_frames)) {
      opts <- if (is_per_sheet_opts) {
        if (!is.null(additional_sheet_options[[label]])) additional_sheet_options[[label]] else list()
      } else {
        additional_sheet_options
      }
      
      actual <- do.call(
        addStyledTable,
        c(list(wb = wb, sheet_name = label, data = additional_data_frames[[label]]),
          opts)
      )
      sheet_map[label] <- actual
    }
  }
  
  if (isTRUE(create_summary_if_multiple) && length(additional_data_frames) > 0L) {
    summary_list <- additional_data_frames
    if (isTRUE(include_main_in_summary)) {
      summary_list <- c(setNames(list(main_data), main_sheet_name), summary_list)
    }
    
    do.call(
      addSummarySheet,
      c(list(
        wb = wb,
        data_frames = summary_list,
        sheet_map = sheet_map,
        summary_sheet_name = summary_sheet_name
      ), summary_sheet_options)
    )
  }
  
  attr(wb, "sheet_map") <- sheet_map
  wb
}
