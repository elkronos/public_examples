#' Workbook Utilities for Styled Excel Exports
#'
#' Functions to create and style Excel workbooks using \pkg{openxlsx}, including
#' worksheet creation with styles, inferred number/date formatting, a summary sheet
#' with hyperlinks, and a top-level constructor to assemble workbooks from data frames.
#'
#' @section Main entry points:
#' * [addStyledTable()] — write a data frame to a worksheet with styles and options
#' * [addSummarySheet()] — add a summary worksheet with per-sheet row/column counts
#' * [createStyledWorkbook()] — build a complete workbook from main and additional data frames
#'
#' @seealso [openxlsx::createWorkbook()], [openxlsx::addWorksheet()], [openxlsx::writeDataTable()]
#' @keywords Excel openxlsx export table formatting workbook
#' @name workbook_utils
NULL

# ---- Dependencies ----
#' @import gt
#' @import openxlsx
NULL

library(gt)
library(openxlsx)

# ---- Helpers ---------------------------------------------------------------

#' Validate an openxlsx Workbook object
#'
#' @param wb An object expected to be of class `"Workbook"` from \pkg{openxlsx}.
#' @return Invisibly returns `NULL`. Errors if `wb` is not a workbook.
#' @keywords internal
validateWorkbook <- function(wb) {
  if (!inherits(wb, "Workbook")) {
    stop("Invalid workbook object. Must be an object of class 'Workbook' from openxlsx.")
  }
}

#' Sanitize a candidate Excel sheet name
#'
#' Ensures the name satisfies Excel constraints (length <= 31, no `: \\ / ? * [ ]`,
#' no trailing apostrophes, not blank).
#'
#' @param x A single character string.
#' @return A safe sheet name string.
#' @keywords internal
.sanitize_sheet_name <- function(x) {
  if (!is.character(x) || length(x) != 1L) stop("sheet_name must be a single string.")
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- gsub("[:\\\\/\\?\\*\\[\\]]", "_", x, perl = TRUE)
  x <- gsub("[[:cntrl:]]", "", x, perl = TRUE)
  x <- substr(x, 1L, 31L)
  x <- sub("'+$", "", x)
  if (identical(tolower(x), "history") || x == "") x <- "Sheet"
  if (!nzchar(gsub("_|\\s", "", x))) x <- "Sheet"
  x
}

#' Ensure a sheet name is unique within a workbook
#'
#' Appends suffixes like `" (1)"` if needed, respecting Excel's 31-character limit.
#'
#' @param wb A \pkg{openxlsx} `Workbook`.
#' @param base_name Candidate base name (already sanitized).
#' @return A unique sheet name.
#' @keywords internal
.ensure_unique_sheet <- function(wb, base_name) {
  existing <- sheets(wb)
  name <- base_name
  i <- 1L
  while (name %in% existing) {
    suffix <- sprintf(" (%d)", i)
    name <- substr(paste0(base_name, suffix), 1L, 31L)
    i <- i + 1L
  }
  name
}

#' Apply inferred numeric/date formats to columns
#'
#' Adds a `0.00` numeric format to numeric columns and a `yyyy-mm-dd` format to
#' `Date`/`POSIXt` columns.
#'
#' @param wb A \pkg{openxlsx} `Workbook`.
#' @param sheet Sheet name.
#' @param df Data frame written starting at `start_row`.
#' @param start_row First data row (default `2L`, assuming row 1 is header).
#' @return Invisibly returns `NULL`.
#' @keywords internal
.apply_inferred_formats <- function(wb, sheet, df, start_row = 2L) {
  is_date <- function(v) inherits(v, c("Date", "POSIXct", "POSIXt"))
  n <- ncol(df)
  end_row <- nrow(df) + start_row - 1L
  if (end_row < start_row) return(invisible(NULL))
  for (j in seq_len(n)) {
    col <- df[[j]]
    if (is.numeric(col)) {
      addStyle(
        wb, sheet,
        style = createStyle(numFmt = "0.00"),
        rows = start_row:end_row, cols = j,
        gridExpand = TRUE, stack = TRUE
      )
    } else if (is_date(col)) {
      addStyle(
        wb, sheet,
        style = createStyle(numFmt = "yyyy-mm-dd"),
        rows = start_row:end_row, cols = j,
        gridExpand = TRUE, stack = TRUE
      )
    }
  }
  invisible(NULL)
}

#' Optionally freeze the header row
#'
#' @param wb A \pkg{openxlsx} `Workbook`.
#' @param sheet Sheet name.
#' @param freeze_header Logical; if `TRUE`, freeze first row.
#' @return Invisibly returns `NULL`.
#' @keywords internal
.maybe_freeze <- function(wb, sheet, freeze_header) {
  if (isTRUE(freeze_header)) freezePane(wb, sheet = sheet, firstActiveRow = 2, firstActiveCol = 1)
}

# ---- Public API ------------------------------------------------------------

#' Add a styled table to a worksheet
#'
#' Writes a data frame to a new worksheet with optional Excel table styling,
#' header/cell styling, inferred number/date formats, column widths, and a frozen header row.
#'
#' @param wb An \pkg{openxlsx} `Workbook` object.
#' @param sheet_name Worksheet name; will be sanitized and uniquified within the workbook.
#' @param data A data frame to write.
#' @param table_style Excel table style name (e.g., `"TableStyleMedium9"`). Used when `as_table = TRUE`.
#' @param header_style An \pkg{openxlsx} style for the header row.
#' @param cell_style An \pkg{openxlsx} style for data cells.
#' @param col_widths Numeric vector of widths, `"auto"` for automatic sizing, or `NULL` to skip.
#' @param apply_header_style Logical; apply `header_style` to the header row.
#' @param apply_cell_style Logical; apply `cell_style` to data rows.
#' @param freeze_header Logical; freeze the header row.
#' @param as_table Logical; write as an Excel table (filters + table style). If `FALSE`, writes plain data with filters.
#' @param apply_inferred_formats Logical; apply simple numeric/date formats by column.
#'
#' @return Invisibly returns the final worksheet name (after sanitization/uniqueness).
#' @examples
#' \dontrun{
#' wb <- openxlsx::createWorkbook()
#' df <- data.frame(A = 1:3, B = as.Date("2024-01-01") + 0:2)
#' addStyledTable(wb, "MySheet", df)
#' openxlsx::saveWorkbook(wb, "example.xlsx", overwrite = TRUE)
#' }
#' @export
addStyledTable <- function(wb, sheet_name, data,
                           table_style = "TableStyleMedium9",
                           header_style = createStyle(fontSize = 12, textDecoration = "bold",
                                                      halign = "center", fgFill = "#DCE6F1",
                                                      border = "TopBottomLeftRight"),
                           cell_style = createStyle(halign = "center", border = "TopBottomLeftRight"),
                           col_widths = "auto",
                           apply_header_style = TRUE,
                           apply_cell_style = TRUE,
                           freeze_header = TRUE,
                           as_table = TRUE,
                           apply_inferred_formats = TRUE) {
  validateWorkbook(wb)
  if (!is.data.frame(data)) stop("data must be a data frame.")
  if (ncol(data) < 1) stop("data must have at least one column.")
  if (!is.character(sheet_name) || length(sheet_name) != 1L || !nzchar(trimws(sheet_name))) {
    stop("sheet_name must be a single string")
  }
  
  sheet_name <- .sanitize_sheet_name(sheet_name)
  sheet_name <- .ensure_unique_sheet(wb, sheet_name)
  
  addWorksheet(wb, sheetName = sheet_name)
  
  if (isTRUE(as_table)) {
    writeDataTable(wb, sheet = sheet_name, x = data, tableStyle = table_style, withFilter = TRUE)
  } else {
    writeData(wb, sheet = sheet_name, x = data, withFilter = TRUE)
  }
  
  if (isTRUE(apply_header_style)) {
    addStyle(wb, sheet = sheet_name, style = header_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  }
  if (isTRUE(apply_cell_style)) {
    addStyle(wb, sheet = sheet_name, style = cell_style,
             rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
  }
  
  if (isTRUE(apply_inferred_formats) && nrow(data) > 0) {
    .apply_inferred_formats(wb, sheet_name, data, start_row = 2L)
  }
  
  if (identical(col_widths, "auto")) {
    setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = "auto")
  } else if (!is.null(col_widths)) {
    setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = col_widths)
  }
  
  .maybe_freeze(wb, sheet_name, freeze_header)
  
  invisible(sheet_name)
}

#' Add a summary worksheet
#'
#' Creates a worksheet summarizing a named list of data frames, including row and column counts,
#' with hyperlinks to each corresponding sheet in the workbook (if present).
#'
#' @param wb An \pkg{openxlsx} `Workbook` object.
#' @param data_frames A non-empty **named** list of data frames; names are expected to match the target sheet names.
#' @param summary_sheet_name Name for the summary sheet.
#' @param table_style Excel table style name for the summary table.
#' @param header_style An \pkg{openxlsx} style for the header row.
#' @param col_widths Numeric vector of widths, `"auto"` for automatic sizing, or `NULL` to skip.
#' @param freeze_header Logical; freeze the header row.
#'
#' @return Invisibly returns the created summary sheet name (may differ after uniqueness resolution).
#' @examples
#' \dontrun{
#' wb <- openxlsx::createWorkbook()
#' addStyledTable(wb, "Sales", data.frame(A=1:3))
#' addStyledTable(wb, "Costs", data.frame(B=letters[1:4]))
#' addSummarySheet(wb, list(Sales=data.frame(A=1:3), Costs=data.frame(B=letters[1:4])))
#' }
#' @export
addSummarySheet <- function(wb, data_frames, summary_sheet_name = "Summary",
                            table_style = "TableStyleLight9",
                            header_style = createStyle(fontSize = 12, textDecoration = "bold",
                                                       halign = "center", fgFill = "#DCE6F1",
                                                       border = "TopBottomLeftRight"),
                            col_widths = "auto",
                            freeze_header = TRUE) {
  validateWorkbook(wb)
  if (!is.list(data_frames) || length(data_frames) == 0)
    stop("data_frames must be a non-empty list of data frames.")
  if (is.null(names(data_frames)) || any(names(data_frames) == ""))
    stop("data_frames must be a named list (names used as sheet labels).")
  
  ssn <- .ensure_unique_sheet(wb, .sanitize_sheet_name(summary_sheet_name))
  
  summaryData <- data.frame(
    Sheet = names(data_frames),
    Rows = vapply(data_frames, function(df) if (is.data.frame(df)) nrow(df) else NA_integer_, integer(1)),
    Columns = vapply(data_frames, function(df) if (is.data.frame(df)) ncol(df) else NA_integer_, integer(1)),
    stringsAsFactors = FALSE
  )
  
  addWorksheet(wb, sheetName = ssn)
  writeDataTable(wb, sheet = ssn, x = summaryData, tableStyle = table_style, withFilter = TRUE)
  addStyle(wb, sheet = ssn, style = header_style, rows = 1, cols = 1:ncol(summaryData), gridExpand = TRUE)
  
  available <- sheets(wb)
  for (i in seq_len(nrow(summaryData))) {
    target <- summaryData$Sheet[i]
    if (target %in% available) {
      safe_target <- gsub("'", "''", target, fixed = TRUE)
      link_formula <- sprintf('=HYPERLINK("#\'%s\'!A1","%s")', safe_target, target)
      writeFormula(wb, ssn, x = link_formula, startCol = 1, startRow = i + 1)
    }
  }
  
  if (identical(col_widths, "auto")) {
    setColWidths(wb, sheet = ssn, cols = 1:ncol(summaryData), widths = "auto")
  } else if (!is.null(col_widths)) {
    setColWidths(wb, sheet = ssn, cols = 1:ncol(summaryData), widths = col_widths)
  }
  
  .maybe_freeze(wb, ssn, freeze_header)
  invisible(ssn)
}

#' Create a styled workbook from data frames
#'
#' Builds an Excel workbook with a main sheet, optional additional sheets, and
#' an optional summary sheet. Styling options for each group can be passed as lists.
#'
#' @param main_data Data frame for the main sheet.
#' @param additional_data_frames Named list of additional data frames.
#' @param summary_sheet_name Name of the summary sheet.
#' @param main_sheet_options List of arguments forwarded to [addStyledTable()] for the main sheet.
#' @param additional_sheet_options List of arguments forwarded to [addStyledTable()] for each additional sheet.
#' @param summary_sheet_options List of arguments forwarded to [addSummarySheet()].
#' @param main_sheet_name Name of the main sheet; defaults to `"Sheet1"`.
#' @param create_summary_if_multiple Logical; if `TRUE`, create a summary when additional sheets are present.
#'
#' @return An \pkg{openxlsx} `Workbook` object.
#' @examples
#' \dontrun{
#' main_df <- data.frame(Name = c("John", "Jane"), Age = c(30, 25))
#' extra <- list(Sheet2 = data.frame(Value = c(1,2,3)))
#' wb <- createStyledWorkbook(main_df, extra, "Summary")
#' openxlsx::saveWorkbook(wb, "export.xlsx", overwrite = TRUE)
#' }
#' @export
createStyledWorkbook <- function(main_data,
                                 additional_data_frames = list(),
                                 summary_sheet_name = "Summary",
                                 main_sheet_options = list(),
                                 additional_sheet_options = list(),
                                 summary_sheet_options = list(),
                                 main_sheet_name = "Sheet1",
                                 create_summary_if_multiple = TRUE) {
  if (!is.data.frame(main_data)) stop("main_data must be a data frame.")
  
  wb <- createWorkbook()
  
  do.call(addStyledTable,
          c(list(wb = wb, sheet_name = main_sheet_name, data = main_data),
            main_sheet_options))
  
  if (length(additional_data_frames) > 0) {
    if (is.null(names(additional_data_frames)) || any(names(additional_data_frames) == ""))
      stop("additional_data_frames must be a named list, where names will be used as sheet names.")
    for (nm in names(additional_data_frames)) {
      do.call(addStyledTable,
              c(list(wb = wb, sheet_name = nm, data = additional_data_frames[[nm]]),
                additional_sheet_options))
    }
  }
  
  if (isTRUE(create_summary_if_multiple) && length(additional_data_frames) > 0) {
    do.call(addSummarySheet,
            c(list(wb = wb, data_frames = additional_data_frames, summary_sheet_name = summary_sheet_name),
              summary_sheet_options))
  }
  
  wb
}

# ---- Example usage (script) -----------------------------------------------

gt_tbl <- gt(data.frame(Name = c("John", "Jane"), Age = c(30, 25)))
main_df <- gt_tbl[["_data"]]

other_data_frames <- list(
  Sheet2 = data.frame(OtherContent = c("Data1", "Data2")),
  Sheet3 = data.frame(MoreContent = c("Data3", "Data4"))
)

custom_main_options <- list(
  table_style = "TableStyleMedium2",
  header_style = createStyle(fontSize = 14, textDecoration = "bold", halign = "center", fgFill = "#FFFFCC"),
  cell_style = createStyle(halign = "left", border = "TopBottomLeftRight"),
  col_widths = "auto",
  apply_header_style = TRUE,
  apply_cell_style = TRUE,
  freeze_header = TRUE,
  as_table = TRUE,
  apply_inferred_formats = TRUE
)

custom_summary_options <- list(
  table_style = "TableStyleLight1",
  header_style = createStyle(fontSize = 12, textDecoration = "bold", halign = "center", fgFill = "#CCFFCC"),
  col_widths = "auto",
  freeze_header = TRUE
)

wb <- createStyledWorkbook(main_df, other_data_frames, summary_sheet_name = "Summary",
                           main_sheet_options = custom_main_options,
                           summary_sheet_options = custom_summary_options)

saveWorkbook(wb, "output_example.xlsx", overwrite = TRUE)

# ---- User Acceptance Tests (optional) -------------------------------------

if (interactive() || Sys.getenv("RUN_UAT") == "true") {
  library(testthat)
  
  test_that("addStyledTable works with valid inputs and custom options", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:3, B = letters[1:3])
    expect_silent(addStyledTable(wb_test, "TestSheet", df_test,
                                 table_style = "TableStyleDark9",
                                 apply_header_style = TRUE,
                                 apply_cell_style = TRUE))
    expect_true("TestSheet" %in% sheets(wb_test))
  })
  
  test_that("addStyledTable uniquifies and sanitizes names", {
    wb_test <- createWorkbook()
    df <- data.frame(A=1:2)
    nm <- "Bad/Name:*?:"
    addStyledTable(wb_test, nm, df)
    addStyledTable(wb_test, nm, df)
    shs <- sheets(wb_test)
    expect_equal(length(shs), 2L)
    expect_true(any(grepl("^Bad_Name", shs)))
  })
  
  test_that("addStyledTable can skip cell styling for performance", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:1000, B = rnorm(1000))
    expect_silent(addStyledTable(wb_test, "BigDataSheet", df_test,
                                 apply_cell_style = FALSE, as_table = FALSE))
    expect_true("BigDataSheet" %in% sheets(wb_test))
  })
  
  test_that("addStyledTable errors with non-data.frame input", {
    wb_test <- createWorkbook()
    expect_error(addStyledTable(wb_test, "TestSheet", list(a = 1, b = 2)),
                 "data must be a data frame")
  })
  
  test_that("addStyledTable errors with invalid sheet name", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:3)
    expect_error(addStyledTable(wb_test, "", df_test),
                 "sheet_name must be a single string")
    expect_error(addStyledTable(wb_test, 123, df_test),
                 "sheet_name must be a single string")
  })
  
  test_that("addSummarySheet works with valid inputs and custom options", {
    wb_test <- createWorkbook()
    df1 <- data.frame(X = 1:2)
    df2 <- data.frame(Y = letters[1:3])
    additional_list <- list(SheetA = df1, SheetB = df2)
    expect_silent(addSummarySheet(wb_test, additional_list, "MySummary",
                                  table_style = "TableStyleLight2"))
    expect_true("MySummary" %in% sheets(wb_test))
  })
  
  test_that("addSummarySheet errors with invalid data_frames input", {
    wb_test <- createWorkbook()
    expect_error(addSummarySheet(wb_test, "not_a_list", "MySummary"),
                 "data_frames must be a non-empty list")
  })
  
  test_that("createStyledWorkbook works with valid main_data and additional_data_frames with custom options", {
    main_df <- data.frame(Col1 = c(1, 2))
    additional_data <- list(
      Extra1 = data.frame(A = 1:3),
      Extra2 = data.frame(B = letters[1:2])
    )
    wb_new <- createStyledWorkbook(main_df, additional_data, "Summary",
                                   main_sheet_options = list(table_style = "TableStyleMedium4",
                                                             apply_cell_style = TRUE),
                                   additional_sheet_options = list(apply_cell_style = FALSE),
                                   summary_sheet_options = list(table_style = "TableStyleLight3"))
    expect_true(all(c("Sheet1","Extra1","Extra2","Summary") %in% sheets(wb_new)))
  })
  
  test_that("createStyledWorkbook errors with invalid main_data", {
    expect_error(createStyledWorkbook("not_a_dataframe", list()),
                 "main_data must be a data frame")
  })
  
  test_that("createStyledWorkbook errors with unnamed additional_data_frames", {
    main_df <- data.frame(Col1 = c(1, 2))
    unnamed_list <- list(data.frame(A = 1:3))
    expect_error(createStyledWorkbook(main_df, unnamed_list, "Summary"),
                 "additional_data_frames must be a named list")
  })
  
  message("All UAT tests passed successfully.")
}
