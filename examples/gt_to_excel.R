# =====================
# Workbook Creation Module
# =====================

# Load required libraries
library(gt)
library(openxlsx)

# ---------------------
# Helper: Validate Workbook Object
# ---------------------
validateWorkbook <- function(wb) {
  if (!inherits(wb, "Workbook")) {
    stop("Invalid workbook object. Must be an object of class 'Workbook' from openxlsx.")
  }
}

# ---------------------
# Function: addStyledTable
# ---------------------
#' Add a Styled Table to a Worksheet
#'
#' This function adds a worksheet to the given workbook, writes the provided
#' data frame as a styled table, and optionally applies header and cell styles.
#' Disabling cell styling can improve performance on large datasets.
#'
#' @param wb An openxlsx Workbook object.
#' @param sheet_name A non-empty string specifying the worksheet name.
#' @param data A non-empty data frame to be written to the worksheet.
#' @param table_style A string defining the table style. Default: "TableStyleMedium9".
#' @param header_style An openxlsx style object for header cells.
#'   Default is a bold, centered style with light blue fill.
#' @param cell_style An openxlsx style object for data cells.
#'   Default is a centered style with borders.
#' @param col_widths Either a numeric vector of widths or "auto" for automatic adjustment.
#'   Default: "auto".
#' @param apply_header_style Logical. If TRUE (default) the header style is applied.
#' @param apply_cell_style Logical. If TRUE (default) the cell style is applied.
#'   For big data, consider setting this to FALSE.
#' @return Invisibly returns NULL. Modifies the workbook in place.
#' @examples
#' wb <- createWorkbook()
#' df <- data.frame(A = 1:3, B = letters[1:3])
#' addStyledTable(wb, "MySheet", df)
addStyledTable <- function(wb, sheet_name, data,
                           table_style = "TableStyleMedium9",
                           header_style = createStyle(fontSize = 12, textDecoration = "bold",
                                                      halign = "center", fgFill = "#DCE6F1",
                                                      border = "TopBottomLeftRight"),
                           cell_style = createStyle(halign = "center", border = "TopBottomLeftRight"),
                           col_widths = "auto",
                           apply_header_style = TRUE,
                           apply_cell_style = TRUE) {
  
  # Parameter validation
  validateWorkbook(wb)
  
  if (!is.character(sheet_name) || length(sheet_name) != 1 || nchar(sheet_name) == 0) {
    stop("sheet_name must be a non-empty string.")
  }
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (ncol(data) < 1) {
    stop("data must have at least one column.")
  }
  
  # Check if the sheet already exists (openxlsx stores sheet names in wb$sheet_names)
  if (sheet_name %in% wb$sheet_names) {
    stop(paste("Sheet", sheet_name, "already exists in the workbook."))
  }
  
  # Add worksheet and write the data table
  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet = sheet_name, x = data, tableStyle = table_style)
  
  # Optionally apply header style
  if (apply_header_style) {
    addStyle(wb, sheet = sheet_name, style = header_style,
             rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  }
  
  # Optionally apply cell style (data rows)
  if (apply_cell_style) {
    addStyle(wb, sheet = sheet_name, style = cell_style,
             rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
  }
  
  # Set dynamic column widths
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = col_widths)
}

# ---------------------
# Function: addSummarySheet
# ---------------------
#' Add a Summary Sheet to the Workbook
#'
#' This function creates a summary worksheet based on a named list of data frames.
#' It reports the number of rows and columns for each sheet.
#'
#' @param wb An openxlsx Workbook object.
#' @param data_frames A non-empty named list of data frames.
#' @param summary_sheet_name A non-empty string for the summary sheet name.
#' @param table_style A string defining the table style for the summary sheet.
#'   Default: "TableStyleLight9".
#' @param header_style An openxlsx style object for header cells.
#'   Default is a bold, centered style with light blue fill.
#' @param col_widths Either a numeric vector of widths or "auto". Default: "auto".
#' @return Invisibly returns NULL. Modifies the workbook in place.
#' @examples
#' wb <- createWorkbook()
#' data_list <- list(SheetA = data.frame(X = 1:2), SheetB = data.frame(Y = 1:3))
#' addSummarySheet(wb, data_list, "Summary")
addSummarySheet <- function(wb, data_frames, summary_sheet_name = "Summary",
                            table_style = "TableStyleLight9",
                            header_style = createStyle(fontSize = 12, textDecoration = "bold",
                                                       halign = "center", fgFill = "#DCE6F1",
                                                       border = "TopBottomLeftRight"),
                            col_widths = "auto") {
  validateWorkbook(wb)
  
  if (!is.list(data_frames) || length(data_frames) == 0) {
    stop("data_frames must be a non-empty list of data frames.")
  }
  
  if (!is.character(summary_sheet_name) || length(summary_sheet_name) != 1 || nchar(summary_sheet_name) == 0) {
    stop("summary_sheet_name must be a non-empty string.")
  }
  
  if (summary_sheet_name %in% wb$sheet_names) {
    stop(paste("Sheet", summary_sheet_name, "already exists in the workbook."))
  }
  
  # Create summary data frame
  summaryData <- data.frame(
    Sheet = names(data_frames),
    Rows = sapply(data_frames, function(df) {
      if (!is.data.frame(df)) return(NA)
      nrow(df)
    }),
    Columns = sapply(data_frames, function(df) {
      if (!is.data.frame(df)) return(NA)
      ncol(df)
    }),
    stringsAsFactors = FALSE
  )
  
  # Add summary worksheet and write data table
  addWorksheet(wb, summary_sheet_name)
  writeDataTable(wb, sheet = summary_sheet_name, x = summaryData, tableStyle = table_style)
  
  # Apply header style
  addStyle(wb, sheet = summary_sheet_name, style = header_style,
           rows = 1, cols = 1:ncol(summaryData), gridExpand = TRUE)
  
  # Set dynamic column widths
  setColWidths(wb, sheet = summary_sheet_name, cols = 1:ncol(summaryData), widths = col_widths)
}

# ---------------------
# Function: createStyledWorkbook
# ---------------------
#' Create a Styled Workbook from Data Frames
#'
#' This function creates an Excel workbook that includes:
#' - A main sheet ("Sheet1") from a primary data frame.
#' - Additional sheets from a named list of data frames.
#' - A summary sheet (if additional sheets are provided) that lists the sheet names along
#'   with their number of rows and columns.
#'
#' Optional styling parameters for each type of sheet can be passed via option lists:
#'   - main_sheet_options: options for the main sheet (e.g., table_style, header_style,
#'     cell_style, col_widths, apply_header_style, apply_cell_style)
#'   - additional_sheet_options: options for additional sheets (same keys as above)
#'   - summary_sheet_options: options for the summary sheet (e.g., table_style, header_style, col_widths)
#'
#' @param main_data A data frame for the main sheet.
#' @param additional_data_frames A named list of data frames to be added as additional sheets.
#' @param summary_sheet_name A non-empty string for the summary sheet name.
#' @param main_sheet_options A list of optional parameters to override defaults for the main sheet.
#' @param additional_sheet_options A list of optional parameters to override defaults for additional sheets.
#' @param summary_sheet_options A list of optional parameters to override defaults for the summary sheet.
#' @return An openxlsx Workbook object.
#' @examples
#' main_df <- data.frame(Name = c("John", "Jane"), Age = c(30, 25))
#' extra <- list(Sheet2 = data.frame(Value = c(1,2,3)))
#' wb <- createStyledWorkbook(main_df, extra, "Summary")
createStyledWorkbook <- function(main_data,
                                 additional_data_frames = list(),
                                 summary_sheet_name = "Summary",
                                 main_sheet_options = list(),
                                 additional_sheet_options = list(),
                                 summary_sheet_options = list()) {
  if (!is.data.frame(main_data)) {
    stop("main_data must be a data frame.")
  }
  
  wb <- createWorkbook()
  
  # Add the main data sheet as "Sheet1"
  do.call(addStyledTable,
          c(list(wb = wb, sheet_name = "Sheet1", data = main_data),
            main_sheet_options))
  
  # Validate and add additional data frames
  if (length(additional_data_frames) > 0) {
    if (is.null(names(additional_data_frames)) || any(names(additional_data_frames) == "")) {
      stop("additional_data_frames must be a named list, where names will be used as sheet names.")
    }
    
    for (sheet_name in names(additional_data_frames)) {
      do.call(addStyledTable,
              c(list(wb = wb, sheet_name = sheet_name, data = additional_data_frames[[sheet_name]]),
                additional_sheet_options))
    }
  }
  
  # Add summary sheet if additional sheets were provided
  if (length(additional_data_frames) > 0) {
    do.call(addSummarySheet,
            c(list(wb = wb, data_frames = additional_data_frames, summary_sheet_name = summary_sheet_name),
              summary_sheet_options))
  }
  
  return(wb)
}

# ---------------------
# Example Usage
# ---------------------

# Create a gt table and convert it to a data frame (the "_data" slot holds the underlying data)
gt_tbl <- gt(data.frame(Name = c("John", "Jane"), Age = c(30, 25)))
main_df <- gt_tbl[["_data"]]

# Additional data frames to be added as separate sheets
other_data_frames <- list(
  Sheet2 = data.frame(OtherContent = c("Data1", "Data2")),
  Sheet3 = data.frame(MoreContent = c("Data3", "Data4"))
)

# Custom style options (if desired)
custom_main_options <- list(
  table_style = "TableStyleMedium2",
  header_style = createStyle(fontSize = 14, textDecoration = "bold", halign = "center", fgFill = "#FFFFCC"),
  cell_style = createStyle(halign = "left", border = "TopBottomLeftRight"),
  col_widths = "auto",
  apply_header_style = TRUE,
  apply_cell_style = TRUE  # Set to FALSE for very large datasets if needed
)

custom_summary_options <- list(
  table_style = "TableStyleLight1",
  header_style = createStyle(fontSize = 12, textDecoration = "bold", halign = "center", fgFill = "#CCFFCC"),
  col_widths = "auto"
)

# Create the workbook using our modular and flexible functions
wb <- createStyledWorkbook(main_df, other_data_frames, summary_sheet_name = "Summary",
                           main_sheet_options = custom_main_options,
                           summary_sheet_options = custom_summary_options)

# Save the workbook (the file "output_example.xlsx" will be created/overwritten)
saveWorkbook(wb, "output_example.xlsx", overwrite = TRUE)

# =====================
# User Acceptance Tests (UAT)
# =====================

# The following UAT tests use the 'testthat' package.
# To run them, ensure that you are in an interactive session or set the environment variable RUN_UAT=true.
if (interactive() || Sys.getenv("RUN_UAT") == "true") {
  
  library(testthat)
  
  test_that("addStyledTable works with valid inputs and custom options", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:3, B = letters[1:3])
    custom_options <- list(table_style = "TableStyleDark9",
                           apply_header_style = TRUE,
                           apply_cell_style = TRUE)
    expect_silent(addStyledTable(wb_test, "TestSheet", df_test,
                                 table_style = custom_options$table_style,
                                 apply_header_style = custom_options$apply_header_style,
                                 apply_cell_style = custom_options$apply_cell_style))
    expect_true("TestSheet" %in% wb_test$sheet_names)
  })
  
  test_that("addStyledTable can skip cell styling for performance", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:1000, B = rnorm(1000))
    expect_silent(addStyledTable(wb_test, "BigDataSheet", df_test,
                                 apply_cell_style = FALSE))
    expect_true("BigDataSheet" %in% wb_test$sheet_names)
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
                 "sheet_name must be a non-empty string")
    expect_error(addStyledTable(wb_test, 123, df_test),
                 "sheet_name must be a non-empty string")
  })
  
  test_that("addStyledTable errors when sheet already exists", {
    wb_test <- createWorkbook()
    df_test <- data.frame(A = 1:3)
    addStyledTable(wb_test, "DuplicateSheet", df_test)
    expect_error(addStyledTable(wb_test, "DuplicateSheet", df_test),
                 "already exists in the workbook")
  })
  
  test_that("addSummarySheet works with valid inputs and custom options", {
    wb_test <- createWorkbook()
    df1 <- data.frame(X = 1:2)
    df2 <- data.frame(Y = letters[1:3])
    additional_list <- list(SheetA = df1, SheetB = df2)
    custom_summary <- list(table_style = "TableStyleLight2")
    expect_silent(addSummarySheet(wb_test, additional_list, "MySummary",
                                  table_style = custom_summary$table_style))
    expect_true("MySummary" %in% wb_test$sheet_names)
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
    main_opts <- list(table_style = "TableStyleMedium4",
                      apply_cell_style = TRUE)
    add_opts <- list(apply_cell_style = FALSE)  # Disabling cell styling for additional sheets
    sum_opts <- list(table_style = "TableStyleLight3")
    wb_new <- createStyledWorkbook(main_df, additional_data, "Summary",
                                   main_sheet_options = main_opts,
                                   additional_sheet_options = add_opts,
                                   summary_sheet_options = sum_opts)
    expect_true("Sheet1" %in% wb_new$sheet_names)
    expect_true("Extra1" %in% wb_new$sheet_names)
    expect_true("Extra2" %in% wb_new$sheet_names)
    expect_true("Summary" %in% wb_new$sheet_names)
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
