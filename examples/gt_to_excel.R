# Load libraries
library(gt)
library(openxlsx)

# Create a gt table
gt_tbl <- gt(data.frame(Name = c("John", "Jane"), Age = c(30, 25)))

# Convert gt table to a data frame
df <- gt_tbl$`_data`

# Create a new workbook
wb <- createWorkbook()

# Function to add a styled table to a worksheet with additional enhancements
addStyledTable <- function(wb, sheet_name, data) {
  addWorksheet(wb, sheet_name)
  writeDataTable(wb, sheet = sheet_name, x = data, tableStyle = "TableStyleMedium9")
  
  # Apply header style
  headerStyle <- createStyle(fontSize = 12, textDecoration = "bold", halign = "center", fgFill = "#DCE6F1", border = "TopBottomLeftRight")
  addStyle(wb, sheet = sheet_name, style = headerStyle, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
  
  # Apply cell style
  cellStyle <- createStyle(halign = "center", border = "TopBottomLeftRight")
  addStyle(wb, sheet = sheet_name, style = cellStyle, rows = 2:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
  
  # Set dynamic column widths
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(data), widths = "auto")
}

# Add the first sheet with the gt table
addStyledTable(wb, "Sheet1", df)

# Additional data frames to add to new sheets
other_data_frames <- list(
  Sheet2 = data.frame(OtherContent = c("Data1", "Data2")),
  Sheet3 = data.frame(MoreContent = c("Data3", "Data4"))
)

# Loop through additional data frames and add them as new sheets
for (sheet_name in names(other_data_frames)) {
  addStyledTable(wb, sheet_name, other_data_frames[[sheet_name]])
}

# Add a summary sheet
addWorksheet(wb, "Summary")
summaryData <- data.frame(
  Sheet = names(other_data_frames),
  Rows = sapply(other_data_frames, nrow),
  Columns = sapply(other_data_frames, ncol)
)
writeDataTable(wb, sheet = "Summary", x = summaryData, tableStyle = "TableStyleLight9")
addStyle(wb, sheet = "Summary", style = createStyle(fontSize = 12, textDecoration = "bold", halign = "center", fgFill = "#DCE6F1", border = "TopBottomLeftRight"), rows = 1, cols = 1:ncol(summaryData), gridExpand = TRUE)
setColWidths(wb, sheet = "Summary", cols = 1:ncol(summaryData), widths = "auto")

# Save the updated workbook
saveWorkbook(wb, "output_example.xlsx", overwrite = TRUE)
