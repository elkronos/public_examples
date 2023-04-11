#' Install required packages if not installed
#'
#' @param packages A character vector of package names to be installed.
#'
#' @return None.
install_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
}

install_packages(c("tesseract", "pdftools", "magick", "parallel", "progress"))

# Load libraries
library(tesseract)
library(pdftools)
library(magick)
library(parallel)
library(progress)

#' OCR function to parse text from images and PDFs
#'
#' @param input_file Character string of file path or magick-image object to be parsed.
#' @param output_file Character string of file path to save the extracted text.
#'
#' @return None.
#' @examples
#' \dontrun{
#' ocr_parse("sample.pdf", "output.txt")
#' ocr_parse("sample_image.png", "output.txt")
#' }
ocr_parse <- function(input_file, output_file) {
  if (is.character(input_file)) {
    if (!file.exists(input_file)) {
      stop("Input file does not exist:", input_file)
    }
    
    ext <- tolower(tools::file_ext(input_file))
    if (!ext %in% c("pdf", "jpg", "jpeg", "png", "tiff", "bmp")) {
      stop("Unsupported input file type:", ext)
    }
    
    if (ext == "pdf") {
      text <- pdf_text(input_file)
      text <- paste(text, collapse = "\n")
    } else {
      eng <- tesseract("eng")
      img <- magick::image_read(input_file)
      text <- tesseract::ocr(img, engine = eng)
    }
  } else if (inherits(input_file, "magick-image")) {
    eng <- tesseract("eng")
    text <- tesseract::ocr(input_file, engine = eng)
  } else {
    stop("Unsupported input type. Please provide a file path or a magick-image object.")
  }
  
  # Check output file write permission
  if (file.exists(output_file) && !file.access(output_file, 2)) {
    stop("Output file already exists and is not writable:", output_file)
  }
  
  # Save the extracted text to the output file
  cat(text, file = output_file)
  cat("Text extracted and saved to", output_file, "\n")
}

#' Preprocessing function to process images before OCR
#'
#' @param input_file Character string of file path for the image to be processed.
#'
#' @return A magick-image object containing the processed image.
#' @examples
#' \dontrun{
#' img <- process_image("sample_image.png")
#' ocr_parse(img, "output.txt")
#' }
process_image <- function(input_file) {
  if (!file.exists(input_file)) {
    stop("Input file does not exist:", input_file)
  }
  
  img <- image_read(input_file) %>%
    image_resize("3000x") %>%   # Resize to a fixed width
    image_convert(type = "Grayscale") %>% # Convert to grayscale
    image_threshold("black", "50%") # Adaptive thresholding
  
  return(img)
}

#' Run OCR function in parallel for multiple files
#'
#' @param files A list containing named lists with 'input' and 'output' file paths.
#'
#' @return None.
#' @examples
#' \dontrun{
#' files <- list(
#'   list(input = "sample1.pdf", output = "output1.txt"),
#'   list(input = "sample2.pdf", output = "output2.txt"),
#'   list(input = "sample3.pdf", output = "output3.txt")
#' )
#'
#' process_files_parallel(files)
#' }
process_files_parallel <- function(files) {
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  clusterExport(cl, "ocr_parse")
  parLapply(cl, files, function(file_pair) {
    ocr_parse(file_pair$input, file_pair$output)
  })
  stopCluster(cl)
}

#' Process files with progress reporting
#'
#' @param files A list containing named lists with 'input' and 'output' file paths.
#'
#' @return None.
#' @examples
#' \dontrun{
#' files <- list(
#'   list(input = "sample1.pdf", output = "output1.txt"),
#'   list(input = "sample2.pdf", output = "output2.txt"),
#'   list(input = "sample3.pdf", output = "output3.txt")
#' )
#'
#' process_files_with_progress(files)
#' }
process_files_with_progress <- function(files) {
  pb <- progress::progress_bar$new(total = length(files), format = "Processing: [:bar] :percent :elapsed :eta")
  
  for (file_pair in files) {
    ocr_parse(file_pair$input, file_pair$output)
    pb$tick()
  }
}
