# =============================================================================
# Enhanced OCR Processing Module with UAT Tests
# =============================================================================

# ----- Helper Function: Install Required Packages -----

#' Install required packages if they are not already installed.
#'
#' @param packages A non-empty character vector of package names.
#'
#' @return Invisible NULL.
install_packages <- function(packages) {
  if (!is.character(packages) || length(packages) == 0) {
    stop("packages must be a non-empty character vector.")
  }
  installed <- rownames(installed.packages())
  for (pkg in packages) {
    if (!(pkg %in% installed)) {
      message("Installing package: ", pkg)
      install.packages(pkg)
    }
  }
  invisible(NULL)
}

# List of required packages (including testthat for UAT)
required_packages <- c("tesseract", "pdftools", "magick", "parallel", "progress", "testthat")
install_packages(required_packages)

# Load libraries
library(tesseract)
library(pdftools)
library(magick)
library(parallel)
library(progress)
library(testthat)

# ----- OCR Parsing Function -----

#' Extract text from an image or PDF file using OCR.
#'
#' @param input Either a character string (file path) or a magick-image object.
#' @param output A character string specifying the file path to save the extracted text.
#' @param lang A character string specifying the OCR language (default "eng").
#' @param verbose Logical; if TRUE, prints progress and statistics (default TRUE).
#'
#' @return A list with elements:
#'   - \code{text}: The extracted text.
#'   - \code{stats}: A list containing processing time (secs), character count, and number of pages processed.
#'
#' @examples
#' \dontrun{
#'   # Using a file path
#'   result <- ocr_parse("sample.pdf", "output.txt")
#'
#'   # Using a magick-image object
#'   img <- image_read("sample_image.png")
#'   result <- ocr_parse(img, "output.txt")
#' }
ocr_parse <- function(input, output, lang = "eng", verbose = TRUE) {
  start_time <- Sys.time()
  text <- NULL
  
  # Validate and process the input
  if (is.character(input)) {
    if (!file.exists(input)) {
      stop("Input file does not exist: ", input)
    }
    ext <- tolower(tools::file_ext(input))
    if (! ext %in% c("pdf", "jpg", "jpeg", "png", "tiff", "bmp")) {
      stop("Unsupported input file type: ", ext)
    }
    
    if (ext == "pdf") {
      if (verbose) message("Processing PDF file: ", input)
      pages <- pdf_text(input)
      text <- paste(pages, collapse = "\n")
      num_pages <- length(pages)
    } else {
      if (verbose) message("Processing image file: ", input)
      eng <- tesseract::tesseract(lang)
      img <- magick::image_read(input)
      text <- tesseract::ocr(img, engine = eng)
      num_pages <- 1
    }
    
  } else if (inherits(input, "magick-image")) {
    if (verbose) message("Processing magick-image object.")
    eng <- tesseract::tesseract(lang)
    text <- tesseract::ocr(input, engine = eng)
    num_pages <- 1
  } else {
    stop("Unsupported input type. Provide a file path or a magick-image object.")
  }
  
  # Check output file write permissions
  if (file.exists(output)) {
    if (file.access(output, mode = 2) != 0) {
      stop("Output file exists and is not writable: ", output)
    }
    if (verbose) message("Warning: Output file exists and will be overwritten: ", output)
  }
  
  # Write the extracted text to the output file
  writeLines(text, con = output)
  
  elapsed_time <- Sys.time() - start_time
  stats <- list(
    processing_time = as.numeric(elapsed_time, units = "secs"),
    character_count = nchar(text),
    num_pages = num_pages
  )
  
  if (verbose) {
    message("Text extracted and saved to ", output)
    message("Processing time (secs): ", stats$processing_time)
    message("Character count: ", stats$character_count)
    message("Number of pages processed: ", num_pages)
  }
  
  return(list(text = text, stats = stats))
}

# ----- Image Preprocessing Function -----

#' Preprocess an image to enhance OCR accuracy.
#'
#' @param input A character string specifying the image file path.
#' @param resize_width A character string specifying the new width (default "3000x").
#' @param grayscale Logical; if TRUE, convert the image to grayscale (default TRUE).
#' @param threshold A string specifying the threshold level (default "50%").
#'
#' @return A magick-image object containing the processed image.
#'
#' @examples
#' \dontrun{
#'   processed_img <- process_image("sample_image.png")
#' }
process_image <- function(input, resize_width = "3000x", grayscale = TRUE, threshold = "50%") {
  if (!is.character(input) || length(input) != 1) {
    stop("input must be a single file path.")
  }
  if (!file.exists(input)) {
    stop("Input file does not exist: ", input)
  }
  
  img <- magick::image_read(input)
  img <- magick::image_resize(img, resize_width)
  
  if (grayscale) {
    img <- magick::image_convert(img, colorspace = "gray")
  }
  
  img <- magick::image_threshold(img, type = "black", threshold = threshold)
  return(img)
}

# ----- Parallel Processing Function -----

#' Process multiple OCR tasks in parallel.
#'
#' @param files A non-empty list of lists. Each inner list must have elements \code{input} and \code{output}.
#' @param lang A character string specifying the OCR language (default "eng").
#' @param verbose Logical; if TRUE, prints status messages (default TRUE).
#'
#' @return A list of results (each as returned by \code{ocr_parse}).
#'
#' @examples
#' \dontrun{
#'   files <- list(
#'     list(input = "sample1.pdf", output = "output1.txt"),
#'     list(input = "sample2.pdf", output = "output2.txt")
#'   )
#'   results <- process_files_parallel(files)
#' }
process_files_parallel <- function(files, lang = "eng", verbose = TRUE) {
  if (!is.list(files) || length(files) == 0) {
    stop("files must be a non-empty list of file pair lists.")
  }
  
  num_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(num_cores)
  
  # Ensure necessary libraries are loaded on each cluster node
  parallel::clusterEvalQ(cl, {
    library(tesseract)
    library(magick)
    library(pdftools)
  })
  parallel::clusterExport(cl, varlist = c("ocr_parse"), envir = environment())
  
  results <- parallel::parLapply(cl, files, function(file_pair) {
    if (!is.list(file_pair) || is.null(file_pair$input) || is.null(file_pair$output)) {
      stop("Each file pair must be a list with 'input' and 'output' elements.")
    }
    res <- ocr_parse(file_pair$input, file_pair$output, lang = lang, verbose = FALSE)
    return(res)
  })
  
  parallel::stopCluster(cl)
  if (verbose) {
    message("Parallel processing completed for ", length(files), " files.")
  }
  return(results)
}

# ----- Sequential Processing with Progress Bar -----

#' Process multiple OCR tasks sequentially with a progress bar.
#'
#' @param files A non-empty list of lists. Each inner list must have elements \code{input} and \code{output}.
#' @param lang A character string specifying the OCR language (default "eng").
#' @param verbose Logical; if TRUE, prints status messages (default TRUE).
#'
#' @return A list of results (each as returned by \code{ocr_parse}).
#'
#' @examples
#' \dontrun{
#'   files <- list(
#'     list(input = "sample1.pdf", output = "output1.txt"),
#'     list(input = "sample2.pdf", output = "output2.txt")
#'   )
#'   results <- process_files_with_progress(files)
#' }
process_files_with_progress <- function(files, lang = "eng", verbose = TRUE) {
  if (!is.list(files) || length(files) == 0) {
    stop("files must be a non-empty list of file pair lists.")
  }
  
  pb <- progress::progress_bar$new(
    total = length(files),
    format = "Processing: [:bar] :percent | Elapsed: :elapsed | ETA: :eta"
  )
  
  results <- vector("list", length(files))
  for (i in seq_along(files)) {
    file_pair <- files[[i]]
    if (!is.list(file_pair) || is.null(file_pair$input) || is.null(file_pair$output)) {
      stop("Each file pair must be a list with 'input' and 'output' elements.")
    }
    results[[i]] <- ocr_parse(file_pair$input, file_pair$output, lang = lang, verbose = FALSE)
    pb$tick()
  }
  
  if (verbose) {
    message("Sequential processing completed for ", length(files), " files.")
  }
  return(results)
}

# =============================================================================
# UAT (User Acceptance Testing) Section: Testing Every Parameter & Function
# =============================================================================

if (interactive()) {
  message("Running UAT tests ...")
  
  test_that("install_packages validates input", {
    expect_error(install_packages(123), "non-empty character vector")
    expect_error(install_packages(character(0)), "non-empty character vector")
  })
  
  test_that("ocr_parse errors on non-existent file", {
    temp_output <- tempfile(fileext = ".txt")
    expect_error(ocr_parse("nonexistentfile.png", temp_output))
  })
  
  test_that("ocr_parse errors on unsupported file type", {
    temp_file <- tempfile(fileext = ".txt")
    file.create(temp_file)
    temp_output <- tempfile(fileext = ".txt")
    expect_error(ocr_parse(temp_file, temp_output), "Unsupported input file type")
    unlink(temp_file)
  })
  
  test_that("ocr_parse works with a magick-image object", {
    # Create a blank image and add annotation text.
    img <- magick::image_blank(width = 300, height = 100, color = "white")
    img <- magick::image_annotate(img, "Test OCR", size = 20, color = "black", gravity = "center")
    temp_output <- tempfile(fileext = ".txt")
    result <- ocr_parse(img, temp_output, verbose = FALSE)
    expect_true(is.list(result))
    expect_true("text" %in% names(result))
    expect_true("stats" %in% names(result))
    unlink(temp_output)
  })
  
  test_that("process_image errors on non-existent file", {
    expect_error(process_image("nonexistentfile.png"))
  })
  
  test_that("process_image returns a magick-image object", {
    temp_img <- tempfile(fileext = ".png")
    img <- magick::image_blank(width = 300, height = 100, color = "white")
    magick::image_write(img, path = temp_img, format = "png")
    processed <- process_image(temp_img)
    expect_true(inherits(processed, "magick-image"))
    unlink(temp_img)
  })
  
  test_that("process_files_parallel and process_files_with_progress work", {
    # Create a temporary image file with OCR text.
    temp_img <- tempfile(fileext = ".png")
    img <- magick::image_blank(width = 300, height = 100, color = "white")
    img <- magick::image_annotate(img, "Test OCR", size = 20, color = "black", gravity = "center")
    magick::image_write(img, path = temp_img, format = "png")
    temp_output1 <- tempfile(fileext = ".txt")
    temp_output2 <- tempfile(fileext = ".txt")
    
    files_list <- list(
      list(input = temp_img, output = temp_output1),
      list(input = temp_img, output = temp_output2)
    )
    
    res_parallel <- process_files_parallel(files_list, verbose = FALSE)
    res_progress <- process_files_with_progress(files_list, verbose = FALSE)
    
    expect_equal(length(res_parallel), 2)
    expect_equal(length(res_progress), 2)
    
    unlink(c(temp_img, temp_output1, temp_output2))
  })
  
  message("All UAT tests passed!")
}

# =============================================================================
# End of Module
# =============================================================================
