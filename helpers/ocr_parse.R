# =============================================================================
# OCR Processing Module with UAT Tests
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
  
  # Use a default CRAN mirror when none is configured
  repos <- getOption("repos")
  if (is.null(repos) || identical(repos, "@CRAN@") || length(repos) == 0) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  
  installed <- rownames(installed.packages())
  for (pkg in packages) {
    if (!(pkg %in% installed)) {
      message("Installing package: ", pkg)
      install.packages(pkg, repos = repos, dependencies = TRUE)
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
#' @param pdf_dpi Numeric; resolution used when rasterizing PDF pages for OCR (default 300).
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
ocr_parse <- function(input, output, lang = "eng", verbose = TRUE, pdf_dpi = 300) {
  start_time <- Sys.time()
  text <- NULL
  num_pages <- 0L
  
  if (!is.character(output) || length(output) != 1 || !nzchar(output)) {
    stop("output must be a single, non-empty file path.")
  }
  
  # Ensure the output directory exists and is writable
  out_dir <- dirname(output)
  if (!dir.exists(out_dir)) {
    if (verbose) message("Creating output directory: ", out_dir)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(out_dir)) {
    stop("Output directory does not exist and could not be created: ", out_dir)
  }
  if (file.access(out_dir, mode = 2) != 0) {
    stop("Output directory is not writable: ", out_dir)
  }
  
  # If output file exists, confirm it is writable before overwriting
  if (file.exists(output)) {
    if (file.access(output, mode = 2) != 0) {
      stop("Output file exists and is not writable: ", output)
    }
    if (verbose) message("Warning: Output file exists and will be overwritten: ", output)
  }
  
  engine <- tesseract::tesseract(lang)
  
  # Validate and process the input
  if (is.character(input)) {
    if (length(input) != 1 || !nzchar(input)) {
      stop("input must be a single, non-empty file path when provided as character.")
    }
    if (!file.exists(input)) {
      stop("Input file does not exist: ", input)
    }
    
    ext <- tolower(tools::file_ext(input))
    if (!(ext %in% c("pdf", "jpg", "jpeg", "png", "tiff", "bmp"))) {
      stop("Unsupported input file type: ", ext)
    }
    
    if (ext == "pdf") {
      if (!is.numeric(pdf_dpi) || length(pdf_dpi) != 1 || is.na(pdf_dpi) || pdf_dpi <= 0) {
        stop("pdf_dpi must be a single positive number.")
      }
      
      if (verbose) message("Processing PDF file with OCR: ", input)
      
      pdf_info <- pdftools::pdf_info(input)
      num_pages <- as.integer(pdf_info$pages)
      
      # Rasterize PDF pages to images, then run OCR page-by-page
      tmp_dir <- tempfile("ocr_pdf_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(tmp_dir)) {
        stop("Temporary directory could not be created: ", tmp_dir)
      }
      
      on.exit({
        if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE, force = TRUE)
      }, add = TRUE)
      
      image_files <- tryCatch(
        pdftools::pdf_convert(
          pdf = input,
          format = "png",
          dpi = pdf_dpi,
          pages = seq_len(num_pages),
          filenames = file.path(tmp_dir, sprintf("page_%04d", seq_len(num_pages)))
        ),
        error = function(e) {
          stop("PDF rasterization failed: ", conditionMessage(e))
        }
      )
      
      page_text <- character(length(image_files))
      for (i in seq_along(image_files)) {
        if (verbose) message("OCR page ", i, " of ", length(image_files))
        img <- magick::image_read(image_files[[i]])
        page_text[[i]] <- tesseract::ocr(img, engine = engine)
      }
      
      text <- paste(page_text, collapse = "\n")
    } else {
      if (verbose) message("Processing image file: ", input)
      img <- magick::image_read(input)
      num_pages <- as.integer(length(img))
      frame_text <- vapply(seq_len(num_pages), function(i) {
        tesseract::ocr(img[i], engine = engine)
      }, FUN.VALUE = character(1))
      text <- paste(frame_text, collapse = "\n")
    }
  } else if (inherits(input, "magick-image")) {
    if (verbose) message("Processing magick-image object.")
    num_pages <- as.integer(length(input))
    frame_text <- vapply(seq_len(num_pages), function(i) {
      tesseract::ocr(input[i], engine = engine)
    }, FUN.VALUE = character(1))
    text <- paste(frame_text, collapse = "\n")
  } else {
    stop("Unsupported input type. Provide a file path or a magick-image object.")
  }
  
  # Write the extracted text to the output file
  writeLines(text, con = output, useBytes = TRUE)
  
  elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
  stats <- list(
    processing_time = as.numeric(elapsed_time),
    character_count = nchar(text, type = "chars"),
    num_pages = num_pages
  )
  
  if (verbose) {
    message("Text extracted and saved to ", output)
    message("Processing time (secs): ", stats$processing_time)
    message("Character count: ", stats$character_count)
    message("Number of pages processed: ", stats$num_pages)
  }
  
  list(text = text, stats = stats)
}

# ----- Image Preprocessing Function -----

#' Preprocess an image to support OCR readability.
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
  if (!is.character(input) || length(input) != 1 || !nzchar(input)) {
    stop("input must be a single, non-empty file path.")
  }
  if (!file.exists(input)) {
    stop("Input file does not exist: ", input)
  }
  
  img <- magick::image_read(input)
  img <- magick::image_resize(img, resize_width)
  
  if (!is.logical(grayscale) || length(grayscale) != 1 || is.na(grayscale)) {
    stop("grayscale must be a single logical value.")
  }
  if (grayscale) {
    img <- magick::image_convert(img, colorspace = "gray")
  }
  
  if (!is.character(threshold) || length(threshold) != 1 || !nzchar(threshold)) {
    stop("threshold must be a single, non-empty character value (for example, \"50%\").")
  }
  
  img <- magick::image_threshold(img, type = "black", threshold = threshold)
  img
}

# ----- Parallel Processing Function -----

#' Process multiple OCR tasks in parallel.
#'
#' @param files A non-empty list of lists. Each inner list must have elements \code{input} and \code{output}.
#' @param lang A character string specifying the OCR language (default "eng").
#' @param verbose Logical; if TRUE, prints status messages (default TRUE).
#' @param workers Integer; number of worker processes (default: max(1, detectCores() - 1)).
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
process_files_parallel <- function(files, lang = "eng", verbose = TRUE,
                                   workers = max(1L, parallel::detectCores() - 1L)) {
  if (!is.list(files) || length(files) == 0) {
    stop("files must be a non-empty list of file pair lists.")
  }
  if (!is.numeric(workers) || length(workers) != 1 || is.na(workers) || workers < 1) {
    stop("workers must be a single positive integer.")
  }
  workers <- as.integer(workers)
  
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Ensure required libraries are loaded on each worker
  parallel::clusterEvalQ(cl, {
    library(tesseract)
    library(magick)
    library(pdftools)
  })
  
  # Export the OCR function to workers
  parallel::clusterExport(cl, varlist = c("ocr_parse"), envir = environment())
  
  results <- parallel::parLapply(cl, files, function(file_pair, lang) {
    if (!is.list(file_pair) || is.null(file_pair$input) || is.null(file_pair$output)) {
      stop("Each file pair must be a list with 'input' and 'output' elements.")
    }
    ocr_parse(file_pair$input, file_pair$output, lang = lang, verbose = FALSE)
  }, lang = lang)
  
  if (verbose) {
    message("Parallel processing completed for ", length(files), " files using ", workers, " workers.")
  }
  results
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
  results
}
