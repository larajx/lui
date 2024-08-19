#' gse_load
#'
#' Takes a folder_path containing a set of pattern "*.tar.gz",
#' and returns one them into one seurat object.
#'
#' @param dir_path char, path to folder containing the gsm files (in a *.tar.gz format)
#' @param files c(chars), name of each gsm (for example, gsm00223)to extract into the seurat object,
#' otherwise it'll take all of the gsm files in the folder and compile them.
#' @param sample_type c(chars), the grouping of each sample.  For example,
#>  if you have 4 samples total, 2 ctrl, 2 exp, it would be c("ctrl", 'ctrl', 'exp', 'exp')
#' @return The seurat object
#' @export
gse_load <- function(dir_path, files = NULL, sample_type = NULL) {
  library(Seurat)
  library(GEOquery)
  library(data.table)

  # Determine the files to process
  if (is.null(files)) {
    files_to_take <- list.files(dir_path, pattern = "*.tar.gz", full.names = TRUE)
  } else {
    files_to_take <- list.files(dir_path, pattern = paste0(files, collapse = "|"), full.names = TRUE)
  }

  # Print the files to be processed
  print(paste("Files to process:", files_to_take))

  # Create a temporary directory for extraction
  temp_dir <- tempdir()

  # Extract all tar.gz files to the temporary directory
  for (file in files_to_take) {
    untar(file, exdir = temp_dir)
  }

  # List all files in the temporary directory
  extracted_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)

  # Initialize a list to store sample data
  sample_list <- list()
  sample_names <- c()
  index <- 1

  # Read the expression data from each extracted file
  for (folder in unique(dirname(extracted_files))) {
    matrix_file <- list.files(folder, pattern = "matrix.mtx.gz", full.names = TRUE)
    barcodes_file <- list.files(folder, pattern = "barcodes.tsv.gz", full.names = TRUE)
    features_file <- list.files(folder, pattern = "features.tsv.gz", full.names = TRUE)

    if (length(matrix_file) > 0 && length(barcodes_file) > 0 && length(features_file) > 0) {
      expr_data <- Read10X(data.dir = folder)
      sample_list[[basename(folder)]] <- CreateSeuratObject(counts = expr_data,
                                                            project = basename(folder))
      sample_list[[basename(folder)]]$type <- sample_type[index]
      sample_names <- c(sample_names, basename(folder))
      index <- index + 1
    }
  }

  # Check if sample_list is empty
  if (length(sample_list) == 0) {
    stop("No expression files found.")
  }

  # Merge all the Seurat objects together
  seurat_obj <- merge(sample_list[[1]], sample_list[-1], add.cell.ids = files)
  sample_list <<- sample_list
  return(seurat_obj)


}
