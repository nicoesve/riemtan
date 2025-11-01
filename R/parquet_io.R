#' Write Connectomes to Parquet Files
#'
#' @description
#' Exports a list of SPD matrices (connectomes) to individual Parquet files
#' with accompanying metadata.
#'
#' @param connectomes A list of dppMatrix objects representing SPD matrices
#' @param output_dir Path to output directory (will be created if it doesn't exist)
#' @param file_pattern File naming pattern with %d placeholder for index (default: "matrix_%04d.parquet")
#' @param subject_ids Optional vector of subject/sample identifiers (default: NULL)
#' @param provenance Optional list containing data provenance information (default: NULL)
#' @param overwrite If TRUE, overwrites existing directory (default: FALSE)
#'
#' @return Invisibly returns the path to the output directory
#'
#' @details
#' Creates a directory structure:
#' - Individual Parquet files (one per matrix)
#' - metadata.json with dimensions, file pattern, and optional metadata
#'
#' The metadata.json file contains:
#' - n_matrices: Number of matrices
#' - matrix_dim: Dimension p (matrices are p x p)
#' - file_pattern: Naming pattern for Parquet files
#' - subject_ids: Optional subject identifiers
#' - provenance: Optional provenance information
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' mats <- replicate(10, Matrix::pack(Matrix::Matrix(diag(5), sparse = FALSE)), simplify = FALSE)
#'
#' # Write to Parquet
#' write_connectomes_to_parquet(
#'   mats,
#'   output_dir = "my_connectomes",
#'   subject_ids = paste0("subj_", 1:10),
#'   provenance = list(study = "Example Study", date = Sys.Date())
#' )
#' }
#'
#' @export
write_connectomes_to_parquet <- function(connectomes,
                                          output_dir,
                                          file_pattern = "matrix_%04d.parquet",
                                          subject_ids = NULL,
                                          provenance = NULL,
                                          overwrite = FALSE) {
  # Validate inputs
  if (!is.list(connectomes)) {
    stop("connectomes must be a list")
  }
  if (length(connectomes) == 0) {
    stop("connectomes list cannot be empty")
  }

  # Validate all are dppMatrix
  class_flag <- connectomes |>
    purrr::map_lgl(\(x) inherits(x, "dppMatrix")) |>
    all()
  if (!class_flag) {
    stop("All connectomes must be dppMatrix objects")
  }

  # Check dimensions are consistent
  p <- nrow(connectomes[[1]])
  dims_flag <- connectomes |>
    purrr::map_lgl(\(x) nrow(x) == p) |>
    all()
  if (!dims_flag) {
    stop("All matrices must have the same dimensions")
  }

  # Validate subject_ids if provided
  if (!is.null(subject_ids)) {
    if (length(subject_ids) != length(connectomes)) {
      stop("subject_ids must have the same length as connectomes")
    }
  }

  # Check output directory
  if (dir.exists(output_dir)) {
    if (!overwrite) {
      stop(sprintf("Directory '%s' already exists. Use overwrite = TRUE to replace.", output_dir))
    }
    unlink(output_dir, recursive = TRUE)
  }

  # Create output directory
  dir.create(output_dir, recursive = TRUE)

  # Write each matrix to Parquet
  n <- length(connectomes)
  message(sprintf("Writing %d matrices to %s...", n, output_dir))

  for (i in seq_along(connectomes)) {
    file_name <- sprintf(file_pattern, i)
    file_path <- file.path(output_dir, file_name)

    # Convert packed matrix to full matrix
    mat <- as.matrix(connectomes[[i]])

    # Convert to data frame for Arrow
    df <- as.data.frame(mat)

    # Write to Parquet
    arrow::write_parquet(df, file_path)

    if (i %% 10 == 0) {
      message(sprintf("  Written %d/%d matrices", i, n))
    }
  }

  message(sprintf("  Written %d/%d matrices", n, n))

  # Create metadata
  metadata <- list(
    n_matrices = n,
    matrix_dim = p,
    file_pattern = file_pattern,
    created_date = as.character(Sys.time())
  )

  if (!is.null(subject_ids)) {
    metadata$subject_ids <- subject_ids
  }

  if (!is.null(provenance)) {
    metadata$provenance <- provenance
  }

  # Write metadata to JSON
  metadata_path <- file.path(output_dir, "metadata.json")
  jsonlite::write_json(metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE)

  message(sprintf("Metadata written to %s", metadata_path))
  message("Done!")

  invisible(normalizePath(output_dir))
}

#' Validate Parquet Directory
#'
#' @description
#' Validates that a directory contains properly formatted Parquet files
#' and metadata for use with ParquetBackend.
#'
#' @param data_dir Path to directory to validate
#' @param verbose If TRUE, prints validation details (default: TRUE)
#'
#' @return Logical indicating whether the directory is valid
#'
#' @details
#' Checks:
#' - Directory exists
#' - metadata.json exists and is valid
#' - All expected Parquet files exist
#' - Parquet files have correct dimensions
#'
#' @examples
#' \dontrun{
#' validate_parquet_directory("my_connectomes")
#' }
#'
#' @export
validate_parquet_directory <- function(data_dir, verbose = TRUE) {
  # Check directory exists
  if (!dir.exists(data_dir)) {
    if (verbose) message(sprintf("ERROR: Directory does not exist: %s", data_dir))
    return(FALSE)
  }

  # Check metadata.json exists
  metadata_path <- file.path(data_dir, "metadata.json")
  if (!file.exists(metadata_path)) {
    if (verbose) message(sprintf("ERROR: metadata.json not found in %s", data_dir))
    return(FALSE)
  }

  # Read metadata
  metadata <- tryCatch(
    {
      jsonlite::fromJSON(metadata_path)
    },
    error = function(e) {
      if (verbose) message(sprintf("ERROR: Failed to read metadata.json: %s", e$message))
      return(NULL)
    }
  )

  if (is.null(metadata)) {
    return(FALSE)
  }

  # Validate metadata structure
  required_fields <- c("n_matrices", "matrix_dim", "file_pattern")
  missing <- setdiff(required_fields, names(metadata))
  if (length(missing) > 0) {
    if (verbose) {
      message(sprintf("ERROR: metadata.json missing required fields: %s",
                      paste(missing, collapse = ", ")))
    }
    return(FALSE)
  }

  n <- metadata$n_matrices
  p <- metadata$matrix_dim
  pattern <- metadata$file_pattern

  if (verbose) {
    message(sprintf("Validating directory: %s", data_dir))
    message(sprintf("  Expected: %d matrices of dimension %dx%d", n, p, p))
  }

  # Check all Parquet files exist
  missing_files <- character(0)
  for (i in 1:n) {
    file_name <- sprintf(pattern, i)
    file_path <- file.path(data_dir, file_name)
    if (!file.exists(file_path)) {
      missing_files <- c(missing_files, file_name)
    }
  }

  if (length(missing_files) > 0) {
    if (verbose) {
      message(sprintf("ERROR: Missing %d Parquet files", length(missing_files)))
      if (length(missing_files) <= 5) {
        message(sprintf("  Missing: %s", paste(missing_files, collapse = ", ")))
      } else {
        message(sprintf("  First 5 missing: %s, ...", paste(missing_files[1:5], collapse = ", ")))
      }
    }
    return(FALSE)
  }

  # Validate a sample of Parquet files have correct dimensions
  sample_indices <- if (n <= 3) {
    1:n
  } else {
    c(1, floor(n / 2), n)  # Check first, middle, and last
  }

  for (i in sample_indices) {
    file_name <- sprintf(pattern, i)
    file_path <- file.path(data_dir, file_name)

    tryCatch(
      {
        df <- arrow::read_parquet(file_path)
        mat <- as.matrix(df)

        if (nrow(mat) != p || ncol(mat) != p) {
          if (verbose) {
            message(sprintf("ERROR: File %s has incorrect dimensions: %dx%d (expected %dx%d)",
                            file_name, nrow(mat), ncol(mat), p, p))
          }
          return(FALSE)
        }
      },
      error = function(e) {
        if (verbose) {
          message(sprintf("ERROR: Failed to read %s: %s", file_name, e$message))
        }
        return(FALSE)
      }
    )
  }

  if (verbose) {
    message("  All checks passed!")
    if (!is.null(metadata$subject_ids)) {
      message(sprintf("  Contains subject IDs: %d subjects", length(metadata$subject_ids)))
    }
    if (!is.null(metadata$provenance)) {
      message(sprintf("  Contains provenance metadata"))
    }
  }

  TRUE
}

#' Create ParquetBackend from Directory
#'
#' @description
#' Convenience function to create a ParquetBackend from a validated directory.
#'
#' @param data_dir Path to directory containing Parquet files and metadata
#' @param cache_size Maximum number of matrices to cache in memory (default: 10)
#' @param validate If TRUE, validates directory before creating backend (default: TRUE)
#'
#' @return A ParquetBackend object
#'
#' @examples
#' \dontrun{
#' backend <- create_parquet_backend("my_connectomes")
#' sample <- CSample$new(backend = backend, metric_obj = airm())
#' }
#'
#' @export
create_parquet_backend <- function(data_dir, cache_size = 10, validate = TRUE) {
  if (validate) {
    is_valid <- validate_parquet_directory(data_dir, verbose = FALSE)
    if (!is_valid) {
      stop(sprintf("Directory validation failed: %s\nRun validate_parquet_directory() for details.", data_dir))
    }
  }

  ParquetBackend$new(data_dir, cache_size)
}
