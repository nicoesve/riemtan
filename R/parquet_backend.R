#' DataBackend Abstract Class
#'
#' @description
#' Abstract base class defining the interface for storage backends.
#' All backend implementations must inherit from this class and implement
#' the required methods.
#'
#' @details
#' This class provides a common interface for different storage strategies:
#' - ListBackend: In-memory list storage (current default)
#' - ParquetBackend: Lazy-loaded Parquet files with LRU cache
#'
#' @keywords internal
DataBackend <- R6::R6Class(
  classname = "DataBackend",
  public = list(
    #' @description Get a specific matrix by index
    #' @param i Integer index of the matrix to retrieve
    #' @return A dppMatrix object
    get_matrix = function(i) {
      stop("get_matrix must be implemented by subclass")
    },

    #' @description Get all matrices
    #' @return A list of dppMatrix objects
    get_all_matrices = function() {
      stop("get_all_matrices must be implemented by subclass")
    },

    #' @description Get the number of matrices
    #' @return Integer count of matrices
    length = function() {
      stop("length must be implemented by subclass")
    },

    #' @description Get matrix dimensions
    #' @return Integer dimension (p) where matrices are p x p
    get_dimensions = function() {
      stop("get_dimensions must be implemented by subclass")
    }
  )
)

#' ListBackend Class
#'
#' @description
#' Backend implementation using in-memory list storage.
#' This wraps the existing list-based storage mechanism for backwards compatibility.
#'
#' @export
ListBackend <- R6::R6Class(
  classname = "ListBackend",
  inherit = DataBackend,
  private = list(
    matrices = NULL,
    n = NULL,
    p = NULL
  ),
  public = list(
    #' @description Initialize a ListBackend
    #' @param matrices A list of dppMatrix objects
    initialize = function(matrices) {
      if (!is.list(matrices)) {
        stop("matrices must be a list")
      }
      if (length(matrices) == 0) {
        stop("matrices list cannot be empty")
      }

      # Validate all matrices are dppMatrix
      class_flag <- matrices |>
        purrr::map_lgl(\(x) inherits(x, "dppMatrix")) |>
        all()
      if (!class_flag) {
        stop("All matrices must be dppMatrix objects")
      }

      private$matrices <- matrices
      private$n <- length(matrices)
      private$p <- nrow(matrices[[1]])
    },

    #' @description Get a specific matrix by index
    #' @param i Integer index
    #' @return A dppMatrix object
    get_matrix = function(i) {
      if (i < 1 || i > private$n) {
        stop(sprintf("Index %d out of bounds [1, %d]", i, private$n))
      }
      private$matrices[[i]]
    },

    #' @description Get all matrices
    #' @return A list of dppMatrix objects
    get_all_matrices = function() {
      private$matrices
    },

    #' @description Get the number of matrices
    #' @return Integer count
    length = function() {
      private$n
    },

    #' @description Get matrix dimensions
    #' @return Integer p (matrices are p x p)
    get_dimensions = function() {
      private$p
    }
  )
)

#' ParquetBackend Class
#'
#' @description
#' Backend implementation using Apache Arrow Parquet files with lazy loading.
#' Matrices are stored as individual Parquet files and loaded on-demand with LRU caching.
#'
#' @export
ParquetBackend <- R6::R6Class(
  classname = "ParquetBackend",
  inherit = DataBackend,
  private = list(
    data_dir = NULL,
    metadata = NULL,
    cache = NULL,
    cache_keys = NULL,
    cache_size = NULL,
    n = NULL,
    p = NULL,

    #' @description Load metadata from JSON file
    load_metadata = function() {
      metadata_path <- file.path(private$data_dir, "metadata.json")
      if (!file.exists(metadata_path)) {
        stop(sprintf("Metadata file not found: %s", metadata_path))
      }
      private$metadata <- jsonlite::fromJSON(metadata_path)

      # Validate metadata structure
      required_fields <- c("n_matrices", "matrix_dim", "file_pattern")
      missing <- setdiff(required_fields, names(private$metadata))
      if (length(missing) > 0) {
        stop(sprintf("Metadata missing required fields: %s", paste(missing, collapse = ", ")))
      }

      private$n <- private$metadata$n_matrices
      private$p <- private$metadata$matrix_dim
    },

    #' @description Load a matrix from Parquet file
    #' @param i Integer index
    #' @return A dppMatrix object
    load_from_parquet = function(i) {
      file_name <- sprintf(private$metadata$file_pattern, i)
      file_path <- file.path(private$data_dir, file_name)

      if (!file.exists(file_path)) {
        stop(sprintf("Parquet file not found: %s", file_path))
      }

      # Read Parquet file
      df <- arrow::read_parquet(file_path)

      # Convert to matrix
      mat <- as.matrix(df)

      # Validate dimensions
      if (nrow(mat) != private$p || ncol(mat) != private$p) {
        stop(sprintf("Matrix %d has incorrect dimensions: expected %dx%d, got %dx%d",
                     i, private$p, private$p, nrow(mat), ncol(mat)))
      }

      # Convert to dppMatrix (use nearPD to ensure positive definite)
      Matrix::nearPD(mat)$mat |> Matrix::pack()
    },

    #' @description Update LRU cache with a new matrix
    #' @param i Integer index
    #' @param mat A dppMatrix object
    update_cache = function(i, mat) {
      key <- as.character(i)

      # If key already in cache, move to front
      if (key %in% private$cache_keys) {
        private$cache_keys <- c(key, setdiff(private$cache_keys, key))
        return()
      }

      # If cache is full, remove least recently used
      if (length(private$cache_keys) >= private$cache_size) {
        lru_key <- private$cache_keys[length(private$cache_keys)]
        private$cache[[lru_key]] <- NULL
        private$cache_keys <- private$cache_keys[-length(private$cache_keys)]
      }

      # Add new entry to front
      private$cache[[key]] <- mat
      private$cache_keys <- c(key, private$cache_keys)
    }
  ),
  public = list(
    #' @description Initialize a ParquetBackend
    #' @param data_dir Path to directory containing Parquet files and metadata.json
    #' @param cache_size Maximum number of matrices to cache (default 10)
    initialize = function(data_dir, cache_size = 10) {
      if (!dir.exists(data_dir)) {
        stop(sprintf("Data directory does not exist: %s", data_dir))
      }

      private$data_dir <- normalizePath(data_dir)
      private$cache_size <- cache_size
      private$cache <- list()
      private$cache_keys <- character(0)

      # Load metadata
      private$load_metadata()
    },

    #' @description Get a specific matrix by index
    #' @param i Integer index
    #' @return A dppMatrix object
    get_matrix = function(i) {
      if (i < 1 || i > private$n) {
        stop(sprintf("Index %d out of bounds [1, %d]", i, private$n))
      }

      key <- as.character(i)

      # Check cache first
      if (key %in% names(private$cache)) {
        # Update LRU order
        private$cache_keys <- c(key, setdiff(private$cache_keys, key))
        return(private$cache[[key]])
      }

      # Load from disk
      mat <- private$load_from_parquet(i)

      # Update cache
      private$update_cache(i, mat)

      mat
    },

    #' @description Get all matrices (loads all from disk if necessary)
    #' @param parallel Logical indicating whether to use parallel loading (default: NULL, auto-detect)
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return A list of dppMatrix objects
    get_all_matrices = function(parallel = NULL, progress = FALSE) {
      # Auto-detect parallelization if not specified
      if (is.null(parallel)) {
        parallel <- should_parallelize(private$n, threshold = 5)
      }

      if (parallel) {
        # Parallel loading
        matrices <- with_progress({
          p <- create_progressor(private$n, enable = progress)
          furrr::future_map(
            1:private$n,
            \(i) {
              mat <- self$get_matrix(i)
              p()
              mat
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
        }, name = "Loading matrices from Parquet", enable = progress)
      } else {
        # Sequential loading
        matrices <- 1:private$n |>
          purrr::map(\(i) self$get_matrix(i))
      }

      matrices
    },

    #' @description Load multiple matrices in parallel (batch loading)
    #' @param indices Vector of integer indices to load
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return A list of dppMatrix objects
    get_matrices_parallel = function(indices, progress = FALSE) {
      # Validate indices
      if (!is.numeric(indices) || any(indices < 1) || any(indices > private$n)) {
        stop(sprintf("All indices must be in range [1, %d]", private$n))
      }

      n <- length(indices)

      if (should_parallelize(n, threshold = 5)) {
        # Parallel loading
        with_progress({
          p <- create_progressor(n, enable = progress)
          furrr::future_map(
            indices,
            \(i) {
              mat <- self$get_matrix(i)
              p()
              mat
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
        }, name = "Loading batch from Parquet", enable = progress)
      } else {
        # Sequential loading
        lapply(indices, \(i) self$get_matrix(i))
      }
    },

    #' @description Get the number of matrices
    #' @return Integer count
    length = function() {
      private$n
    },

    #' @description Get matrix dimensions
    #' @return Integer p (matrices are p x p)
    get_dimensions = function() {
      private$p
    },

    #' @description Get metadata
    #' @return List containing metadata information
    get_metadata = function() {
      private$metadata
    },

    #' @description Clear the cache
    clear_cache = function() {
      private$cache <- list()
      private$cache_keys <- character(0)
      invisible(self)
    },

    #' @description Get current cache size
    #' @return Integer number of cached matrices
    get_cache_size = function() {
      length(private$cache_keys)
    }
  )
)
