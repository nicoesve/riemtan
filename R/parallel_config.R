#' Parallel Processing Configuration for riemtan
#'
#' @description
#' This module provides functions to configure and manage parallel processing
#' using the futureverse framework (future + furrr packages).
#'
#' @name parallel_config
NULL

#' Set Parallel Processing Plan
#'
#' @description
#' Configure the parallel processing strategy for riemtan operations.
#' Uses the future package to manage parallel backends.
#'
#' @param strategy Character string specifying the parallel strategy:
#'   - `"sequential"`: No parallelization (default for safety)
#'   - `"multisession"`: Parallel processing using multiple R sessions (works on all platforms including Windows)
#'   - `"multicore"`: Parallel processing using forked R processes (Unix-like systems only, faster but not available on Windows)
#'   - `"cluster"`: Parallel processing on a cluster of machines
#' @param workers Integer specifying the number of parallel workers.
#'   If `NULL` (default), uses `parallel::detectCores() - 1` to leave one core free.
#'   Ignored when `strategy = "sequential"`.
#'
#' @return Invisibly returns the future plan object.
#'
#' @details
#' The `multisession` strategy is recommended for most users as it works on all platforms.
#' The `multicore` strategy is faster on Unix-like systems but is not available on Windows.
#'
#' To disable parallel processing, use `set_parallel_plan("sequential")`.
#'
#' @examples
#' \dontrun{
#' # Enable parallel processing with automatic worker detection
#' set_parallel_plan("multisession")
#'
#' # Use 4 parallel workers
#' set_parallel_plan("multisession", workers = 4)
#'
#' # Disable parallel processing
#' set_parallel_plan("sequential")
#' }
#'
#' @seealso [future::plan()], [is_parallel_enabled()], [should_parallelize()]
#'
#' @export
set_parallel_plan <- function(strategy = "sequential", workers = NULL) {
  # Validate strategy
  valid_strategies <- c("sequential", "multisession", "multicore", "cluster")
  if (!strategy %in% valid_strategies) {
    stop(sprintf(
      "Invalid strategy '%s'. Must be one of: %s",
      strategy,
      paste(valid_strategies, collapse = ", ")
    ))
  }

  # Determine number of workers
  if (is.null(workers) && strategy != "sequential") {
    workers <- max(1, parallel::detectCores() - 1)
  }

  # Set the plan
  if (strategy == "sequential") {
    future::plan(future::sequential)
  } else if (strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
  } else if (strategy == "multicore") {
    if (.Platform$OS.type == "windows") {
      warning("multicore strategy not available on Windows. Using multisession instead.")
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multicore, workers = workers)
    }
  } else if (strategy == "cluster") {
    future::plan(future::cluster, workers = workers)
  }

  message(sprintf(
    "Parallel processing configured: strategy = %s%s",
    strategy,
    if (strategy != "sequential") sprintf(", workers = %d", workers) else ""
  ))

  invisible(future::plan())
}

#' Check if Parallel Processing is Enabled
#'
#' @description
#' Checks whether parallel processing is currently enabled based on the future plan.
#'
#' @return Logical value: `TRUE` if parallel processing is enabled, `FALSE` if sequential.
#'
#' @examples
#' \dontrun{
#' # Check current status
#' is_parallel_enabled()
#'
#' # Enable parallel processing
#' set_parallel_plan("multisession")
#' is_parallel_enabled()  # Returns TRUE
#'
#' # Disable parallel processing
#' set_parallel_plan("sequential")
#' is_parallel_enabled()  # Returns FALSE
#' }
#'
#' @seealso [set_parallel_plan()], [should_parallelize()]
#'
#' @export
is_parallel_enabled <- function() {
  !inherits(future::plan(), "sequential")
}

#' Decide Whether to Use Parallel Processing
#'
#' @description
#' Internal function to determine if an operation should use parallel processing
#' based on the number of items to process and current configuration.
#'
#' @param n Integer specifying the number of items to process.
#' @param threshold Integer specifying the minimum number of items required
#'   for parallel processing to be beneficial (default: 10).
#'   Below this threshold, sequential processing is used even if parallelization is enabled.
#'
#' @return Logical value: `TRUE` if parallel processing should be used, `FALSE` otherwise.
#'
#' @details
#' This function returns `TRUE` only if:
#' 1. Parallel processing is enabled (via `set_parallel_plan()`)
#' 2. The number of items `n` is at least `threshold`
#'
#' For small numbers of items, the overhead of parallelization typically outweighs the benefits,
#' so sequential processing is used.
#'
#' @examples
#' \dontrun{
#' # With parallel processing enabled
#' set_parallel_plan("multisession")
#' should_parallelize(5)    # FALSE (below threshold)
#' should_parallelize(20)   # TRUE (above threshold)
#'
#' # With parallel processing disabled
#' set_parallel_plan("sequential")
#' should_parallelize(100)  # FALSE (sequential plan)
#' }
#'
#' @seealso [set_parallel_plan()], [is_parallel_enabled()]
#'
#' @export
should_parallelize <- function(n, threshold = 10) {
  # Validate inputs
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("n must be a single non-negative numeric value")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 1) {
    stop("threshold must be a single positive numeric value")
  }

  # Check both conditions
  n >= threshold && is_parallel_enabled()
}

#' Get Current Number of Parallel Workers
#'
#' @description
#' Returns the number of parallel workers configured in the current future plan.
#'
#' @return Integer specifying the number of workers, or 1 if sequential processing is enabled.
#'
#' @examples
#' \dontrun{
#' set_parallel_plan("multisession", workers = 4)
#' get_n_workers()  # Returns 4
#'
#' set_parallel_plan("sequential")
#' get_n_workers()  # Returns 1
#' }
#'
#' @seealso [set_parallel_plan()]
#'
#' @export
get_n_workers <- function() {
  if (is_parallel_enabled()) {
    future::nbrOfWorkers()
  } else {
    1L
  }
}

#' Reset Parallel Plan to Sequential
#'
#' @description
#' Convenience function to reset parallel processing to sequential mode.
#' Equivalent to `set_parallel_plan("sequential")`.
#'
#' @return Invisibly returns the future plan object.
#'
#' @examples
#' \dontrun{
#' # Enable parallel processing
#' set_parallel_plan("multisession", workers = 4)
#'
#' # Reset to sequential
#' reset_parallel_plan()
#' }
#'
#' @seealso [set_parallel_plan()]
#'
#' @export
reset_parallel_plan <- function() {
  set_parallel_plan("sequential")
}
