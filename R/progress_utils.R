#' Progress Reporting Utilities for riemtan
#'
#' @description
#' This module provides utilities for progress reporting during
#' computationally intensive operations, using the progressr package.
#'
#' @name progress_utils
NULL

#' Execute Expression with Progress Reporting
#'
#' @description
#' Wrapper function that executes an expression with optional progress reporting.
#' Integrates with the progressr package when available.
#'
#' @param expr Expression to evaluate
#' @param name Character string specifying the name of the operation (default: "Processing")
#' @param enable Logical indicating whether to enable progress reporting (default: TRUE).
#'   If FALSE or if progressr is not available, the expression is executed without progress reporting.
#'
#' @return The result of evaluating `expr`.
#'
#' @details
#' This function provides a consistent interface for progress reporting across riemtan.
#' Progress handlers can be configured using `progressr::handlers()`.
#'
#' When `enable = TRUE` and progressr is installed, users can see progress by calling:
#' ```r
#' progressr::handlers("progress")  # or "txtprogressbar", "cli", etc.
#' ```
#'
#' @examples
#' \dontrun{
#' # Enable progress reporting
#' progressr::handlers("progress")
#'
#' # Use within a function
#' result <- with_progress({
#'   p <- progressr::progressor(steps = 10)
#'   lapply(1:10, function(i) {
#'     Sys.sleep(0.1)
#'     p()  # Signal progress
#'     i^2
#'   })
#' }, name = "Computing squares")
#' }
#'
#' @seealso [progressr::with_progress()], [progressr::progressor()]
#'
#' @keywords internal
with_progress <- function(expr, name = "Processing", enable = TRUE) {
  if (enable && requireNamespace("progressr", quietly = TRUE)) {
    progressr::with_progress(expr)
  } else {
    expr
  }
}

#' Create a Progress Reporter for Iterative Operations
#'
#' @description
#' Creates a progress reporting function for use in loops or apply-style operations.
#' Returns a no-op function if progressr is not available or disabled.
#'
#' @param steps Integer specifying the total number of steps
#' @param enable Logical indicating whether to enable progress reporting (default: TRUE)
#'
#' @return A function that signals progress when called. If progressr is not available
#'   or `enable = FALSE`, returns a no-op function.
#'
#' @details
#' This function wraps `progressr::progressor()` with a fallback for when progressr
#' is not available.
#'
#' @examples
#' \dontrun{
#' # Enable progress reporting
#' progressr::handlers("progress")
#'
#' # Use in a loop
#' p <- create_progressor(10)
#' results <- lapply(1:10, function(i) {
#'   Sys.sleep(0.1)
#'   p()  # Signal progress
#'   i^2
#' })
#' }
#'
#' @seealso [progressr::progressor()], [with_progress()]
#'
#' @keywords internal
create_progressor <- function(steps, enable = TRUE) {
  if (enable && requireNamespace("progressr", quietly = TRUE)) {
    progressr::progressor(steps = steps)
  } else {
    # Return no-op function
    function(...) invisible(NULL)
  }
}

#' Execute Function with Progress Reporting for Each Item
#'
#' @description
#' Helper function that wraps a function to report progress after each invocation.
#' Useful for integrating progress reporting with `purrr::map()` or `furrr::future_map()`.
#'
#' @param .f Function to wrap
#' @param .p Progressor function created by `create_progressor()` or `progressr::progressor()`
#'
#' @return A wrapped function that calls `.f(x)` and then signals progress via `.p()`.
#'
#' @details
#' This is an internal utility function used by riemtan's parallel processing functions.
#'
#' @examples
#' \dontrun{
#' progressr::handlers("progress")
#'
#' result <- with_progress({
#'   p <- progressr::progressor(steps = 10)
#'   wrapped_fn <- with_progress_signal(sqrt, p)
#'   lapply(1:10, wrapped_fn)
#' })
#' }
#'
#' @keywords internal
with_progress_signal <- function(.f, .p) {
  function(x, ...) {
    result <- .f(x, ...)
    .p()
    result
  }
}

#' Check if Progress Reporting is Available
#'
#' @description
#' Checks whether the progressr package is available and can be used for progress reporting.
#'
#' @return Logical value: TRUE if progressr is available, FALSE otherwise.
#'
#' @examples
#' if (is_progress_available()) {
#'   message("Progress reporting is available")
#' }
#'
#' @export
is_progress_available <- function() {
  requireNamespace("progressr", quietly = TRUE)
}

#' Configure Progress Handlers
#'
#' @description
#' Convenience function to configure progressr handlers for the current session.
#'
#' @param handler Character string specifying the handler type:
#'   - `"progress"`: progress package style (if available)
#'   - `"txtprogressbar"`: base R text progress bar
#'   - `"cli"`: cli package style (if available)
#'   - `"none"`: Disable progress reporting
#'   - `NULL`: Use progressr default handlers
#' @param ... Additional arguments passed to `progressr::handlers()`
#'
#' @return Invisibly returns the configured handlers (via `progressr::handlers()`).
#'
#' @details
#' This is a convenience wrapper around `progressr::handlers()`.
#' If progressr is not available, a warning is issued and the function returns NULL.
#'
#' @examples
#' \dontrun{
#' # Use base R text progress bar
#' configure_progress("txtprogressbar")
#'
#' # Use cli style (if cli package is installed)
#' configure_progress("cli")
#'
#' # Disable progress reporting
#' configure_progress("none")
#' }
#'
#' @seealso [progressr::handlers()]
#'
#' @export
configure_progress <- function(handler = "txtprogressbar", ...) {
  if (!is_progress_available()) {
    warning("progressr package is not available. Install it with: install.packages('progressr')")
    return(invisible(NULL))
  }

  if (is.null(handler)) {
    return(invisible(progressr::handlers(...)))
  }

  if (handler == "none") {
    progressr::handlers(list())
  } else {
    progressr::handlers(handler, ...)
  }

  invisible(progressr::handlers())
}
