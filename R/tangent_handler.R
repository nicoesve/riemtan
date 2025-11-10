#' TangentImageHandler Class
#'
#' This class handles tangent images on a manifold. It provides methods to set a reference point, compute tangents, and perform various operations using a provided metric.
TangentImageHandler <- R6::R6Class(
  classname = "TangentImageHandler",
  private = list(
    reference_point = NULL, # The current reference point on the manifold
    tan_images = NULL, # List of tangent images (dspMatrix objects)
    metric_obj = NULL # Metric object for operations
  ),
  public = list(
    #' Initialize the TangentImageHandler
    #'
    #' @param metric_obj An rmetric object for operations.
    #' @param reference_point An optional reference point on the manifold.
    #' @return A new instance of TangentImageHandler.
    initialize = function(metric_obj, reference_point = NULL) {
      if (is.null(metric_obj)) stop("Metric object must be provided.")
      private$metric_obj <- metric_obj
      private$reference_point <- reference_point
      private$tan_images <- list()
    },

    #' Set a new reference point.
    #'
    #' @description If tangent images have been created, it recomputes them by mapping to the manifold and then to the new tangent space.
    #'
    #' @param new_ref_pt A new reference point of class dppMatrix.
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return None.
    set_reference_point = function(new_ref_pt, progress = FALSE) {
      if (!inherits(new_ref_pt, "dppMatrix")) {
        stop("Reference point must be of class dppMatrix.")
      }
      if (!is.null(private$reference_point) &&
        !is.null(private$tan_images)) {
        n <- length(private$tan_images)

        if (should_parallelize(n)) {
          # Parallel relocation
          private$tan_images <- with_progress({
            p <- create_progressor(n, enable = progress)
            furrr::future_map(
              private$tan_images,
              \(tan) {
                # Map to manifold, then to new tangent space
                intermediate <- private$metric_obj$exp(private$reference_point, tan)
                result <- private$metric_obj$log(new_ref_pt, intermediate)
                p()
                result
              },
              .options = furrr::furrr_options(seed = TRUE)
            )
          }, name = "Relocating tangent images", enable = progress)
        } else {
          # Sequential relocation
          private$tan_images <- private$tan_images |>
            purrr::map(
              \(tan) private$metric_obj$exp(private$reference_point, tan)
            ) |>
            purrr::map(\(point) private$metric_obj$log(new_ref_pt, point))
        }
      }
      private$reference_point <- new_ref_pt
    },

    #' Computes the tangent images from the points in the manifold
    #'
    #' @param manifold_points A list of connectomes
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return None
    compute_tangents = function(manifold_points, progress = FALSE) {
      if (is.null(private$reference_point)) {
        stop("Reference point must be set before computing tangents.")
      }

      n <- length(manifold_points)

      if (should_parallelize(n)) {
        # Parallel computation
        private$tan_images <- with_progress({
          p <- create_progressor(n, enable = progress)
          furrr::future_map(
            manifold_points,
            \(point) {
              result <- private$metric_obj$log(private$reference_point, point)
              p()
              result
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
        }, name = "Computing tangent images", enable = progress)
      } else {
        # Sequential computation
        private$tan_images <- purrr::map(
          manifold_points,
          \(point) private$metric_obj$log(private$reference_point, point)
        )
      }
    },

    #' Computes vectorizations from tangent images
    #'
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return A matrix, each row of which is a vectorization
    compute_vecs = function(progress = FALSE) {
      x <- self$tangent_images
      n <- length(x)

      if (should_parallelize(n)) {
        # Parallel computation
        y <- with_progress({
          p <- create_progressor(n, enable = progress)
          furrr::future_map(
            x,
            \(tan) {
              result <- private$metric_obj$vec(private$reference_point, tan)
              p()
              result
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
        }, name = "Computing vectorizations", enable = progress)
      } else {
        # Sequential computation
        y <- purrr::map(x, \(tan) private$metric_obj$vec(private$reference_point, tan))
      }

      do.call(rbind, args = y)
    },

    #' Computes connectomes from tangent images
    #'
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @details Error if the tangent images have not been specified
    #' @return A list of connectomes
    compute_conns = function(progress = FALSE) {
      if (is.null(private$tan_images)) {
        stop("tangent images must be specified.")
      }

      n <- length(private$tan_images)

      if (should_parallelize(n)) {
        # Parallel computation
        with_progress({
          p <- create_progressor(n, enable = progress)
          furrr::future_map(
            private$tan_images,
            \(tan) {
              result <- private$metric_obj$exp(private$reference_point, tan)
              p()
              result
            },
            .options = furrr::furrr_options(seed = TRUE)
          )
        }, name = "Computing connectomes", enable = progress)
      } else {
        # Sequential computation
        purrr::map(
          private$tan_images,
          \(tan) private$metric_obj$exp(private$reference_point, tan)
        )
      }
    },

    #' Setter for the tangent images
    #'
    #' @param reference_point A connectome
    #' @param tangent_images A list of tangent images
    #' @details Error if the reference point is not an object of class dppMatrix
    #' @return None
    set_tangent_images = function(reference_point, tangent_images) {
      if (!inherits(reference_point, "dppMatrix")) {
        stop("Reference point must be of class dppMatrix.")
      }
      class_flag <- tangent_images |>
        purrr::map_lgl(\(x) inherits(x, "dspMatrix")) |>
        all()
      if (!class_flag) {
        stop("All tangent images must be of class dspMatrix.")
      }
      private$reference_point <- reference_point
      private$tan_images <- tangent_images
    },

    #' Appends a matrix to the list of tangent images
    #'
    #' @param image Matrix to be added
    #' @details Error if the matrix is not of type dspMatrix
    add_tangent_image = function(image) {
      if (!inherits(image, "dspMatrix")) {
        stop("Tangent image must be of class dspMatrix.")
      }
      private$tan_images <- c(private$tan_images, list(image))
    },

    #' Tangent images getter
    #'
    #' @return list of tangent matrices
    get_tangent_images = function() {
      private$tan_images
    },

    #' Wrapper for set_reference_point
    #'
    #' @param new_ref_pt The new reference point
    #' @param progress Logical indicating whether to show progress (default: FALSE)
    #' @return None
    relocate_tangents = function(new_ref_pt, progress = FALSE) {
      self$set_reference_point(new_ref_pt, progress = progress)
    }
  ),
  active = list(
    #' @field ref_point A matrix of type dppMatrix
    ref_point = function() {
      private$reference_point
    },

    #' @field tangent_images A list of dspMatrix objects
    tangent_images = function() {
      self$get_tangent_images()
    }
  )
)
