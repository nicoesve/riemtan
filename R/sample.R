#' CSample Class
#'
#' @description
#' This class represents a sample of connectomes, with various properties and methods to handle their tangent and vectorized images.
#' @export
CSample <- R6::R6Class(
  classname = "CSample",
  private = list(
    conns = NULL, vec_imgs = NULL,
    n = NULL, p = NULL, d = NULL, centered = NULL,
    f_mean = NULL, metric_obj = NULL, var = NULL, s_cov = NULL,
    tangent_handler = NULL,
    dists = NULL,
    ._batch_size = NULL
  ),
  public = list(
    #' @description Initialize a CSample object
    #'
    #' @param conns A list of connectomes (default is NULL).
    #' @param ref_pt A connectome (default is identity)
    #' @param tan_imgs A list of tangent images (default is NULL).
    #' @param vec_imgs A matrix whose rows are vectorized images (default is NULL).
    #' @param centered Boolean indicating whether tangent or vectorized images are centered (default is NULL).
    #' @param metric_obj Object of class `rmetric` representing the Riemannian metric used.
    #' @param batch_size The batch size for compute_fmean (default is 32).
    #'
    #' @return A new `CSample` object.
    initialize = function(conns = NULL, tan_imgs = NULL,
                          vec_imgs = NULL, centered = NULL,
                          ref_pt = NULL, metric_obj, batch_size = NULL) {
      # Validate and set the metric
      validate_metric(metric_obj)
      private$metric_obj <- metric_obj
      private$tangent_handler <- TangentImageHandler$new(metric_obj, ref_pt)


      # If connectomes are provided
      if (!is.null(conns)) {
        validate_conns(conns, tan_imgs, vec_imgs, centered)

        # Set dimensions and initialize properties
        n <- length(conns)
        p <- nrow(conns[[1]])
        d <- p * (p + 1) / 2
        private$conns <- conns
        private$vec_imgs <- NULL
        private$n <- n
        private$p <- p
        private$d <- d
        private$centered <- NULL
        private$f_mean <- NULL
        private$var <- NULL
        private$s_cov <- NULL
        private$dists <- NULL
        private$._batch_size <- if (is.null(batch_size)) n else batch_size

        # If tangent images are provided
      } else if (!is.null(tan_imgs)) {
        validate_tan_imgs(tan_imgs, vec_imgs, centered)

        # Set dimensions and initialize properties
        n <- length(tan_imgs)
        p <- nrow(ref_pt)
        d <- p * (p + 1) / 2
        frechet_mean <- if (centered) ref_pt else NULL
        private$conns <- NULL
        private$vec_imgs <- NULL
        private$n <- n
        private$p <- p
        private$d <- d
        private$centered <- centered
        private$f_mean <- frechet_mean
        private$var <- NULL
        private$s_cov <- NULL
        private$dists <- NULL

        # Set tangent images in the handler
        private$tangent_handler$set_tangent_images(
          ref_pt, tan_imgs
        )

        # If vector images are provided
      } else {
        validate_vec_imgs(vec_imgs, centered)

        # Set dimensions and initialize properties
        n <- nrow(vec_imgs)
        p <- nrow(private$tangent_handler$ref_point)
        d <- p * (p + 1) / 2

        # Check if dimensions match
        dims_flag <- d == (vec_imgs |> ncol())
        if (!dims_flag) stop("Dimensions don't match")

        frechet_mean <- if (centered) ref_pt else NULL

        private$conns <- NULL
        private$vec_imgs <- vec_imgs
        private$n <- n
        private$p <- p
        private$d <- d
        private$centered <- centered
        private$f_mean <- frechet_mean
        private$var <- NULL
        private$s_cov <- NULL
        private$dists <- NULL
        private$tangent_handler$set_reference_point(ref_pt)
      }
    },

    #' @description
    #' This function computes the tangent images from the connectomes.
    #'
    #' @param ref_pt A reference point, which must be a `dppMatrix` object (default is `default_ref_pt`).
    #'
    #' @return None
    #' @details Error if `ref_pt` is not a `dppMatrix` object or if `conns` is not specified.
    compute_tangents = function(ref_pt = default_ref_pt(private$p)) {
      if (!inherits(ref_pt, "dppMatrix")) {
        stop("ref_pt must be a dppMatrix object.")
      }
      if (is.null(private$conns)) stop("conns must be specified.")
      private$tangent_handler$set_reference_point(ref_pt)
      private$tangent_handler$compute_tangents(private$conns)
    },

    #' @description
    #' This function computes the connectomes from the tangent images.
    #'
    #' @return None
    #' @details Error if tangent images are not specified.
    compute_conns = function() {
      if (is.null(private$tangent_handler$tangent_images)) {
        stop("tangent images must be specified.")
      }
      private$conns <- private$tangent_handler$compute_conns()
    },

    #' @description
    #' This function computes the vectorized tangent images from the tangent images.
    #'
    #' @return None
    #' @details Error if tangent images are not specified.
    compute_vecs = function() {
      if (is.null(private$tangent_handler$tangent_images)) {
        stop("tangent images must be specified.")
      }
      private$vec_imgs <- private$tangent_handler$compute_vecs()
    },

    #' @description
    #' This function computes the tangent images from the vector images.
    #'
    #' @return None
    #' @details Error if `vec_imgs` is not specified.
    compute_unvecs = function() {
      if (is.null(self$vector_images)) stop("vec_imgs must be specified.")
      private$tangent_handler$set_tangent_images(
        private$tangent_handler$ref_point,
        1:nrow(self$vector_images) |>
          purrr::map(\(i) {
            private$metric_obj$unvec(
              private$tangent_handler$ref_point,
              self$vector_images[i, ]
            )
          })
      )
    },

    #' @description
    #' This function computes the Frechet mean of the sample.
    #'
    #' @param tol Tolerance for the convergence of the mean (default is 0.05).
    #' @param max_iter Maximum number of iterations for the computation (default is 20).
    #' @param lr Learning rate for the optimization algorithm (default is 0.2).
    #' @param batch_size The batch size (default is the instance's batch_size).
    #'
    #' @return None
    compute_fmean = function(tol = 0.05, max_iter = 20, lr = 0.2, batch_size = NULL) {
      if (is.null(batch_size)) {
        batch_size <- if (is.null(private$._batch_size)) private$n else private$._batch_size
      }
      private$f_mean <- compute_frechet_mean(self, tol, max_iter, lr, batch_size)
    },

    #' @description
    #' This function changes the reference point for the tangent images.
    #'
    #' @param new_ref_pt A new reference point, which must be a `dppMatrix` object.
    #'
    #' @return None
    #' @details Error if tangent images have not been computed or if `new_ref_pt` is not a `dppMatrix` object.
    change_ref_pt = function(new_ref_pt) {
      if (is.null(private$tangent_handler$tangent_images)) {
        stop("tangent images have not been computed")
      }
      if (!inherits(new_ref_pt, "dppMatrix")) {
        stop("new_ref_pt must be a dppMatrix object.")
      }
      private$tangent_handler$relocate_tangents(new_ref_pt)
    },

    #' @description Center the sample
    #'
    #' @return None. This function is called for its side effects.
    #' @details This function centers the sample by computing the Frechet mean if it is not already computed, and then changing the reference point to the computed Frechet mean. Error if tangent images are not specified. Error if the sample is already centered.
    center = function() {
      if (is.null(private$tangent_handler$tangent_images)) {
        stop("Tangent images must be specified.")
      }
      if (!is.null(private$centered) && private$centered) {
        stop("The sample is already centered.")
      }
      if (is.null(private$f_mean)) {
        self$compute_fmean()
      }
      self$change_ref_pt(private$f_mean)
      private$centered <- TRUE
    },

    #' @description Compute Variation
    #'
    #' @return None. This function is called for its side effects.
    #' @details This function computes the variation of the sample. It first checks if the vector images are null, and if so, it computes the vectors, computing first the tangent images if necessary. If the sample is not centered, it centers the sample and recomputes the vectors. Finally, it calculates the variation as the mean of the sum of squares of the vector images. Error if `vec_imgs` is not specified.
    compute_variation = function() {
      if (self$vector_images |> is.null()) {
        if (length(private$tangent_handler$tangent_images) == 0) {
          self$compute_tangents()
        }
        self$compute_vecs()
      }

      if (is.null(self$vector_images)) stop("vec_imgs must be specified.")
      if (is.null(self$is_centered) || !self$is_centered) {
        self$compute_unvecs()
        self$center()
        self$compute_vecs()
      }
      private$var <- self$vector_images |>
        apply(X = _, MARGIN = 1, FUN = function(x) sum(x^2)) |>
        mean()
    },

    #' @description Compute distances
    #'
    #' @return None. This function is called for its side effects.
    #' @details This function computes the distances of the elements of the sample to the Frechet mean. It first checks if the vector images are null, and if so, it computes the vectors, computing first the tangent images if necessary. If the sample is not centered, it centers the sample and recomputes the vectors. Finally, it calculates the distances as the Euclidean norms of the vector images. Error if `vec_imgs` is not specified.
    #' @importFrom matrixStats rowSums2
    compute_dists = function() {
      if (self$vector_images |> is.null()) {
        if (length(private$tangent_handler$tangent_images) == 0) {
          self$compute_tangents()
        }
        self$compute_vecs()
      }

      if (is.null(self$vector_images)) stop("vec_imgs must be specified.")
      if (is.null(self$is_centered) || !self$is_centered) {
        self$compute_unvecs()
        self$center()
        self$compute_vecs()
      }
      private$dists <- self$vector_images |>
        (\(x) x^2)() |>
        matrixStats::rowSums2() |>
        purrr::map_dbl(sqrt)
    },

    #' @description Compute Sample Covariance
    #'
    #' @details This function computes the sample covariance matrix for the vector images. It first checks if the vector images are null, and if so, it computes the vectors, computing first the tangent images if necessary.
    #' @return None. This function is called for its side effects.
    compute_sample_cov = function() {
      if (self$vector_images |> is.null()) {
        if (length(private$tangent_handler$tangent_images) == 0) {
          if (!is.null(private$conns)) {
            self$compute_tangents()
          } else {
            stop("Cannot compute sample covariance: no connectomes, tangent images, or vector images available.")
          }
        }
        self$compute_vecs()
      }

      private$s_cov <- cov(private$vec_imgs)
    }
  ),
  active = list(
    #' @field connectomes Connectomes data
    connectomes = function() private$conns,

    #' @field tangent_images Tangent images data
    tangent_images = function() private$tangent_handler$tangent_images,

    #' @field vector_images Vector images data
    vector_images = function() private$vec_imgs,

    #' @field sample_size Sample size
    sample_size = function() private$n,

    #' @field matrix_size Matrix size
    matrix_size = function() private$p,

    #' @field mfd_dim Manifold dimension
    mfd_dim = function() private$d,

    #' @field is_centered Centering status
    is_centered = function() private$centered,

    #' @field frechet_mean Frechet mean
    frechet_mean = function() private$f_mean,

    #' @field riem_metric Riemannian Metric used
    riem_metric = function() private$metric_obj,

    #' @field variation Variation of the sample
    variation = function() private$var,

    #' @field sample_cov Sample covariance
    sample_cov = function() private$s_cov,

    #' @field ref_point Reference point for tangent or vectorized images
    ref_point = function() private$tangent_handler$ref_point,

    #' @field distances Squared distances to the Frechet mean
    distances = function() private$dists,

    #' @field batch_size Batch size for compute_fmean
    batch_size = function() private$._batch_size
  )
)
