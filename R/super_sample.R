#' CSuperSample Class
#'
#' @description
#' This class represents a collection of CSample objects (samples of connectomes), providing methods to aggregate, analyze, and compute statistics across multiple samples sharing the same Riemannian metric.
#' @export
CSuperSample <- R6::R6Class(
  classname = "CSuperSample",
  private = list(
    samples = NULL, n = NULL, p = NULL, d = NULL, geom = NULL, var = NULL,
    s_cov = NULL, gathered_sample = NULL, f_mean = NULL, W = NULL, T = NULL
  ),
  public = list(
    #' @description Initialize a CSuperSample object
    #'
    #' @param samples A list of CSample objects. All must use the same Riemannian metric.
    #' @return A new `CSuperSample` object.
    initialize = function(samples) {
      if (!is.list(samples)) stop("samples must be a list.")

      class_flag <- samples |>
        purrr::map_lgl(\(x) x |> inherits("CSample")) |>
        all()
      if (!class_flag) stop("samples must be a list of CSample objects")

      geoms <- samples |> purrr::map(\(x) x$riem_metric)
      geoms_flag <- geoms |>
        purrr::map_lgl(\(g) {
          g |>
            identical(geoms[[1]])
        }) |>
        all()
      if (!geoms_flag) stop("All Riemannian metrics must be the same")

      n <- samples |>
        purrr::map_dbl(\(x) x$sample_size) |>
        sum()
      p <- samples[[1]]$matrix_size
      d <- samples[[1]]$mfd_dim
      private$samples <- samples
      private$n <- n
      private$p <- p
      private$d <- d
      private$geom <- samples[[1]]$riem_metric
      private$var <- NULL
      private$s_cov <- NULL
      private$gathered_sample <- NULL
      private$f_mean <- NULL
      private$W <- NULL
      private$T <- NULL
    },

    #' @description Compute the total variation of the aggregated sample.
    #'
    #' @return None. This function is called for its side effects. The result is stored in the `variation` active binding.
    compute_variation = function() {
      if (self$full_sample |> is.null()) self$gather()
      self$full_sample$compute_variation()
      private$var <- self$full_sample$variation
    },

    #' @description Compute the sample covariance matrix of the aggregated sample.
    #'
    #' @return None. This function is called for its side effects. The result is stored in the `sample_cov` active binding.
    compute_sample_cov = function() {
      if (self$full_sample |> is.null()) self$gather()
      self$full_sample$compute_sample_cov()
      private$s_cov <- self$full_sample$sample_cov
    },

    #' @description Gather all connectomes from the list of samples into a single CSample object.
    #'
    #' @return None. This function is called for its side effects. The result is stored in the `full_sample` active binding.
    gather = function() {
      if (private$samples |> is.null()) stop("samples must be specified.")

      super_conns <- private$samples |> purrr::map(
        \(sample) {
          if (sample$connectomes |> is.null()) {
            if (sample$tangent_images |> (\(x) length(x) == 0)()) {
              # aux_sample <- CSample$new(
              #   vec_imgs = sample$vector_images,
              #   centered = sample$is_centered,
              #   metric_obj = sample$riem_metric
              #   ref_pt =
              # )
              # aux_sample$compute_unvecs()
              sample$compute_unvecs()
            } # else {
            #   aux_sample <- CSample$new(
            #     tan_imgs = sample$tangent_images,
            #     centered = sample$is_centered,
            #     metric_obj = sample$riem_metric
            #   )
            # }
            # aux_sample$compute_conns()
            sample$compute_conns()
          } # else {
          #   aux_sample <- sample
          # }
          # aux_sample$connectomes
          sample$connectomes
        }
      )
      private$gathered_sample <- CSample$new(
        conns = super_conns |> unlist(),
        metric_obj = self$riem_metric
      )
    },

    #' @description Compute the Frechet mean of the aggregated sample.
    #'
    #' @param batch_size Optional batch size parameter passed to the underlying compute_fmean function.
    #' @return None. This function is called for its side effects. The result is stored in the `frechet_mean` active binding.
    compute_fmean = function(batch_size = NULL) {
      if (private$gathered_sample |> is.null()) self$gather()

      private$gathered_sample$compute_fmean(batch_size = batch_size)
      private$f_mean <- private$gathered_sample$frechet_mean
    },

    #' @description Compute the within-group covariance matrix (W) for the samples.
    #'
    #' @return None. This function is called for its side effects. The result is stored in the `Within` active binding.
    compute_W = function() {
      private$W <- private$samples |>
        purrr::map(
          \(sam) {
            if (sam$sample_cov |> is.null()) sam$compute_sample_cov()
            (sam$sample_size - 1) * sam$sample_cov
          }
        ) |>
        Reduce(`+`, x = _) |>
        Matrix::nearPD() |>
        _$mat |>
        Matrix::pack()
    },

    #' @description Compute the total covariance matrix (T) for the samples.
    #'
    #' @return None. This function is called for its side effects. The result is stored in the `Total` active binding.
    compute_T = function() {
      if (private$gathered_sample |> is.null()) {
        self$gather()
        self$full_sample$compute_tangents(self$full_sample$connectomes[[1]])
      }
      if (private$gathered_sample$frechet_mean |> is.null()) {
        self$compute_fmean()
      }
      # Rprof(path)

      v <- tryCatch({
        private$samples |>
          purrr::imap(
            \(sam, idx) {
              tryCatch({
                if (sam$frechet_mean |> is.null()) sam$compute_fmean()
                list(
                  sam$frechet_mean,
                  self$riem_metric$log(sam$frechet_mean, self$frechet_mean)
                )
              }, error = function(e) {
                stop(sprintf("Error computing log map for sample %d: %s", idx, e$message))
              })
            }
          ) |>
          purrr::imap(\(l, idx) {
            tryCatch({
              do.call(self$riem_metric$vec, args = l)
            }, error = function(e) {
              stop(sprintf("Error vectorizing tangent vector for sample %d: %s", idx, e$message))
            })
          })
      }, error = function(e) {
        stop(sprintf("Error in compute_T while computing tangent vectors: %s", e$message))
      })
      # Rprof(NULL)
      # sink(file.path("profiles", basename(path)))
      # summaryRprof(path) |> print()
      # sink()
      u <- tryCatch({
        private$samples |> purrr::imap(
          \(sam, idx) {
            tryCatch({
              if ((sam$is_centered |> is.null()) || !sam$is_centered) sam$center()
              if (sam$vector_images |> is.null()) {
                sam$compute_tangents()
                sam$compute_vecs()
              }
              sam$vector_images
            }, error = function(e) {
              stop(sprintf("Error processing vector images for sample %d: %s", idx, e$message))
            })
          }
        )
      }, error = function(e) {
        stop(sprintf("Error in compute_T while processing vector images: %s", e$message))
      })

      private$T <- tryCatch({
        purrr::map2(
          u, v, function(x, y) {
            tryCatch({
              sweep(x, 2, y, FUN = "-")
            }, error = function(e) {
              stop(sprintf("Error in sweep operation: %s", e$message))
            })
          }
        ) |>
          purrr::imap(
            \(m, idx) {
              tryCatch({
                if (nrow(m) == 0) {
                  stop(sprintf("Sample %d has no rows in matrix", idx))
                }
                1:nrow(m) |>
                  purrr::map(
                    \(i) matrix(m[i, ], ncol = 1) %*% matrix(m[i, ], nrow = 1)
                  ) |>
                  Reduce(`+`, x = _)
              }, error = function(e) {
                stop(sprintf("Error computing outer products for sample %d: %s", idx, e$message))
              })
            }
          ) |>
          Reduce(`+`, x = _) |>
          Matrix::nearPD() |>
          _$mat |>
          Matrix::pack()
      }, error = function(e) {
        stop(sprintf("Error in compute_T while computing total covariance matrix: %s", e$message))
      })
    }
  ),
  active = list(
    #' @field list_of_samples The list of CSample objects aggregated in this super-sample.
    list_of_samples = function() private$samples,

    #' @field sample_size The total number of connectomes in all samples.
    sample_size = function() private$n,

    #' @field matrix_size The size of the connectome matrices.
    matrix_size = function() private$p,

    #' @field mfd_dim The dimension of the manifold.
    mfd_dim = function() private$d,

    #' @field riem_metric The Riemannian metric used by all samples.
    riem_metric = function() private$geom,

    #' @field variation The total variation of the aggregated sample.
    variation = function() private$var,

    #' @field sample_cov The sample covariance matrix of the aggregated sample.
    sample_cov = function() private$s_cov,

    #' @field full_sample The aggregated CSample object containing all connectomes.
    full_sample = function() private$gathered_sample,

    #' @field frechet_mean The Frechet mean of the aggregated sample.
    frechet_mean = function() private$f_mean,

    #' @field Within The within-group covariance matrix (W).
    Within = function() private$W,

    #' @field Total The total covariance matrix (T).
    Total = function() private$T
  )
)