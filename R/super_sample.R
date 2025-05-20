CSuperSample <- R6::R6Class(
  classname = "CSuperSample",
  private = list(
    samples = NULL, n = NULL, p = NULL, d = NULL, geom = NULL, var = NULL,
    s_cov = NULL, gathered_sample = NULL, f_mean = NULL, W = NULL, T = NULL
  ),
  public = list(
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
    compute_variation = function() {
      if (self$full_sample |> is.null()) self$gather()
      self$full_sample$compute_variation()
      private$var <- self$full_sample$variation
    },
    compute_sample_cov = function() {
      if (self$full_sample |> is.null()) self$gather()
      self$full_sample$compute_sample_cov()
      private$s_cov <- self$full_sample$sample_cov
    },
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
    compute_fmean = function() {
      if (private$gathered_sample |> is.null()) self$gather()

      private$gathered_sample$compute_fmean()
      private$f_mean <- private$gathered_sample$frechet_mean
    },
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
    compute_T = function() {
      if (private$gathered_sample |> is.null()) {
        self$gather()
        self$full_sample$compute_tangents(self$full_sample$connectomes[[1]])
      }
      if (private$gathered_sample$frechet_mean |> is.null()) {
        self$compute_fmean()
      }
      # Rprof(path)

      v <- private$samples |>
        purrr::map(
          \(sam) {
            if (sam$frechet_mean |> is.null()) sam$compute_fmean()
            list(
              sam$frechet_mean,
              self$riem_metric$log(sam$frechet_mean, self$frechet_mean)
            )
          }
        ) |>
        purrr::map(\(l) do.call(self$riem_metric$vec, args = l))
      # Rprof(NULL)
      # sink(file.path("profiles", basename(path)))
      # summaryRprof(path) |> print()
      # sink()
      u <- private$samples |> purrr::map(
        \(sam) {
          if ((sam$is_centered |> is.null()) || !sam$is_centered) sam$center()
          if (sam$vector_images |> is.null()) {
            sam$compute_tangents()
            sam$compute_vecs()
          }
          sam$vector_images
        }
      )

      private$T <- purrr::map2(
        u, v, function(x, y) sweep(x, 2, y, FUN = "-")
      ) |>
        purrr::map(
          \(m) {
            1:nrow(m) |>
              purrr::map(
                \(i) matrix(m[i, ], ncol = 1) %*% matrix(m[i, ], nrow = 1)
              ) |>
              Reduce(`+`, x = _)
          }
        ) |>
        Reduce(`+`, x = _) |>
        Matrix::nearPD() |>
        _$mat |>
        Matrix::pack()
    }
  ),
  active = list(
    list_of_samples = function() private$samples,
    sample_size = function() private$n,
    matrix_size = function() private$p,
    mfd_dim = function() private$d,
    riem_metric = function() private$geom,
    variation = function() private$var,
    sample_cov = function() private$s_cov,
    full_sample = function() private$gathered_sample,
    frechet_mean = function() private$f_mean,
    Within = function() private$W,
    Total = function() private$T
  )
)