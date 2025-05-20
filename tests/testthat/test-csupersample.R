test_pd_mats <- list(
  Matrix::Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack(),
  Matrix::Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
    Matrix::nearPD() |> _$mat |> Matrix::pack()
)

test_that("initalization works", {
  # load("data/test_data.RData")
  # load("data/airm.RData")
  # data("test_pd_mats")

  data("airm")
  s <- test_pd_mats |> CSample$new(metric_obj = airm)
  ss <- s |>
    (\(x) list(x, x))() |>
    CSuperSample$new()
  ss$list_of_samples |> expect_identical(list(s, s))
  ss$sample_size |> expect_equal(4)
  ss$matrix_size |> expect_equal(2)
  ss$mfd_dim |> expect_equal(3)
  ss$riem_metric |> expect_identical(airm)
  c(
    ss$variation, ss$sample_cov, ss$full_sample, ss$frechet_mean,
    ss$Within, ss$Total
  ) |>
    is.null() |>
    all() |>
    expect_true()
})

test_that("gathering works", {
  # load("data/test_data.RData")
  # load("data/airm.RData")
  # data("test_pd_mats")
  data("airm")
  s <- test_pd_mats |> CSample$new(metric_obj = airm)
  ss <- s |>
    (\(x) list(x, x))() |>
    CSuperSample$new()
  ss$gather()
  ss$full_sample |> expect_equal(
    list(test_pd_mats, test_pd_mats) |>
      unlist() |>
      CSample$new(metric_obj = airm)
  )
})

test_that("computing simple statistics", {
  # load("data/test_data.RData"); load("data/airm.RData")
  # data("test_pd_mats");
  data("airm")
  sam1 <- test_pd_mats |>
    purrr::map(\(x) 2 * x) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |> CSample$new(metric_obj = airm)
  ss <- list(sam1, sam2) |> CSuperSample$new()
  ss$compute_variation()
  ss$compute_sample_cov()
  ss$variation |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("numeric") |> expect_true(),
      x |> expect_gt(0)
    )
  })()
  ss$sample_cov |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("matrix") |> expect_true(),
      x |> isSymmetric() |> expect_true()
    )
  })()
})

test_that("computation of advanced statistics works", {
  # load("data/test_data.RData"); load("data/airm.RData")
  # data("test_pd_mats");
  data("airm")
  sam1 <- test_pd_mats |>
    purrr::map(\(x) 2 * x) |>
    CSample$new(metric_obj = airm)
  sam2 <- test_pd_mats |> CSample$new(metric_obj = airm)
  ss <- list(sam1, sam2) |> CSuperSample$new()
  ss$compute_fmean()
  ss$frechet_mean |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("dppMatrix") |> expect_true()
    )
  })()
  ss$compute_W()
  ss$Within |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("dppMatrix") |> expect_true()
    )
  })()
  ss$compute_T()
  ss$Total |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("dppMatrix") |> expect_true()
    )
  })()
  ss$Log_Wilks_Lambda() |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("numeric") |> expect_true(),
      x |> expect_lt(0)
    )
  })()
  ss$Pillais_Trace() |> (\(x) {
    list(
      x |> is.null() |> expect_false(),
      x |> inherits("numeric") |> expect_true(),
      x |> expect_gt(0)
    )
  })()
})