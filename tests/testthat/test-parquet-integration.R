test_that("CSample works with ListBackend (backwards compatibility)", {
  # Create test matrices
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  # Create CSample the old way (should use ListBackend internally)
  data(airm)
  sample <- CSample$new(conns = mats, metric_obj = airm)

  expect_s3_class(sample, "CSample")
  expect_equal(sample$sample_size, 2)
  expect_equal(sample$matrix_size, 3)
})

test_that("CSample works with explicit ListBackend", {
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  backend <- ListBackend$new(mats)
  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  expect_s3_class(sample, "CSample")
  expect_equal(sample$sample_size, 2)
  expect_equal(sample$matrix_size, 3)
})

test_that("CSample works with ParquetBackend", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  expect_s3_class(sample, "CSample")
  expect_equal(sample$sample_size, 3)
  expect_equal(sample$matrix_size, 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample lazy-loads connectomes from ParquetBackend", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir, cache_size = 1)

  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  # Connectomes should be NULL initially (lazy)
  # Access via active binding triggers loading
  conns <- sample$connectomes
  expect_type(conns, "list")
  expect_equal(length(conns), 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample compute_tangents works with ParquetBackend", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  # Compute tangents
  ref_pt <- (diag(3) * 1.5) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  sample$compute_tangents(ref_pt)

  expect_equal(length(sample$tangent_images), 2)
  expect_s4_class(sample$tangent_images[[1]], "dspMatrix")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample compute_fmean works with ParquetBackend", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()

  # Create test matrices
  set.seed(42)
  base_mat <- diag(3)
  mats <- lapply(1:5, function(i) {
    mat <- base_mat + rnorm(9, 0, 0.1) |> matrix(3, 3)
    mat <- (mat + t(mat)) / 2  # Make symmetric
    mat <- mat + diag(3) * 0.5  # Ensure positive definite
    Matrix::nearPD(mat)$mat |> Matrix::pack()
  })

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  sample$compute_fmean(tol = 0.1, max_iter = 10)

  expect_s4_class(sample$frechet_mean, "dppMatrix")
  expect_equal(nrow(sample$frechet_mean), 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample compute_vecs works with ParquetBackend", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  sample$compute_tangents()
  sample$compute_vecs()

  expect_true(is.matrix(sample$vector_images))
  expect_equal(nrow(sample$vector_images), 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample validation rejects both backend and conns", {
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())
  backend <- ListBackend$new(mats)

  data(airm)
  expect_error(
    CSample$new(backend = backend, conns = mats, metric_obj = airm),
    "Cannot provide both 'backend' and 'conns'"
  )
})

test_that("CSample with ParquetBackend rejects tan_imgs and vec_imgs", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())
  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  data(airm)
  tan_imgs <- list(Matrix::pack(Matrix::symmpart(diag(3))))

  expect_error(
    CSample$new(backend = backend, tan_imgs = tan_imgs, metric_obj = airm),
    "tan_imgs and vec_imgs must be NULL"
  )

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSuperSample works with samples using ParquetBackend", {
  skip_if_not_installed("arrow")

  # Create two temporary directories with different samples
  temp_dir1 <- tempfile()
  temp_dir2 <- tempfile()

  mats1 <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  mats2 <- list(
    (diag(3) * 3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 4) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats1, temp_dir1, overwrite = TRUE)
  write_connectomes_to_parquet(mats2, temp_dir2, overwrite = TRUE)

  backend1 <- ParquetBackend$new(temp_dir1)
  backend2 <- ParquetBackend$new(temp_dir2)

  data(airm)
  sample1 <- CSample$new(backend = backend1, metric_obj = airm)
  sample2 <- CSample$new(backend = backend2, metric_obj = airm)

  super_sample <- CSuperSample$new(list(sample1, sample2))

  expect_s3_class(super_sample, "CSuperSample")
  expect_equal(super_sample$sample_size, 4)
  expect_equal(length(super_sample$list_of_samples), 2)

  # Clean up
  unlink(temp_dir1, recursive = TRUE)
  unlink(temp_dir2, recursive = TRUE)
})

test_that("CSuperSample gather() works with ParquetBackend samples", {
  skip_if_not_installed("arrow")

  temp_dir1 <- tempfile()
  temp_dir2 <- tempfile()

  mats1 <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())
  mats2 <- list((diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  write_connectomes_to_parquet(mats1, temp_dir1, overwrite = TRUE)
  write_connectomes_to_parquet(mats2, temp_dir2, overwrite = TRUE)

  backend1 <- ParquetBackend$new(temp_dir1)
  backend2 <- ParquetBackend$new(temp_dir2)

  data(airm)
  sample1 <- CSample$new(backend = backend1, metric_obj = airm)
  sample2 <- CSample$new(backend = backend2, metric_obj = airm)

  super_sample <- CSuperSample$new(list(sample1, sample2))
  super_sample$gather()

  expect_s3_class(super_sample$full_sample, "CSample")
  expect_equal(length(super_sample$full_sample$connectomes), 2)

  # Clean up
  unlink(temp_dir1, recursive = TRUE)
  unlink(temp_dir2, recursive = TRUE)
})

test_that("Mixed backends work in CSuperSample", {
  skip_if_not_installed("arrow")

  # One sample with ListBackend, one with ParquetBackend
  temp_dir <- tempfile()

  mats_list <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())
  mats_parquet <- list((diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  write_connectomes_to_parquet(mats_parquet, temp_dir, overwrite = TRUE)

  backend_parquet <- ParquetBackend$new(temp_dir)

  data(airm)
  sample_list <- CSample$new(conns = mats_list, metric_obj = airm)
  sample_parquet <- CSample$new(backend = backend_parquet, metric_obj = airm)

  super_sample <- CSuperSample$new(list(sample_list, sample_parquet))

  expect_s3_class(super_sample, "CSuperSample")
  expect_equal(super_sample$sample_size, 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("Full workflow: Parquet backend with all CSample operations", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()

  # Create diverse test matrices
  set.seed(123)
  mats <- lapply(1:10, function(i) {
    mat <- diag(4) + rnorm(16, 0, 0.05) |> matrix(4, 4)
    mat <- (mat + t(mat)) / 2
    mat <- mat + diag(4) * 0.5
    Matrix::nearPD(mat)$mat |> Matrix::pack()
  })

  # Write to Parquet with metadata
  write_connectomes_to_parquet(
    mats, temp_dir,
    subject_ids = paste0("subj_", 1:10),
    provenance = list(study = "Integration Test", date = "2024-01-01"),
    overwrite = TRUE
  )

  # Create sample with ParquetBackend
  backend <- create_parquet_backend(temp_dir, cache_size = 3)
  data(airm)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  # Compute tangents
  sample$compute_tangents()
  expect_equal(length(sample$tangent_images), 10)

  # Compute vectors
  sample$compute_vecs()
  expect_equal(nrow(sample$vector_images), 10)

  # Compute Frechet mean
  sample$compute_fmean(tol = 0.1, max_iter = 20)
  expect_s4_class(sample$frechet_mean, "dppMatrix")

  # Center the sample
  sample$center()
  expect_true(sample$is_centered)

  # Compute variation
  sample$compute_variation()
  expect_true(is.numeric(sample$variation))
  expect_true(sample$variation > 0)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
