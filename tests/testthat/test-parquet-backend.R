test_that("DataBackend is abstract and cannot be instantiated directly", {
  backend <- DataBackend$new()

  # Should throw errors for unimplemented methods
  expect_error(backend$get_matrix(1))
  expect_error(backend$get_all_matrices())
  expect_error(backend$length())
  expect_error(backend$get_dimensions())
})

test_that("ListBackend initializes correctly with valid matrices", {
  # Create test matrices
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  backend <- ListBackend$new(mats)

  expect_s3_class(backend, "ListBackend")
  expect_equal(backend$length(), 3)
  expect_equal(backend$get_dimensions(), 3)
})

test_that("ListBackend validates input matrices", {
  # Non-list input
  expect_error(ListBackend$new(matrix(1:9, 3, 3)), "matrices must be a list")

  # Empty list
  expect_error(ListBackend$new(list()), "matrices list cannot be empty")

  # Non-dppMatrix objects
  mats <- list(matrix(1:9, 3, 3), matrix(1:9, 3, 3))
  expect_error(ListBackend$new(mats), "All matrices must be dppMatrix objects")
})

test_that("ListBackend get_matrix retrieves correct matrix", {
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  backend <- ListBackend$new(mats)

  mat1 <- backend$get_matrix(1)
  expect_s4_class(mat1, "dppMatrix")
  expect_equal(as.vector(mat1@x), as.vector(mats[[1]]@x))

  mat2 <- backend$get_matrix(2)
  expect_equal(as.vector(mat2@x), as.vector(mats[[2]]@x))
})

test_that("ListBackend get_matrix validates index bounds", {
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  backend <- ListBackend$new(mats)

  expect_error(backend$get_matrix(0), "Index .* out of bounds")
  expect_error(backend$get_matrix(3), "Index .* out of bounds")
  expect_error(backend$get_matrix(-1), "Index .* out of bounds")
})

test_that("ListBackend get_all_matrices returns all matrices", {
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  backend <- ListBackend$new(mats)
  all_mats <- backend$get_all_matrices()

  expect_equal(length(all_mats), 2)
  expect_s4_class(all_mats[[1]], "dppMatrix")
  expect_s4_class(all_mats[[2]], "dppMatrix")
})

test_that("ParquetBackend initializes with valid directory", {
  skip_if_not_installed("arrow")

  # Create temporary directory with test data
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create test matrices and write to Parquet
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)

  # Initialize backend
  backend <- ParquetBackend$new(temp_dir, cache_size = 5)

  expect_s3_class(backend, "ParquetBackend")
  expect_equal(backend$length(), 2)
  expect_equal(backend$get_dimensions(), 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend validates directory exists", {
  expect_error(ParquetBackend$new("/nonexistent/directory"), "Data directory does not exist")
})

test_that("ParquetBackend validates metadata.json exists", {
  skip_if_not_installed("arrow")

  # Create directory without metadata
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(ParquetBackend$new(temp_dir), "Metadata file not found")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend get_matrix loads from disk", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  dir.create(temp_dir)

  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  mat1 <- backend$get_matrix(1)
  expect_s4_class(mat1, "dppMatrix")
  expect_equal(nrow(mat1), 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend caches recently accessed matrices", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  dir.create(temp_dir)

  mats <- replicate(5, diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(), simplify = FALSE)
  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)

  backend <- ParquetBackend$new(temp_dir, cache_size = 2)

  # Access first matrix
  mat1 <- backend$get_matrix(1)
  expect_equal(backend$get_cache_size(), 1)

  # Access second matrix
  mat2 <- backend$get_matrix(2)
  expect_equal(backend$get_cache_size(), 2)

  # Access third matrix (should evict first)
  mat3 <- backend$get_matrix(3)
  expect_equal(backend$get_cache_size(), 2)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend clear_cache empties the cache", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  dir.create(temp_dir)

  mats <- replicate(3, diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(), simplify = FALSE)
  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)

  backend <- ParquetBackend$new(temp_dir)

  # Load some matrices
  backend$get_matrix(1)
  backend$get_matrix(2)
  expect_gt(backend$get_cache_size(), 0)

  # Clear cache
  backend$clear_cache()
  expect_equal(backend$get_cache_size(), 0)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend get_all_matrices loads all matrices", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  dir.create(temp_dir)

  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE)
  backend <- ParquetBackend$new(temp_dir)

  all_mats <- backend$get_all_matrices()

  expect_equal(length(all_mats), 3)
  expect_true(all(sapply(all_mats, inherits, "dppMatrix")))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend get_metadata returns metadata", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  dir.create(temp_dir)

  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())
  write_connectomes_to_parquet(
    mats, temp_dir,
    subject_ids = "subj_1",
    provenance = list(study = "test"),
    overwrite = TRUE
  )

  backend <- ParquetBackend$new(temp_dir)
  metadata <- backend$get_metadata()

  expect_equal(metadata$n_matrices, 1)
  expect_equal(metadata$matrix_dim, 3)
  expect_equal(metadata$subject_ids, "subj_1")
  expect_equal(metadata$provenance$study, "test")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
