test_that("write_connectomes_to_parquet creates directory structure", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()

  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    (diag(3) * 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  result <- write_connectomes_to_parquet(mats, temp_dir)

  # Check directory was created
  expect_true(dir.exists(temp_dir))

  # Check metadata.json exists
  expect_true(file.exists(file.path(temp_dir, "metadata.json")))

  # Check Parquet files exist
  expect_true(file.exists(file.path(temp_dir, "matrix_0001.parquet")))
  expect_true(file.exists(file.path(temp_dir, "matrix_0002.parquet")))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_connectomes_to_parquet validates input", {
  # Non-list input
  expect_error(
    write_connectomes_to_parquet(matrix(1:9, 3, 3), tempfile()),
    "connectomes must be a list"
  )

  # Empty list
  expect_error(
    write_connectomes_to_parquet(list(), tempfile()),
    "connectomes list cannot be empty"
  )

  # Non-dppMatrix objects
  mats <- list(matrix(1:9, 3, 3))
  expect_error(
    write_connectomes_to_parquet(mats, tempfile()),
    "All connectomes must be dppMatrix objects"
  )

  # Inconsistent dimensions
  mats <- list(
    diag(2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )
  expect_error(
    write_connectomes_to_parquet(mats, tempfile()),
    "All matrices must have the same dimensions"
  )
})

test_that("write_connectomes_to_parquet validates subject_ids length", {
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  expect_error(
    write_connectomes_to_parquet(mats, tempfile(), subject_ids = "only_one"),
    "subject_ids must have the same length as connectomes"
  )
})

test_that("write_connectomes_to_parquet respects overwrite parameter", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  # First write
  write_connectomes_to_parquet(mats, temp_dir)

  # Second write without overwrite should fail
  expect_error(
    write_connectomes_to_parquet(mats, temp_dir, overwrite = FALSE),
    "already exists"
  )

  # With overwrite should succeed (expect success, not silence - function produces progress messages)
  expect_error(write_connectomes_to_parquet(mats, temp_dir, overwrite = TRUE), NA)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_connectomes_to_parquet creates valid metadata", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(4) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(4) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(
    mats, temp_dir,
    subject_ids = c("subj_1", "subj_2"),
    provenance = list(study = "Test Study", version = "1.0")
  )

  metadata <- jsonlite::fromJSON(file.path(temp_dir, "metadata.json"))

  expect_equal(metadata$n_matrices, 2)
  expect_equal(metadata$matrix_dim, 4)
  expect_equal(metadata$file_pattern, "matrix_%04d.parquet")
  expect_equal(metadata$subject_ids, c("subj_1", "subj_2"))
  expect_equal(metadata$provenance$study, "Test Study")
  expect_equal(metadata$provenance$version, "1.0")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("write_connectomes_to_parquet with custom file pattern", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  write_connectomes_to_parquet(
    mats, temp_dir,
    file_pattern = "conn_%03d.parquet"
  )

  expect_true(file.exists(file.path(temp_dir, "conn_001.parquet")))

  metadata <- jsonlite::fromJSON(file.path(temp_dir, "metadata.json"))
  expect_equal(metadata$file_pattern, "conn_%03d.parquet")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("validate_parquet_directory accepts valid directory", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir)

  expect_true(validate_parquet_directory(temp_dir, verbose = FALSE))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("validate_parquet_directory rejects non-existent directory", {
  expect_false(validate_parquet_directory("/nonexistent/path", verbose = FALSE))
})

test_that("validate_parquet_directory rejects directory without metadata", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_false(validate_parquet_directory(temp_dir, verbose = FALSE))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("validate_parquet_directory rejects invalid metadata", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Write incomplete metadata
  incomplete_metadata <- list(n_matrices = 2)
  jsonlite::write_json(incomplete_metadata, file.path(temp_dir, "metadata.json"))

  expect_false(validate_parquet_directory(temp_dir, verbose = FALSE))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("validate_parquet_directory detects missing Parquet files", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir)

  # Delete one Parquet file
  file.remove(file.path(temp_dir, "matrix_0001.parquet"))

  expect_false(validate_parquet_directory(temp_dir, verbose = FALSE))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("validate_parquet_directory with verbose prints info", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  write_connectomes_to_parquet(
    mats, temp_dir,
    subject_ids = "subj_1",
    provenance = list(study = "test")
  )

  expect_message(
    validate_parquet_directory(temp_dir, verbose = TRUE),
    "All checks passed"
  )

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("create_parquet_backend creates backend from valid directory", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  write_connectomes_to_parquet(mats, temp_dir)

  backend <- create_parquet_backend(temp_dir, cache_size = 5)

  expect_s3_class(backend, "ParquetBackend")
  expect_equal(backend$length(), 2)
  expect_equal(backend$get_dimensions(), 3)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("create_parquet_backend validates directory by default", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(
    create_parquet_backend(temp_dir, validate = TRUE),
    "Directory validation failed"
  )

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("create_parquet_backend can skip validation", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()
  mats <- list(diag(3) |> Matrix::nearPD() |> _$mat |> Matrix::pack())

  write_connectomes_to_parquet(mats, temp_dir)

  # Should work without validation
  backend <- create_parquet_backend(temp_dir, validate = FALSE)
  expect_s3_class(backend, "ParquetBackend")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("round-trip: write then read produces equivalent matrices", {
  skip_if_not_installed("arrow")

  temp_dir <- tempfile()

  # Create test matrices with specific values
  mats <- list(
    matrix(c(2, 1, 1, 2), 2, 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack(),
    matrix(c(3, 0.5, 0.5, 3), 2, 2) |> Matrix::nearPD() |> _$mat |> Matrix::pack()
  )

  # Write to Parquet
  write_connectomes_to_parquet(mats, temp_dir)

  # Read back via backend
  backend <- create_parquet_backend(temp_dir)
  mat1_read <- backend$get_matrix(1)
  mat2_read <- backend$get_matrix(2)

  # Compare
  expect_equal(as.matrix(mat1_read), as.matrix(mats[[1]]), tolerance = 1e-10, check.attributes = FALSE)
  expect_equal(as.matrix(mat2_read), as.matrix(mats[[2]]), tolerance = 1e-10, check.attributes = FALSE)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})
