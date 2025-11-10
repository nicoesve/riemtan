# Tests for parallel processing functionality
# Tests cover: parallel_config.R, progress_utils.R, and parallel operations

library(testthat)
library(riemtan)
library(Matrix)

# Helper function to create test SPD matrices
create_test_spd <- function(p = 4, n = 10) {
  lapply(1:n, function(i) {
    mat <- diag(p) + matrix(rnorm(p*p, 0, 0.1), p, p)
    mat <- (mat + t(mat)) / 2
    mat <- mat + diag(p) * 0.5
    Matrix::nearPD(mat)$mat |> Matrix::pack()
  })
}

# ==============================================================================
# Test parallel_config.R functions
# ==============================================================================

test_that("set_parallel_plan() configures strategies correctly", {
  # Test sequential
  set_parallel_plan("sequential")
  expect_false(is_parallel_enabled())
  expect_equal(get_n_workers(), 1)

  # Test multisession
  set_parallel_plan("multisession", workers = 2)
  expect_true(is_parallel_enabled())
  expect_equal(get_n_workers(), 2)

  # Reset
  reset_parallel_plan()
  expect_false(is_parallel_enabled())
})

test_that("set_parallel_plan() validates strategy parameter", {
  expect_error(
    set_parallel_plan("invalid_strategy"),
    "Invalid strategy"
  )

  # future package validates workers >= 1
  expect_error(
    set_parallel_plan("multisession", workers = -1),
    "workers"
  )
})

test_that("is_parallel_enabled() detects parallel state correctly", {
  # Sequential
  set_parallel_plan("sequential")
  expect_false(is_parallel_enabled())

  # Parallel
  set_parallel_plan("multisession", workers = 2)
  expect_true(is_parallel_enabled())

  # Reset
  reset_parallel_plan()
  expect_false(is_parallel_enabled())
})

test_that("should_parallelize() respects threshold and parallel state", {
  # Sequential plan: never parallelize
  set_parallel_plan("sequential")
  expect_false(should_parallelize(100))
  expect_false(should_parallelize(100, threshold = 10))

  # Parallel plan: respect threshold
  set_parallel_plan("multisession", workers = 2)
  expect_true(should_parallelize(100, threshold = 10))
  expect_false(should_parallelize(5, threshold = 10))
  expect_true(should_parallelize(10, threshold = 10))  # Exactly at threshold

  # Reset
  reset_parallel_plan()
})

test_that("should_parallelize() validates inputs", {
  expect_error(should_parallelize(-1), "non-negative")
  expect_error(should_parallelize("100"), "numeric")
  expect_error(should_parallelize(100, threshold = -1), "positive")
  expect_error(should_parallelize(100, threshold = "10"), "numeric")
})

test_that("get_n_workers() returns correct count", {
  # Sequential
  set_parallel_plan("sequential")
  expect_equal(get_n_workers(), 1)

  # Multisession with 2 workers
  set_parallel_plan("multisession", workers = 2)
  expect_equal(get_n_workers(), 2)

  # Multisession with 4 workers
  set_parallel_plan("multisession", workers = 4)
  expect_equal(get_n_workers(), 4)

  # Reset
  reset_parallel_plan()
})

test_that("reset_parallel_plan() resets to sequential", {
  # Set parallel
  set_parallel_plan("multisession", workers = 4)
  expect_true(is_parallel_enabled())

  # Reset
  reset_parallel_plan()
  expect_false(is_parallel_enabled())
  expect_equal(get_n_workers(), 1)
})

# ==============================================================================
# Test progress_utils.R functions
# ==============================================================================

test_that("is_progress_available() checks progressr correctly", {
  has_progressr <- requireNamespace("progressr", quietly = TRUE)
  expect_equal(is_progress_available(), has_progressr)
})

test_that("create_progressor() handles enable parameter", {
  # Disabled: should return no-op function
  p <- create_progressor(10, enable = FALSE)
  expect_silent(p())  # Should not error

  # Enabled with progressr available
  if (requireNamespace("progressr", quietly = TRUE)) {
    p <- create_progressor(10, enable = TRUE)
    expect_silent(p())  # Should not error
  }
})

test_that("with_progress() handles enable parameter", {
  # Disabled
  result <- with_progress({
    1 + 1
  }, name = "Test", enable = FALSE)
  expect_equal(result, 2)

  # Enabled (may or may not have progressr)
  result <- with_progress({
    2 + 2
  }, name = "Test", enable = TRUE)
  expect_equal(result, 4)
})

# ==============================================================================
# Test parallel vs sequential equivalence
# ==============================================================================

test_that("Parallel and sequential compute_tangents() produce identical results", {
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 15)

  # Sequential
  set_parallel_plan("sequential")
  sample_seq <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_seq$compute_tangents()
  tangents_seq <- sample_seq$tangent_images

  # Parallel
  set_parallel_plan("multisession", workers = 2)
  sample_par <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_par$compute_tangents()
  tangents_par <- sample_par$tangent_images

  # Compare
  expect_equal(length(tangents_seq), length(tangents_par))
  for (i in seq_along(tangents_seq)) {
    expect_equal(
      as.matrix(tangents_seq[[i]]),
      as.matrix(tangents_par[[i]]),
      tolerance = 1e-10
    )
  }

  # Reset
  reset_parallel_plan()
})

test_that("Parallel and sequential compute_vecs() produce identical results", {
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 15)

  # Sequential
  set_parallel_plan("sequential")
  sample_seq <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_seq$compute_tangents()
  sample_seq$compute_vecs()
  vecs_seq <- sample_seq$vector_images

  # Parallel
  set_parallel_plan("multisession", workers = 2)
  sample_par <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_par$compute_tangents()
  sample_par$compute_vecs()
  vecs_par <- sample_par$vector_images

  # Compare
  expect_equal(vecs_seq, vecs_par, tolerance = 1e-10)

  # Reset
  reset_parallel_plan()
})

test_that("Parallel and sequential compute_fmean() produce identical results", {
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 15)

  # Sequential
  set_parallel_plan("sequential")
  sample_seq <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_seq$compute_fmean(tol = 0.01, max_iter = 50)
  fmean_seq <- sample_seq$frechet_mean

  # Parallel
  set_parallel_plan("multisession", workers = 2)
  sample_par <- CSample$new(conns = connectomes, metric_obj = airm)
  sample_par$compute_fmean(tol = 0.01, max_iter = 50)
  fmean_par <- sample_par$frechet_mean

  # Compare
  expect_equal(
    as.matrix(fmean_seq),
    as.matrix(fmean_par),
    tolerance = 1e-6  # Slightly looser tolerance for iterative algorithm
  )

  # Reset
  reset_parallel_plan()
})

test_that("Parallel and sequential relocate() produce identical results", {
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 15)

  # Create two reference points
  old_ref <- connectomes[[1]]
  new_ref <- connectomes[[2]]

  # Compute tangent images at old_ref
  tangents <- lapply(connectomes[3:15], function(conn) {
    airm$log(old_ref, conn)
  })

  # Sequential relocate
  set_parallel_plan("sequential")
  relocated_seq <- relocate(old_ref, new_ref, tangents, airm, progress = FALSE)

  # Parallel relocate
  set_parallel_plan("multisession", workers = 2)
  relocated_par <- relocate(old_ref, new_ref, tangents, airm, progress = FALSE)

  # Compare
  expect_equal(length(relocated_seq), length(relocated_par))
  for (i in seq_along(relocated_seq)) {
    expect_equal(
      as.matrix(relocated_seq[[i]]),
      as.matrix(relocated_par[[i]]),
      tolerance = 1e-10
    )
  }

  # Reset
  reset_parallel_plan()
})

# ==============================================================================
# Test ParquetBackend parallel operations
# ==============================================================================

test_that("ParquetBackend parallel loading produces correct results", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 12)

  # Write to Parquet
  temp_dir <- tempfile()
  dir.create(temp_dir)
  write_connectomes_to_parquet(
    connectomes,
    output_dir = temp_dir,
    subject_ids = paste0("subj_", 1:12),
    overwrite = TRUE
  )

  # Sequential loading
  backend_seq <- create_parquet_backend(temp_dir, cache_size = 5)
  set_parallel_plan("sequential")
  matrices_seq <- backend_seq$get_all_matrices(parallel = FALSE)

  # Parallel loading
  backend_par <- create_parquet_backend(temp_dir, cache_size = 5)
  set_parallel_plan("multisession", workers = 2)
  matrices_par <- backend_par$get_all_matrices(parallel = TRUE)

  # Compare
  expect_equal(length(matrices_seq), length(matrices_par))
  for (i in seq_along(matrices_seq)) {
    expect_equal(
      as.matrix(matrices_seq[[i]]),
      as.matrix(matrices_par[[i]]),
      tolerance = 1e-10
    )
  }

  # Cleanup
  reset_parallel_plan()
  unlink(temp_dir, recursive = TRUE)
})

test_that("ParquetBackend$get_matrices_parallel() works correctly", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("Matrix")

  # Create test data
  connectomes <- create_test_spd(p = 4, n = 12)

  # Write to Parquet
  temp_dir <- tempfile()
  dir.create(temp_dir)
  write_connectomes_to_parquet(
    connectomes,
    output_dir = temp_dir,
    subject_ids = paste0("subj_", 1:12),
    overwrite = TRUE
  )

  # Create backend
  backend <- create_parquet_backend(temp_dir, cache_size = 5)

  # Load subset
  set_parallel_plan("multisession", workers = 2)
  subset <- backend$get_matrices_parallel(c(1, 3, 5, 7), progress = FALSE)

  # Verify
  expect_equal(length(subset), 4)
  expect_equal(as.matrix(subset[[1]]), as.matrix(connectomes[[1]]), tolerance = 1e-10, check.attributes = FALSE)
  expect_equal(as.matrix(subset[[2]]), as.matrix(connectomes[[3]]), tolerance = 1e-10, check.attributes = FALSE)

  # Cleanup
  reset_parallel_plan()
  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Test CSample$load_connectomes_batched()
# ==============================================================================

test_that("CSample$load_connectomes_batched() works with ParquetBackend", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 20)

  # Write to Parquet
  temp_dir <- tempfile()
  dir.create(temp_dir)
  write_connectomes_to_parquet(
    connectomes,
    output_dir = temp_dir,
    subject_ids = paste0("subj_", 1:20),
    overwrite = TRUE
  )

  # Create sample
  backend <- create_parquet_backend(temp_dir, cache_size = 5)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  # Load in batches
  set_parallel_plan("multisession", workers = 2)
  loaded <- sample$load_connectomes_batched(
    indices = 1:15,
    batch_size = 5,
    progress = FALSE
  )

  # Verify
  expect_equal(length(loaded), 15)
  for (i in 1:15) {
    expect_equal(
      as.matrix(loaded[[i]]),
      as.matrix(connectomes[[i]]),
      tolerance = 1e-10,
      check.attributes = FALSE
    )
  }

  # Cleanup
  reset_parallel_plan()
  unlink(temp_dir, recursive = TRUE)
})

test_that("CSample$load_connectomes_batched() validates indices", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 10)

  # Write to Parquet
  temp_dir <- tempfile()
  dir.create(temp_dir)
  write_connectomes_to_parquet(
    connectomes,
    output_dir = temp_dir,
    subject_ids = paste0("subj_", 1:10),
    overwrite = TRUE
  )

  # Create sample
  backend <- create_parquet_backend(temp_dir)
  sample <- CSample$new(backend = backend, metric_obj = airm)

  # Invalid indices
  expect_error(
    sample$load_connectomes_batched(indices = c(1, 15), batch_size = 5),
    "must be in range"
  )

  expect_error(
    sample$load_connectomes_batched(indices = c(0, 5), batch_size = 5),
    "must be in range"
  )

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Test auto-detection logic
# ==============================================================================

test_that("Auto-detection prevents parallelization for small datasets", {
  skip_if_not_installed("Matrix")

  # Create small dataset (n = 5, below default threshold of 10)
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 5)

  # Enable parallel plan
  set_parallel_plan("multisession", workers = 2)

  # Create sample and compute
  sample <- CSample$new(conns = connectomes, metric_obj = airm)

  # Should use sequential due to auto-detection
  # (We can't directly test this, but we can verify it doesn't error)
  expect_silent(sample$compute_tangents())
  expect_silent(sample$compute_vecs())

  # Reset
  reset_parallel_plan()
})

test_that("Auto-detection enables parallelization for large datasets", {
  skip_if_not_installed("Matrix")

  # Create large dataset (n = 15, above default threshold of 10)
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 15)

  # Enable parallel plan
  set_parallel_plan("multisession", workers = 2)
  expect_true(is_parallel_enabled())

  # Create sample and compute
  sample <- CSample$new(conns = connectomes, metric_obj = airm)

  # Should use parallel processing
  expect_silent(sample$compute_tangents())
  expect_silent(sample$compute_vecs())

  # Results should be correct
  expect_equal(nrow(sample$vector_images), 15)

  # Reset
  reset_parallel_plan()
})

# ==============================================================================
# Test progress parameter pass-through
# ==============================================================================

test_that("Progress parameter is accepted by all methods", {
  skip_if_not_installed("Matrix")

  # Create test data
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 12)

  # Create sample
  sample <- CSample$new(conns = connectomes, metric_obj = airm)

  # All methods should accept progress parameter without error
  expect_silent(sample$compute_tangents(progress = FALSE))
  expect_silent(sample$compute_vecs(progress = FALSE))
  expect_silent(sample$compute_conns(progress = FALSE))
  expect_silent(sample$compute_unvecs(progress = FALSE))
  # compute_fmean produces convergence messages (expected behavior), so just suppress them
  suppressMessages(sample$compute_fmean(progress = FALSE))
  expect_silent(sample$change_ref_pt(connectomes[[2]], progress = FALSE))
})

# ==============================================================================
# Test edge cases
# ==============================================================================

test_that("Parallel processing handles single matrix correctly", {
  skip_if_not_installed("Matrix")

  # Create single matrix
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 1)

  # Enable parallel
  set_parallel_plan("multisession", workers = 2)

  # Create sample (should handle n=1 gracefully)
  sample <- CSample$new(conns = connectomes, metric_obj = airm)

  # Should work without error (will use sequential due to auto-detection)
  expect_silent(sample$compute_tangents())
  expect_equal(length(sample$tangent_images), 1)

  # Reset
  reset_parallel_plan()
})

test_that("Parallel processing handles exact threshold correctly", {
  skip_if_not_installed("Matrix")

  # Create dataset exactly at threshold (n = 10)
  data(airm)
  connectomes <- create_test_spd(p = 4, n = 10)

  # Enable parallel
  set_parallel_plan("multisession", workers = 2)

  # Create sample
  sample <- CSample$new(conns = connectomes, metric_obj = airm)

  # Should enable parallelization (n >= threshold)
  expect_silent(sample$compute_tangents())
  expect_equal(length(sample$tangent_images), 10)

  # Reset
  reset_parallel_plan()
})

# ==============================================================================
# Test strategy fallback
# ==============================================================================

test_that("multicore strategy falls back to multisession on Windows", {
  skip_if(.Platform$OS.type != "windows")

  # Request multicore on Windows
  expect_warning(
    set_parallel_plan("multicore", workers = 2),
    "not available on Windows"
  )

  # Should still enable parallel processing (via multisession)
  expect_true(is_parallel_enabled())

  # Reset
  reset_parallel_plan()
})

# ==============================================================================
# Cleanup
# ==============================================================================

# Ensure parallel plan is reset after all tests
reset_parallel_plan()
