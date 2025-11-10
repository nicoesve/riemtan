# riemtan 0.2.5

## New Features

### Futureverse/furrr Parallel Processing

* Added comprehensive parallel processing support via the futureverse framework (future + furrr packages)
* **Cross-platform parallelization**: Works on all platforms including Windows (replaces platform-specific `parallel::mclapply`)
* **Intelligent auto-detection**: Automatically uses parallel processing when beneficial based on data size
* **Flexible configuration**: Users can control parallelization via `set_parallel_plan()`
* **Progress reporting**: Optional progress bars via progressr package integration

#### Parallel Processing Infrastructure

* New module `R/parallel_config.R`:
  - `set_parallel_plan()`: Configure parallel strategy (sequential, multisession, multicore, cluster)
  - `is_parallel_enabled()`: Check parallel status
  - `should_parallelize()`: Auto-detect when to parallelize based on threshold
  - `get_n_workers()`: Query number of parallel workers
  - `reset_parallel_plan()`: Reset to sequential mode

* New module `R/progress_utils.R`:
  - `with_progress()`: Execute expressions with progress reporting
  - `create_progressor()`: Create progress reporters for loops
  - `configure_progress()`: Configure progress handlers
  - `is_progress_available()`: Check progressr availability

#### Enhanced Components with Parallel Support

* **TangentImageHandler** (`R/tangent_handler.R`):
  - `compute_tangents()`: Parallel tangent space projections
  - `compute_vecs()`: Parallel vectorization
  - `compute_conns()`: Parallel exponential maps
  - `set_reference_point()`: Parallel tangent relocation
  - All methods support optional `progress` parameter

* **ParquetBackend** (`R/parquet_backend.R`):
  - `get_all_matrices()`: Parallel matrix loading from Parquet files
  - `get_matrices_parallel()`: New method for batch parallel loading
  - Automatic parallelization for I/O-bound operations

* **Core Algorithms** (`R/other_utils.R`):
  - `relocate()`: Now uses `furrr::future_map()` instead of `parallel::mclapply` (cross-platform!)
  - `compute_frechet_mean()`: Parallel processing support with progress parameter
  - Both functions support `progress` parameter

* **CSample Class** (`R/sample.R`):
  - `compute_tangents()`: Pass-through progress support
  - `compute_vecs()`: Pass-through progress support
  - `compute_unvecs()`: Parallel unvectorizati with progress
  - `compute_conns()`: Pass-through progress support
  - `compute_fmean()`: Pass-through progress support
  - `change_ref_pt()`: Pass-through progress support
  - `load_connectomes_batched()`: **NEW** - Batch loading with memory management for large Parquet datasets

## Performance Improvements

* **5-10x speedup** for loading large Parquet datasets (parallel I/O)
* **3-8x speedup** for tangent computations on large samples (scales with workers)
* **2-5x speedup** for Frechet mean computation (n > 100)
* Reduced overhead for small datasets via intelligent threshold-based auto-detection

## Breaking Changes

* `relocate()` function signature changed: added optional `progress` parameter (default: FALSE)
  - Existing code continues to work without modification
* `compute_frechet_mean()` function signature changed: added optional `progress` parameter
  - Existing code continues to work without modification

## Dependencies

* Added `future` to Imports (cross-platform parallel backend)
* Added `furrr` to Imports (future-based parallel map functions)
* Added `progressr` to Suggests (optional progress reporting)

## Documentation

* Updated vignettes with parallel processing examples
* New performance benchmarking guidance
* Comprehensive roxygen2 documentation for all new functions

## Internal Changes

* Removed dependency on `parallel::mclapply` (platform-specific, Windows incompatible)
* All parallel operations now use futureverse framework for consistency
* Progress reporting infrastructure integrated throughout codebase

---

# riemtan 0.2.4

## New Features

### Apache Arrow/Parquet Support

* Added support for Apache Arrow Parquet format for efficient storage and lazy loading of large connectome datasets
* Introduced backend abstraction layer (`DataBackend`, `ListBackend`, `ParquetBackend`) to decouple storage from business logic
* `ListBackend`: Wraps existing list-based storage for backwards compatibility
* `ParquetBackend`: Lazy-loads matrices from Parquet files with LRU caching (default cache size: 10 matrices)
* New I/O functions:
  - `write_connectomes_to_parquet()`: Export matrices to Parquet format with metadata
  - `validate_parquet_directory()`: Validate Parquet directory structure
  - `create_parquet_backend()`: Convenience function to create ParquetBackend
* `CSample` now accepts `backend` parameter for flexible storage options
* Full backwards compatibility: existing code using lists continues to work unchanged
* Metadata support: Store subject IDs, data provenance, and custom metadata with Parquet datasets
* New vignette: "Using Parquet Storage for Large Datasets"

### Validation Functions

* Added `validate_backend()`: Validates backend objects
* Added `validate_parquet_dir()`: Validates Parquet directory structure

## Internal Changes

* Modified `CSample` to use backend abstraction internally
* Updated `CSample$connectomes` active binding to support lazy loading
* `CSuperSample` works transparently with all backend types

## Dependencies

* Added `arrow` to Imports for Parquet support

## Documentation

* Updated package documentation with roxygen2
* Added comprehensive test suite for Parquet backend (50+ tests)
* New vignette explaining Parquet backend usage

# riemtan 0.2.0

## New Features
- Added `super_sample.R`, introducing the `CSuperSample` class for handling and analyzing collections of `CSample` objects.
- Added comprehensive tests for `CSuperSample` in `test-csupersample.R`.

## Dependency Changes
- Moved the `Matrix` package from `Depends` to `Imports` in the DESCRIPTION file.

# riemtan 0.1.0

## New Features
- Implements various Riemannian metrics for symmetric positive definite matrices, including:
  - Affine Invariant Riemannian Metric (AIRM)
  - Log-Euclidean
  - Euclidean
  - Log-Cholesky
  - Bures-Wasserstein metrics
- Provides functions for:
  - Computing logarithmic and exponential maps
  - Vectorization

## Documentation
- Added comprehensive documentation for all functions using `roxygen2`.
- Included vignettes demonstrating the usage of the package.

## Dependencies
- Depends on R (>= 4.3.0)
- Imports the following packages:
  - Matrix
  - methods
  - expm
  - R6
  - purrr
  - MASS
  - furrr
- Suggests the following packages for testing and documentation:
  - testthat (>= 3.0.0)
  - knitr
  - rmarkdown

## Miscellaneous
- Added a `LICENSE` file with the MIT license.
- Maintainer: Nicolas Escobar <nescoba@iu.edu>