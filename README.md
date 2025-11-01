# riemtan: Riemannian Metrics for Symmetric Positive Definite Matrices

<!-- badges: start -->
<!-- [![R-CMD-check](https://github.com/yourusername/riemtan/workflows/R-CMD-check/badge.svg)](https://github.com/nicoesve/riemtan/actions) -->
<!-- badges: end -->

## Overview

`riemtan` implements various Riemannian metrics for symmetric positive definite (SPD) matrices. It provides tools for computing logarithmic and exponential maps, vectorization operations, and statistical analyses on the manifold of SPD matrices.

The package implements five different metrics:
- Affine Invariant Riemannian Metric (AIRM)
- Log-Euclidean
- Euclidean
- Log-Cholesky
- Bures-Wasserstein

## Installation

You can install the released version of riemtan with:

```r
# install.packages("devtools")
devtools::install_github("nicoesve/riemtan")
```

## Usage
Here's a basic example of computing the AIRM logarithm between two SPD matrices:

```r
library(riemtan)
library(Matrix)

# Create two SPD matrices
sigma <- Matrix(c(2.0, 0.5, 0.5, 3.0), nrow = 2) |>
    nearPD() |> _$mat |> pack()
lambda <- Matrix(c(1.5, 0.3, 0.3, 2.5), nrow = 2) |>
    nearPD() |> _$mat |> pack()

# Compute AIRM logarithm
result <- airm_log(sigma, lambda)
```

For more complex analyses, use the CSample class:

```r
# Create a sample of SPD matrices
sample <- CSample$new(
    conns = list_of_matrices,
    metric_obj = airm
)

# Compute Frechet mean
sample$compute_fmean()

# Get sample statistics
sample$variation
sample$sample_cov
```

## Features
* Implementations of five different Riemannian metrics for SPD matrices
* Logarithmic and exponential maps for each metric
* Vectorization and inverse vectorization operations
* Statistical operations including:
    * Frechet mean computation
    * Sample variation
    * Sample covariance

* R6 class system for handling collections of SPD matrices
* Efficient matrix operations using the Matrix package
* **NEW in v0.2.4**: Apache Arrow/Parquet backend for memory-efficient storage of large datasets
* **NEW in v0.2.5**: Futureverse/furrr parallel processing for cross-platform performance

## Parquet Support for Large Datasets

For large brain connectome datasets, riemtan now supports Apache Arrow Parquet format for efficient storage and lazy loading:

```r
library(riemtan)

# Write connectomes to Parquet format
write_connectomes_to_parquet(
  connectomes,
  output_dir = "my_data",
  subject_ids = paste0("subj_", 1:100)
)

# Create CSample with Parquet backend (lazy loading)
backend <- create_parquet_backend("my_data", cache_size = 10)
sample <- CSample$new(backend = backend, metric_obj = airm)

# All operations work as usual
sample$compute_fmean()
sample$compute_variation()
```

Benefits:
- Memory-efficient: Load matrices on-demand
- Fast access: Columnar storage optimized for numerical data
- Metadata support: Store subject IDs and provenance information
- Fully backwards compatible with list-based workflows

See the [Parquet vignette](https://nicoesve.github.io/riemtan/articles/using-parquet.html) for details.

## Parallel Processing with Futureverse

riemtan now supports cross-platform parallel processing via the futureverse framework, dramatically improving performance for large datasets:

```r
library(riemtan)

# Enable parallel processing (works on all platforms including Windows!)
set_parallel_plan("multisession", workers = 4)

# Create sample from large dataset
sample <- CSample$new(conns = large_connectome_list, metric_obj = airm)

# All operations automatically use parallel processing when beneficial
sample$compute_tangents(progress = TRUE)   # 3-8x faster
sample$compute_fmean(progress = TRUE)      # 2-5x faster
sample$compute_variation()

# Parallel batch loading from Parquet
backend <- create_parquet_backend("huge_dataset")
sample <- CSample$new(backend = backend, metric_obj = airm)
conns <- sample$load_connectomes_batched(batch_size = 100, progress = TRUE)

# Reset to sequential processing when done
reset_parallel_plan()
```

Benefits:
- **Cross-platform**: Works on Windows, Mac, and Linux
- **Automatic**: Intelligently uses parallel processing when beneficial
- **Flexible**: Multiple backends (multisession, multicore, cluster)
- **Progress bars**: Optional progress reporting via progressr
- **Seamless**: Existing code works without modification

Performance gains:
- **Parquet I/O**: 5-10x faster loading
- **Tangent computations**: 3-8x faster (scales with workers)
- **Frechet mean**: 2-5x faster for large samples (n > 100)

## Documentation
For more detailed information, check out:

* [Package website](https://nicoesve.github.io/riemtan/)
* [Function reference](https://nicoesve.github.io/riemtann/reference/)
* [Introduction to riemtan](https://nicoesve.github.io/riemtan/articles/riemtann.html)

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.

## License
This package is licensed under the MIT License.
