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

## Documentation
For more detailed information, check out:

* [Package website](https://nicoesve.github.io/riemtan/)
* [Function reference](https://nicoesve.github.io/riemtann/reference/)
* [Introduction to riemtan](https://nicoesve.github.io/riemtan/articles/riemtann.html)

## Contributing
Contributions are welcome! Please feel free to submit a Pull Request.

## License
This package is licensed under the MIT License.
