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