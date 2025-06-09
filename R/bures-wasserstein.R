#' Compute the Bures-Wasserstein Exponential
#'
#' This function computes the Riemannian exponential map using the Bures-Wasserstein metric for symmetric positive-definite matrices. The map operates by solving a Lyapunov equation and then constructing the exponential.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param v A symmetric matrix of class `dspMatrix`, representing the tangent vector to be mapped.
#' @return A symmetric positive-definite matrix of class `dppMatrix`, representing the point on the manifold.
#' @export
bures_wasserstein_exp <- function(sigma, v) {
  validate_exp_args(sigma, v)

  # Convert to standard matrix for C++ computation
  sigma_mat <- as.matrix(sigma)
  v_mat <- as.matrix(v)

  # Use optimized C++ implementation
  result <- bures_wasserstein_exp_cpp(sigma_mat, v_mat)

  # Convert back to Matrix package format and ensure positive definiteness
  result |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::nearPD() |>
    _$mat |>
    Matrix::pack()
}

#' Compute the Bures-Wasserstein Logarithm
#'
#' This function computes the Riemannian logarithmic map using the Bures-Wasserstein metric for symmetric positive-definite matrices.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param lambda A symmetric positive-definite matrix of class `dppMatrix`, representing the target point.
#' @return A symmetric matrix of class `dspMatrix`, representing the tangent space image of `lambda` at `sigma`.
#' @export
bures_wasserstein_log <- function(sigma, lambda) {
  validate_log_args(sigma, lambda)

  # Convert to standard matrix for C++ computation
  sigma_mat <- as.matrix(sigma)
  lambda_mat <- as.matrix(lambda)

  # Use optimized C++ implementation
  result <- bures_wasserstein_log_cpp(sigma_mat, lambda_mat)

  # Convert back to Matrix package format
  result |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::pack()
}

#' Compute the Bures-Wasserstein Vectorization
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param v A symmetric matrix of class `dspMatrix`
#' @return A numeric vector representing the vectorized tangent image
#' @export
bures_wasserstein_vec <- function(sigma, v) {
  # For now, use same vectorization as AIRM
  airm_vec(sigma, v)
}

#' Compute the Bures-Wasserstein Inverse Vectorization
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param w A numeric vector representing the vectorized tangent image
#' @return A symmetric matrix of class `dspMatrix`
#' @export
bures_wasserstein_unvec <- function(sigma, w) {
  # For now, use same inverse vectorization as AIRM
  airm_unvec(sigma, w)
}
