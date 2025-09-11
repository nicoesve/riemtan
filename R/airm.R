#' Compute the AIRM Logarithm
#'
#' This function computes the Riemannian logarithmic map for the Affine-Invariant Riemannian Metric (AIRM).
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param lambda A symmetric positive-definite matrix of class `dppMatrix`, representing the target point.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return A symmetric matrix of class `dspMatrix`, representing the tangent space image of `lambda` at `sigma`.
#' @examples
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   library(Matrix)
#'   sigma <- diag(2) |>
#'     Matrix::nearPD() |>
#'     _$mat |>
#'     Matrix::pack()
#'   lambda <- diag(c(2, 3)) |>
#'     Matrix::nearPD() |>
#'     _$mat |>
#'     Matrix::pack()
#'   airm_log(sigma, lambda)
#' }
#' @export
airm_log <- function(sigma, lambda, validate = FALSE) {
  if (validate) {
    validate_log_args(sigma, lambda)
  }

  # Convert to dense matrices for C++ computation
  sigma_mat <- as.matrix(sigma)
  lambda_mat <- as.matrix(lambda)

  # Use optimized C++ implementation
  result <- airm_log_cpp(sigma_mat, lambda_mat)

  # Convert back to Matrix format and make symmetric
  result |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the AIRM Exponential
#'
#' This function computes the Riemannian exponential map for the Affine-Invariant Riemannian Metric (AIRM).
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param v A tangent vector of class `dspMatrix`, to be mapped back to the manifold at `sigma`.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return A symmetric positive-definite matrix of class `dppMatrix`.
#' @examples
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   library(Matrix)
#'   sigma <- diag(2) |>
#'     Matrix::nearPD() |>
#'     _$mat |>
#'     Matrix::pack()
#'   v <- diag(c(1, 0.5)) |>
#'     Matrix::symmpart() |>
#'     Matrix::pack()
#'   airm_exp(sigma, v)
#' }
#' @export
airm_exp <- function(sigma, v, validate = FALSE) {
  if (validate) {
    validate_exp_args(sigma, v)
  }

  # Convert to dense matrices for C++ computation
  sigma_mat <- as.matrix(sigma)
  v_mat <- as.matrix(v)

  # Use optimized C++ implementation
  result <- airm_exp_cpp(sigma_mat, v_mat)

  # Convert back to Matrix format and ensure positive definiteness
  result |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::nearPD() |>
    _$mat |>
    Matrix::pack()
}

#' Vectorize at Identity Matrix
#'
#' Converts a symmetric matrix into a vector representation specific to operations at the identity matrix.
#'
#' @param v A symmetric matrix of class `dspMatrix`.
#'
#' @return A numeric vector, representing the vectorized tangent image.
#' @examples
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   library(Matrix)
#'   v <- diag(c(1, sqrt(2))) |>
#'     Matrix::symmpart() |>
#'     Matrix::pack()
#'   vec_at_id(v)
#' }
#' @export
vec_at_id <- function(v) {
  if (!inherits(v, "dspMatrix")) {
    stop("v should be an object of class dspMatrix")
  }

  # Use optimized C++ implementation
  v_mat <- as.matrix(v)
  as.numeric(vec_at_id_fast(v_mat))
}

#' Compute the AIRM Vectorization of Tangent Space
#'
#' Vectorizes a tangent matrix into a vector in Euclidean space using AIRM.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param v A symmetric matrix of class `dspMatrix`, representing a tangent vector.
#'
#' @return A numeric vector, representing the vectorized tangent image.
#' @examples
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   library(Matrix)
#'   sigma <- diag(2) |>
#'     Matrix::nearPD() |>
#'     _$mat |>
#'     Matrix::pack()
#'   v <- diag(c(1, 0.5)) |>
#'     Matrix::symmpart() |>
#'     Matrix::pack()
#'   airm_vec(sigma, v)
#' }
#' @export
airm_vec <- function(sigma, v) {
  validate_vec_args(sigma, v)

  # Convert to dense matrices for C++ computation
  sigma_mat <- as.matrix(sigma)
  v_mat <- as.matrix(v)

  # Use optimized C++ implementation
  result <- airm_vec_cpp(sigma_mat, v_mat)
  as.numeric(result)
}

#' Compute the Inverse Vectorization (AIRM)
#'
#' Converts a vector back into a tangent matrix relative to a reference point using AIRM.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param w A numeric vector, representing the vectorized tangent image.
#'
#' @return A symmetric matrix of class `dspMatrix`, representing the tangent vector.
#' @examples
#' if (requireNamespace("Matrix", quietly = TRUE)) {
#'   library(Matrix)
#'   sigma <- diag(2) |>
#'     Matrix::nearPD() |>
#'     _$mat |>
#'     Matrix::pack()
#'   w <- c(1, sqrt(2), 2)
#'   airm_unvec(sigma, w)
#' }
#' @export
airm_unvec <- function(sigma, w) {
  validate_unvec_args(sigma, w)

  # Convert to dense matrix for C++ computation
  sigma_mat <- as.matrix(sigma)
  
  # Use optimized C++ implementation
  result <- airm_unvec_cpp(sigma_mat, w)
  
  # Convert back to Matrix format
  result |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::symmpart() |>
    Matrix::pack()
}
