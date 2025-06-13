#' Compute the Log-Euclidean Logarithm
#'
#' @param sigma A reference point.
#' @param lambda A point on the manifold.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return The tangent space image of `lambda` at `sigma`.
#' @export
log_euclidean_log <- function(sigma, lambda, validate = FALSE) {
  if (validate) {
    validate_log_args(sigma, lambda)
  }

  # Convert to dense matrices for C++ computation
  sigma_mat <- as.matrix(sigma)
  lambda_mat <- as.matrix(lambda)
  
  # Call C++ implementation
  result <- log_euclidean_log_cpp(sigma_mat, lambda_mat)
  
  # Convert back to symmetric packed format
  result |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Log-Euclidean Exponential
#'
#' This function computes the Euclidean exponential map.
#'
#' @param ref_pt A reference point.
#' @param v A tangent vector to be mapped back to the manifold at `ref_pt`.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return The point on the manifold corresponding to the tangent vector at `ref_pt`.
#' @export
log_euclidean_exp <- function(ref_pt, v, validate = FALSE) {
  if (validate) {
    validate_exp_args(ref_pt, v)
  }

  # Convert to dense matrices for C++ computation
  ref_pt_mat <- as.matrix(ref_pt)
  v_mat <- as.matrix(v)
  
  # Call C++ implementation
  result <- log_euclidean_exp_cpp(ref_pt_mat, v_mat)
  
  # Ensure positive definiteness and convert to packed format
  result |>
    Matrix::nearPD() |>
    _$mat |>
    Matrix::pack()
}

#' Vectorize at Identity Matrix (Euclidean)
#'
#' Converts a symmetric matrix into a vector representation.
#'
#' @param sigma A symmetric matrix.
#' @param v A vector.
#'
#' @return A numeric vector, representing the vectorized tangent image.
#' @export
log_euclidean_vec <- function(sigma, v) {
  airm_vec(sigma |> id_matr(), v)
}

#' Compute the Inverse Vectorization (Euclidean)
#'
#' Converts a vector back into a tangent matrix relative to a reference point using Euclidean metric.
#'
#' @param sigma A symmetric matrix.
#' @param w A numeric vector, representing the vectorized tangent image.
#'
#' @return A symmetric matrix, representing the tangent vector.
#' @export
log_euclidean_unvec <- function(sigma, w) {
  airm_unvec(sigma |> id_matr(), w)
}
