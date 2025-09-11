#' Compute the Log-Cholesky Logarithm
#'
#' This function computes the Riemannian logarithmic map using the Log-Cholesky metric for symmetric positive-definite matrices. The Log-Cholesky metric operates by transforming matrices via their Cholesky decomposition.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param lambda A symmetric positive-definite matrix of class `dppMatrix`, representing the target point.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return A symmetric matrix of class `dspMatrix`, representing the tangent space image of `lambda` at `sigma`.
#' @export
log_cholesky_log <- function(sigma, lambda, validate = FALSE) {
  if (validate) {
    validate_log_args(sigma, lambda)
  }

  # Convert to dense matrices for C++ computation
  sigma_dense <- as.matrix(sigma)
  lambda_dense <- as.matrix(lambda)

  # Call C++ implementation
  result <- log_cholesky_log_cpp(sigma_dense, lambda_dense)

  # Convert result to R matrix and then to packed symmetric matrix format
  result |>
    as.matrix() |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Log-Cholesky Exponential
#'
#' This function computes the Riemannian exponential map using the Log-Cholesky metric for symmetric positive-definite matrices. The map operates by transforming the tangent vector via Cholesky decomposition of the reference point.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param v A symmetric matrix of class `dspMatrix`, representing the tangent vector to be mapped.
#' @param validate A logical value indicating whether to validate input arguments. Default is FALSE.
#'
#' @return A symmetric positive-definite matrix of class `dppMatrix`, representing the point on the manifold.
#' @export
log_cholesky_exp <- function(sigma, v, validate = FALSE) {
  if (validate) {
    validate_exp_args(sigma, v)
  }

  # Convert to dense matrices for C++ computation
  sigma_dense <- as.matrix(sigma)
  v_dense <- as.matrix(v)

  # Call C++ implementation
  result <- log_cholesky_exp_cpp(sigma_dense, v_dense)

  # Convert result to R matrix and ensure positive definiteness
  aux_2 <- result |>
    as.matrix() |>
    Matrix::symmpart() |>
    Matrix::nearPD() |>
    _$mat
  aux_2 |> Matrix::pack()
}

#' Isometry from tangent space at P to tangent space at identity
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param v A symmetric matrix of class `dspMatrix`
#' @return A symmetric matrix of class `dspMatrix`
#' @export
spd_isometry_to_identity <- function(sigma, v) {
  validate_vec_args(sigma, v)

  # Convert to dense matrices for C++ computation
  sigma_dense <- as.matrix(sigma)
  v_dense <- as.matrix(v)

  # Call C++ implementation
  result <- spd_isometry_to_identity_cpp(sigma_dense, v_dense)

  # Convert result to R matrix and pack
  result |>
    as.matrix() |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Log-Cholesky Vectorization
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param v A symmetric matrix of class `dspMatrix`
#' @return A numeric vector representing the vectorized tangent image
#' @export
log_cholesky_vec <- function(sigma, v) {
  validate_vec_args(sigma, v)

  # Apply isometry then vectorize at identity
  v |>
    spd_isometry_to_identity(sigma = sigma, v = _) |>
    vec_at_id()
}

#' Reverse isometry from tangent space at identity to tangent space at P
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param v A symmetric matrix of class `dspMatrix`
#' @return A symmetric matrix of class `dspMatrix`
#' @export
spd_isometry_from_identity <- function(sigma, v) {
  validate_vec_args(sigma, v)

  # Convert to dense matrices for C++ computation
  sigma_dense <- as.matrix(sigma)
  v_dense <- as.matrix(v)

  # Call C++ implementation
  result <- spd_isometry_from_identity_cpp(sigma_dense, v_dense)

  # Convert result to R matrix and pack
  result |>
    as.matrix() |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Log-Cholesky Inverse Vectorization
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`
#' @param w A numeric vector representing the vectorized tangent image
#' @return A symmetric matrix of class `dspMatrix`
#' @export
log_cholesky_unvec <- function(sigma, w) {
  validate_unvec_args(sigma, w)

  # First undo vec_at_id like in airm_unvec using C++ implementation
  w_scaled <- scale_vector_for_unvec_cpp(w, sigma@Dim[1]) |> as.vector()

  # Create matrix and reverse isometry
  methods::new(
    "dspMatrix",
    x = w_scaled,
    Dim = as.integer(c(sigma@Dim[1], sigma@Dim[1]))
  ) |>
    spd_isometry_from_identity(sigma = sigma, v = _)
}
