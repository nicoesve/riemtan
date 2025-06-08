#' Compute the AIRM Logarithm
#'
#' This function computes the Riemannian logarithmic map for the Affine-Invariant Riemannian Metric (AIRM).
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param lambda A symmetric positive-definite matrix of class `dppMatrix`, representing the target point.
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
airm_log <- function(sigma, lambda) {
  validate_log_args(sigma, lambda)

  sigma_sqrt <- expm::sqrtm(sigma) |>
    Matrix::nearPD() |>
    _$mat

  sigma_sqrt_inv <- Matrix::solve(sigma_sqrt)

  lambda |>
    (\(x) sigma_sqrt_inv %*% x %*% sigma_sqrt_inv)() |>
    Matrix::symmpart() |>
    as.matrix() |>
    safe_logm() |>
    (\(x) sigma_sqrt %*% x %*% sigma_sqrt)() |>
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
airm_exp <- function(sigma, v) {
  validate_exp_args(sigma, v)

  sigma_sqrt <- expm::sqrtm(sigma) |>
    Matrix::nearPD() |>
    _$mat
  sigma_sqrt_inv <- Matrix::solve(sigma_sqrt)
  v |>
    (\(x) sigma_sqrt_inv %*% x %*% sigma_sqrt_inv)() |>
    Matrix::symmpart() |>
    as.matrix() |>
    expm::expm(method = "hybrid_Eigen_Ward") |>
    (\(x) sigma_sqrt %*% x %*% sigma_sqrt)() |>
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

  sigma_sqrt <- expm::sqrtm(sigma) |>
    Matrix::nearPD() |>
    _$mat
  sigma_sqrt_inv <- Matrix::solve(sigma_sqrt)
  v |>
    (\(x) sigma_sqrt_inv %*% x %*% sigma_sqrt_inv)() |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::symmpart() |>
    Matrix::pack() |>
    vec_at_id()
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

  sigma_sqrt <- expm::sqrtm(sigma) |>
    Matrix::nearPD() |>
    _$mat
  for (i in 1:sigma@Dim[1]) {
    w[i * (i + 1) / 2] <- w[i * (i + 1) / 2] * sqrt(2)
  }
  w <- w / sqrt(2)
  methods::new(
    "dspMatrix",
    x = w,
    Dim = as.integer(c(sigma@Dim[1], sigma@Dim[1]))
  ) |>
    (\(x) sigma_sqrt %*% x %*% sigma_sqrt)() |>
    Matrix::Matrix(sparse = FALSE, doDiag = FALSE) |>
    Matrix::symmpart() |>
    Matrix::pack()
}
