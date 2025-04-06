#' Compute the Log-Cholesky Logarithm
#'
#' This function computes the Riemannian logarithmic map using the Log-Cholesky metric for symmetric positive-definite matrices. The Log-Cholesky metric operates by transforming matrices via their Cholesky decomposition.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param lambda A symmetric positive-definite matrix of class `dppMatrix`, representing the target point.
#'
#' @return A symmetric matrix of class `dspMatrix`, representing the tangent space image of `lambda` at `sigma`.
#' @export
log_cholesky_log <- function(sigma, lambda) {
  validate_log_args(sigma, lambda)

  # Compute Cholesky decompositions - get lower triangular factors
  l_ref <- sigma |>
    chol() |>
    t()
  l_mfd <- lambda |>
    chol() |>
    t()

  # Compute off-diagonal difference and diagonal terms
  lower_diff <- l_mfd - l_ref
  diag_ratio <- diag(l_mfd) / diag(l_ref)
  diag_terms <- diag(l_ref) * log(diag_ratio)

  # Set diagonal terms
  diag(lower_diff) <- diag_terms

  # Project to SPD tangent space and return
  result <- l_ref %*% t(lower_diff) + lower_diff %*% t(l_ref)
  result |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Log-Cholesky Exponential
#'
#' This function computes the Riemannian exponential map using the Log-Cholesky metric for symmetric positive-definite matrices. The map operates by transforming the tangent vector via Cholesky decomposition of the reference point.
#'
#' @param sigma A symmetric positive-definite matrix of class `dppMatrix`, representing the reference point.
#' @param v A symmetric matrix of class `dspMatrix`, representing the tangent vector to be mapped.
#'
#' @return A symmetric positive-definite matrix of class `dppMatrix`, representing the point on the manifold.
#' @export
log_cholesky_exp <- function(sigma, v) {
  validate_exp_args(sigma, v)

  # Compute Cholesky decomposition - get lower triangular factor
  l_ref <- sigma |>
    chol() |>
    t()

  # Transform tangent vector to Cholesky space
  l_inv <- solve(l_ref)
  temp <- l_inv %*% v %*% t(l_inv)
  temp_under_half <- temp |> half_underscore()
  x_l <- l_ref %*% temp_under_half
  x_l <- Matrix::tril(x_l)

  # Compute off-diagonal difference and diagonal terms
  lower_sum <- (x_l + l_ref) |> as.matrix()
  diag_ratio <- diag(x_l |> as.matrix()) / diag(l_ref |> as.matrix())
  diag_terms <- diag(l_ref |> as.matrix()) * exp(diag_ratio |> as.matrix())

  # Set diagonal terms
  diag(lower_sum) <- diag_terms

  # Return SPD matrix
  aux <- (lower_sum %*% t(lower_sum))
  aux_2 <- aux |>
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

  l_ref <- sigma |>
    chol() |>
    t()
  lchol_inv <- l_ref |> (\(x) (1 / diag(x |> as.matrix())) - Matrix::tril(x, -1))()

  l_inv <- solve(l_ref)
  temp <- l_inv %*% v %*% t(l_inv)
  temp_under_half <- temp |> half_underscore()
  x_l <- l_ref %*% temp_under_half

  tan_version <- Matrix::tril(x_l, -1) |> as.matrix()
  diag(tan_version) <- diag(lchol_inv |> as.matrix()) * diag(x_l |> as.matrix())

  (2 * tan_version) |>
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

  # Get Cholesky decomposition
  l_ref <- sigma |>
    chol() |>
    t()

  x_l <- half_underscore(v)
  tan_version <- Matrix::tril(x_l, -1) |> as.matrix()
  diag(tan_version) <- diag(l_ref |> as.matrix()) * diag(x_l |> as.matrix())

  aux <- l_ref %*% t(tan_version) + tan_version %*% t(l_ref)
  aux |>
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

  # First undo vec_at_id like in airm_unvec
  w_scaled <- w
  for (i in 1:sigma@Dim[1]) {
    w_scaled[i * (i + 1) / 2] <- w_scaled[i * (i + 1) / 2] * sqrt(2)
  }
  w_scaled <- w_scaled / sqrt(2)

  # Create matrix and reverse isometry
  methods::new(
    "dspMatrix",
    x = w_scaled,
    Dim = as.integer(c(sigma@Dim[1], sigma@Dim[1]))
  ) |>
    spd_isometry_from_identity(sigma = sigma, v = _)
}
