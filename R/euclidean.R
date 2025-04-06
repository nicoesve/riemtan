#' Compute the Euclidean Logarithm
#'
#' @param sigma A reference point.
#' @param lambda A point on the manifold.
#'
#' @return The tangent space image of `lambda` at `sigma`.
#' @export
euclidean_log <- function(sigma, lambda) {
  validate_log_args(sigma, lambda)
  (lambda - sigma) |>
    Matrix::symmpart() |>
    Matrix::pack()
}

#' Compute the Euclidean Exponential
#'
#' @param sigma A reference point.
#' @param v A tangent vector to be mapped back to the manifold at `sigma`.
#'
#' @return The point on the manifold corresponding to the tangent vector at `sigma`.
#' @export
euclidean_exp <- function(sigma, v) {
  validate_exp_args(sigma, v)
  tryCatch(
    {
      chol(sigma + v)
      (sigma + v) |>
        Matrix::nearPD() |>
        _$mat |>
        Matrix::pack()
    },
    error = function(e) {
      stop("Exponential map is not defined for those arguments")
    }
  )
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
euclidean_vec <- function(sigma, v) {
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
euclidean_unvec <- function(sigma, w) {
  airm_unvec(sigma |> id_matr(), w)
}
