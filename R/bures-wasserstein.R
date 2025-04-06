#' Solve the Lyapunov Equation
#'
#' Solves the Lyapunov equation L P + P L = V for L.
#'
#' @param p A symmetric positive-definite matrix of class `dppMatrix`.
#' @param v A symmetric matrix of class `dspMatrix`.
#'
#' @return A symmetric matrix of class `dspMatrix`, representing the solution L.
#' @keywords internal
solve_lyapunov <- function(p, v) {
    if (!inherits(p, "dppMatrix")) {
        stop("p must be of class dppMatrix")
    }
    if (!inherits(v, "dspMatrix")) {
        stop("v must be of class dspMatrix")
    }

    n <- p@Dim[1]
    # Convert to standard matrices for kronecker operation
    p_dense <- as.matrix(p)
    v_dense <- as.matrix(v)

    # Construct system matrix and right-hand side
    a <- kronecker(diag(n), p_dense) + kronecker(p_dense, diag(n))
    b <- as.vector(v_dense)

    # Solve system and reshape solution
    l_vec <- solve(a, b)
    l_mat <- matrix(l_vec, nrow = n, ncol = n)

    # Convert back to Matrix package format and ensure symmetry
    l_mat |>
        Matrix::Matrix(sparse = FALSE) |>
        Matrix::symmpart() |>
        Matrix::pack()
}

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

    # Solve Lyapunov equation
    # l <- solve_lyapunov(sigma, v)

    # Compute exponential map
    n <- sigma@Dim[1]
    id_mat <- Matrix::Diagonal(n)

    ((id_mat + v) %*% sigma %*% (id_mat + v)) |>
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

    # # Compute square roots and their inverse
    sigma_sqrt <- expm::sqrtm(sigma) |>
        Matrix::nearPD() |>
        _$mat
    sigma_sqrt_inv <- solve(sigma_sqrt)

    # # Compute intermediate terms
    intermediate <- sigma_sqrt %*% lambda %*% sigma_sqrt
    intermediate_sqrt <- expm::sqrtm(intermediate)
    aux <- sigma_sqrt_inv %*% intermediate_sqrt %*% sigma_sqrt_inv

    # Compute the logarithm
    (aux - diag(nrow(sigma))) |>
        Matrix::symmpart() |>
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
