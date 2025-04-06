#' Pre-configured Riemannian metrics for SPD matrices
#'
#' Ready-to-use metric objects for various Riemannian geometries on the manifold
#' of symmetric positive definite matrices.
#'
#' @format Objects of class \code{rmetric} containing four functions:
#' \describe{
#'   \item{log}{Computes the Riemannian logarithm}
#'   \item{exp}{Computes the Riemannian exponential}
#'   \item{vec}{Performs vectorization}
#'   \item{unvec}{Performs inverse vectorization}
#' }
#' @name metrics
#' @aliases airm log_euclidean euclidean log_cholesky bures_wasserstein
NULL

#' @rdname metrics
"airm"

#' @rdname metrics
"log_euclidean"

#' @rdname metrics
"euclidean"

#' @rdname metrics
"log_cholesky"

#' @rdname metrics
"bures_wasserstein"
