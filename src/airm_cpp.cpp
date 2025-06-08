#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Optimized vectorization at identity
// [[Rcpp::export]]
arma::vec vec_at_id_fast(const arma::mat& v) {
  int n = v.n_rows;
  arma::vec w(n * (n + 1) / 2);
  
  int idx = 0;
  for (int j = 0; j < n; j++) {
    for (int i = j; i < n; i++) {
      if (i == j) {
        w(idx) = v(i, j);
      } else {
        w(idx) = sqrt(2.0) * v(i, j);
      }
      idx++;
    }
  }
  
  return w;
}

// Fast matrix multiplication for similarity transforms
// [[Rcpp::export]]
arma::mat similarity_transform(const arma::mat& A, const arma::mat& X) {
  // Computes A * X * A
  return A * X * A;
}

// Fast symmetric part computation
// [[Rcpp::export]]
arma::mat symmpart_fast(const arma::mat& A) {
  return 0.5 * (A + A.t());
}

// Safe matrix logarithm with fallback methods
// [[Rcpp::export]]
arma::mat safe_logm_cpp(const arma::mat& X) {
  // First attempt: Use logmat_sympd for symmetric positive definite matrices
  try {
    arma::mat result = arma::logmat_sympd(X);
    return result;
  } catch (const std::exception& e) {
    // Second attempt: Use general logmat function
    try {
      arma::cx_mat complex_result = arma::logmat(X);
      
      // Check if the result has any non-zero imaginary parts
      if (arma::any(arma::vectorise(arma::imag(complex_result)) != 0)) {
        Rcpp::stop("Matrix logarithm resulted in complex values");
      }
      
      // Return only the real part if all imaginary parts are zero
      return arma::real(complex_result);
      
    } catch (const std::exception& e2) {
      // If both methods fail, throw an error
      Rcpp::stop("Matrix logarithm computation failed with both logmat_sympd and logmat methods");
    }
  }
}