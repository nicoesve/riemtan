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