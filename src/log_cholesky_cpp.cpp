#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Fast Log-Cholesky logarithmic map computation
// [[Rcpp::export]]
arma::mat log_cholesky_log_cpp(const arma::mat& sigma, const arma::mat& lambda) {
  // Compute Cholesky decompositions - get lower triangular factors
  arma::mat l_ref = arma::chol(sigma, "lower");
  arma::mat l_mfd = arma::chol(lambda, "lower");
  
  // Compute off-diagonal difference
  arma::mat lower_diff = l_mfd - l_ref;
  
  // Compute diagonal terms
  arma::vec diag_l_ref = l_ref.diag();
  arma::vec diag_l_mfd = l_mfd.diag();
  arma::vec diag_ratio = diag_l_mfd / diag_l_ref;
  arma::vec diag_terms = diag_l_ref % arma::log(diag_ratio);
  
  // Set diagonal terms
  lower_diff.diag() = diag_terms;
  
  // Project to SPD tangent space and return
  arma::mat result = l_ref * lower_diff.t() + lower_diff * l_ref.t();
  
  // Return symmetric part
  return 0.5 * (result + result.t());
}