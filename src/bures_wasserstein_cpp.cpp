#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Forward declaration for similarity_transform from airm_cpp.cpp
arma::mat similarity_transform(const arma::mat& A, const arma::mat& X);

// Forward declaration for symmpart_fast from airm_cpp.cpp
arma::mat symmpart_fast(const arma::mat& A);

// Optimized Bures-Wasserstein logarithm computation
// [[Rcpp::export]]
arma::mat bures_wasserstein_log_cpp(const arma::mat& sigma, const arma::mat& lambda) {
  // Compute matrix square root of sigma
  arma::mat sigma_sqrt;
  try {
    sigma_sqrt = arma::sqrtmat_sympd(sigma);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of sigma");
  }
  
  // Compute inverse of sigma_sqrt
  arma::mat sigma_sqrt_inv;
  try {
    sigma_sqrt_inv = arma::inv_sympd(sigma_sqrt);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute inverse of sigma square root");
  }
  
  // Compute intermediate terms: sigma_sqrt * lambda * sigma_sqrt
  arma::mat intermediate = similarity_transform(sigma_sqrt, lambda);
  
  // Compute square root of intermediate
  arma::mat intermediate_sqrt;
  try {
    intermediate_sqrt = arma::sqrtmat_sympd(intermediate);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of intermediate matrix");
  }
  
  // Compute aux: sigma_sqrt_inv * intermediate_sqrt * sigma_sqrt_inv
  arma::mat aux = similarity_transform(sigma_sqrt_inv, intermediate_sqrt);
  
  // Compute the logarithm: aux - I
  int n = sigma.n_rows;
  arma::mat result = aux - arma::eye(n, n);
  
  // Ensure symmetry
  return symmpart_fast(result);
}

// Optimized Bures-Wasserstein exponential computation
// [[Rcpp::export]]
arma::mat bures_wasserstein_exp_cpp(const arma::mat& sigma, const arma::mat& v) {
  // Get dimension
  int n = sigma.n_rows;
  
  // Create identity matrix
  arma::mat id_mat = arma::eye(n, n);
  
  // Compute (I + v)
  arma::mat i_plus_v = id_mat + v;
  
  // Compute (I + v) * sigma * (I + v) using similarity_transform
  arma::mat result = similarity_transform(i_plus_v, sigma);
  
  // Ensure symmetry
  return symmpart_fast(result);
}