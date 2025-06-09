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

// Optimized AIRM logarithm computation
// [[Rcpp::export]]
arma::mat airm_log_cpp(const arma::mat& sigma, const arma::mat& lambda) {
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
  
  // Compute similarity transform: sigma_sqrt_inv * lambda * sigma_sqrt_inv
  arma::mat temp = similarity_transform(sigma_sqrt_inv, lambda);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Compute matrix logarithm
  arma::mat log_temp = safe_logm_cpp(temp);
  
  // Final similarity transform: sigma_sqrt * log_temp * sigma_sqrt
  arma::mat result = similarity_transform(sigma_sqrt, log_temp);
  
  return result;
}

// Optimized AIRM exponential computation
// [[Rcpp::export]]
arma::mat airm_exp_cpp(const arma::mat& sigma, const arma::mat& v) {
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
  
  // Compute similarity transform: sigma_sqrt_inv * v * sigma_sqrt_inv
  arma::mat temp = similarity_transform(sigma_sqrt_inv, v);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Compute matrix exponential
  arma::mat exp_temp = arma::expmat_sym(temp);
  
  // Final similarity transform: sigma_sqrt * exp_temp * sigma_sqrt
  arma::mat result = similarity_transform(sigma_sqrt, exp_temp);
  
  return result;
}

// Optimized AIRM vectorization computation
// [[Rcpp::export]]
arma::vec airm_vec_cpp(const arma::mat& sigma, const arma::mat& v) {
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
  
  // Compute similarity transform: sigma_sqrt_inv * v * sigma_sqrt_inv
  arma::mat temp = similarity_transform(sigma_sqrt_inv, v);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Use existing vec_at_id_fast function to vectorize
  return vec_at_id_fast(temp);
}

// Forward declaration for scale_vector_for_unvec_cpp
arma::vec scale_vector_for_unvec_cpp(const arma::vec& w, int n);

// Optimized AIRM unvec (inverse vectorization) computation
// [[Rcpp::export]]
arma::mat airm_unvec_cpp(const arma::mat& sigma, const arma::vec& w) {
  int n = sigma.n_rows;
  
  // Compute matrix square root of sigma
  arma::mat sigma_sqrt;
  try {
    sigma_sqrt = arma::sqrtmat_sympd(sigma);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of sigma");
  }
  
  // Scale the vector using the existing function
  arma::vec w_scaled = scale_vector_for_unvec_cpp(w, n);
  
  // Create symmetric matrix from scaled vector
  arma::mat temp(n, n, arma::fill::zeros);
  int idx = 0;
  for (int j = 0; j < n; j++) {
    for (int i = j; i < n; i++) {
      temp(i, j) = w_scaled(idx);
      if (i != j) {
        temp(j, i) = w_scaled(idx);
      }
      idx++;
    }
  }
  
  // Final similarity transform: sigma_sqrt * temp * sigma_sqrt
  arma::mat result = similarity_transform(sigma_sqrt, temp);
  
  // Ensure symmetry
  return symmpart_fast(result);
}