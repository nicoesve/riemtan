#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// Fast Log-Cholesky logarithmic map computation
// [[Rcpp::export]]
arma::mat log_cholesky_log_cpp(const arma::mat &sigma, const arma::mat &lambda)
{
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

// Helper function to compute half-underscore operation
arma::mat half_underscore_cpp(const arma::mat &x)
{
  arma::mat result = arma::trimatl(x, -1); // Lower triangular without diagonal
  result.diag() = x.diag() / 2.0;          // Half of diagonal elements
  return result;
}

// Fast Log-Cholesky exponential map computation
// [[Rcpp::export]]
arma::mat log_cholesky_exp_cpp(const arma::mat &sigma, const arma::mat &v)
{
  // Compute Cholesky decomposition - get lower triangular factor
  arma::mat l_ref = arma::chol(sigma, "lower");

  // Transform tangent vector to Cholesky space
  arma::mat l_inv = arma::inv(l_ref);
  arma::mat temp = l_inv * v * l_inv.t();
  arma::mat temp_under_half = half_underscore_cpp(temp);
  arma::mat x_l = l_ref * temp_under_half;
  x_l = arma::trimatl(x_l); // Keep only lower triangular part

  // Compute off-diagonal difference and diagonal terms
  arma::mat lower_sum = x_l + l_ref;
  arma::vec diag_ratio = x_l.diag() / l_ref.diag();
  arma::vec diag_terms = l_ref.diag() % arma::exp(diag_ratio);

  // Set diagonal terms
  lower_sum.diag() = diag_terms;

  // Return SPD matrix
  arma::mat result = lower_sum * lower_sum.t();

  return result;
}

// Fast SPD isometry to identity computation
// [[Rcpp::export]]
arma::mat spd_isometry_to_identity_cpp(const arma::mat &sigma, const arma::mat &v)
{
  // Compute Cholesky decomposition - get lower triangular factor
  arma::mat l_ref = arma::chol(sigma, "lower");

  // Compute lchol_inv: 1/diag(l_ref) - tril(l_ref, -1)
  arma::vec diag_inv = 1.0 / l_ref.diag();
  arma::mat lchol_inv = -arma::trimatl(l_ref, -1);
  lchol_inv.diag() = diag_inv;

  // Transform tangent vector to Cholesky space
  arma::mat l_inv = arma::inv(l_ref);
  arma::mat temp = l_inv * v * l_inv.t();
  arma::mat temp_under_half = half_underscore_cpp(temp);
  arma::mat x_l = l_ref * temp_under_half;

  // Create tangent version
  arma::mat tan_version = arma::trimatl(x_l, -1);
  tan_version.diag() = lchol_inv.diag() % x_l.diag();

  // Return symmetric part scaled by 2
  arma::mat result = 2.0 * tan_version;
  return 0.5 * (result + result.t());
}

// Fast SPD isometry from identity computation
// [[Rcpp::export]]
arma::mat spd_isometry_from_identity_cpp(const arma::mat &sigma, const arma::mat &v)
{
  // Get Cholesky decomposition - lower triangular factor
  arma::mat l_ref = arma::chol(sigma, "lower");

  // Apply half-underscore operation to v
  arma::mat x_l = half_underscore_cpp(v);

  // Create tangent version: lower triangular part without diagonal
  arma::mat tan_version = arma::trimatl(x_l, -1);

  // Set diagonal terms: diag(l_ref) * diag(x_l)
  tan_version.diag() = l_ref.diag() % x_l.diag();

  // Compute result: l_ref * tan_version' + tan_version * l_ref'
  arma::mat aux = l_ref * tan_version.t() + tan_version * l_ref.t();

  // Return symmetric part
  return 0.5 * (aux + aux.t());
}

// Fast vector scaling for log_cholesky_unvec
// [[Rcpp::export]]
arma::vec scale_vector_for_unvec_cpp(const arma::vec &w, int n)
{
  arma::vec w_scaled = w;

  // Scale diagonal elements: w_scaled[i * (i + 1) / 2] *= sqrt(2)
  for (int i = 1; i <= n; i++)
  {
    int idx = i * (i + 1) / 2 - 1; // Convert to 0-based indexing
    w_scaled(idx) *= std::sqrt(2.0);
  }

  // Scale entire vector by 1/sqrt(2)
  w_scaled /= std::sqrt(2.0);

  return w_scaled;
}