#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;
using namespace Eigen;

// Fast Log-Cholesky logarithmic map computation
// [[Rcpp::export]]
Eigen::MatrixXd log_cholesky_log_cpp(const Eigen::MatrixXd &sigma, const Eigen::MatrixXd &lambda)
{
  // Compute Cholesky decompositions - get lower triangular factors
  Eigen::LLT<Eigen::MatrixXd> llt_sigma(sigma);
  if (llt_sigma.info() != Eigen::Success) {
    Rcpp::stop("Cholesky decomposition of sigma failed");
  }
  Eigen::MatrixXd l_ref = llt_sigma.matrixL();
  
  Eigen::LLT<Eigen::MatrixXd> llt_lambda(lambda);
  if (llt_lambda.info() != Eigen::Success) {
    Rcpp::stop("Cholesky decomposition of lambda failed");
  }
  Eigen::MatrixXd l_mfd = llt_lambda.matrixL();

  // Compute off-diagonal difference
  Eigen::MatrixXd lower_diff = l_mfd - l_ref;

  // Compute diagonal terms
  Eigen::VectorXd diag_l_ref = l_ref.diagonal();
  Eigen::VectorXd diag_l_mfd = l_mfd.diagonal();
  Eigen::VectorXd diag_ratio = diag_l_mfd.array() / diag_l_ref.array();
  Eigen::VectorXd diag_terms = diag_l_ref.array() * diag_ratio.array().log();

  // Set diagonal terms
  lower_diff.diagonal() = diag_terms;

  // Project to SPD tangent space and return
  Eigen::MatrixXd result = l_ref * lower_diff.transpose() + lower_diff * l_ref.transpose();

  // Return symmetric part
  return 0.5 * (result + result.transpose());
}

// Helper function to compute half-underscore operation
Eigen::MatrixXd half_underscore_cpp(const Eigen::MatrixXd &x)
{
  Eigen::MatrixXd result = Eigen::MatrixXd::Zero(x.rows(), x.cols());
  result.triangularView<Eigen::StrictlyLower>() = x.triangularView<Eigen::StrictlyLower>(); // Lower triangular without diagonal
  result.diagonal() = x.diagonal() / 2.0;                                                      // Half of diagonal elements
  return result;
}

// Fast Log-Cholesky exponential map computation
// [[Rcpp::export]]
Eigen::MatrixXd log_cholesky_exp_cpp(const Eigen::MatrixXd &sigma, const Eigen::MatrixXd &v)
{
  // Compute Cholesky decomposition - get lower triangular factor
  Eigen::LLT<Eigen::MatrixXd> llt_sigma(sigma);
  if (llt_sigma.info() != Eigen::Success) {
    Rcpp::stop("Cholesky decomposition of sigma failed");
  }
  Eigen::MatrixXd l_ref = llt_sigma.matrixL();

  // Transform tangent vector to Cholesky space
  Eigen::MatrixXd l_inv = l_ref.inverse();
  Eigen::MatrixXd temp = l_inv * v * l_inv.transpose();
  Eigen::MatrixXd temp_under_half = half_underscore_cpp(temp);
  Eigen::MatrixXd x_l = l_ref * temp_under_half;
  // Keep only lower triangular part
  Eigen::MatrixXd x_l_lower = Eigen::MatrixXd::Zero(x_l.rows(), x_l.cols());
  x_l_lower.triangularView<Eigen::Lower>() = x_l.triangularView<Eigen::Lower>();
  x_l = x_l_lower;

  // Compute off-diagonal difference and diagonal terms
  Eigen::MatrixXd lower_sum = x_l + l_ref;
  Eigen::VectorXd diag_ratio = x_l.diagonal().array() / l_ref.diagonal().array();
  Eigen::VectorXd diag_terms = l_ref.diagonal().array() * diag_ratio.array().exp();

  // Set diagonal terms
  lower_sum.diagonal() = diag_terms;

  // Return SPD matrix
  Eigen::MatrixXd result = lower_sum * lower_sum.transpose();

  return result;
}

// Fast SPD isometry to identity computation
// [[Rcpp::export]]
Eigen::MatrixXd spd_isometry_to_identity_cpp(const Eigen::MatrixXd &sigma, const Eigen::MatrixXd &v)
{
  // Compute Cholesky decomposition - get lower triangular factor
  Eigen::LLT<Eigen::MatrixXd> llt_sigma(sigma);
  if (llt_sigma.info() != Eigen::Success) {
    Rcpp::stop("Cholesky decomposition of sigma failed");
  }
  Eigen::MatrixXd l_ref = llt_sigma.matrixL();

  // Compute lchol_inv: 1/diag(l_ref) - tril(l_ref, -1)
  Eigen::VectorXd diag_inv = 1.0 / l_ref.diagonal().array();
  Eigen::MatrixXd lchol_inv = Eigen::MatrixXd::Zero(l_ref.rows(), l_ref.cols());
  // Copy the strictly lower triangular part with negation
  for (int j = 0; j < l_ref.cols(); j++) {
    for (int i = j + 1; i < l_ref.rows(); i++) {
      lchol_inv(i, j) = -l_ref(i, j);
    }
  }
  lchol_inv.diagonal() = diag_inv;

  // Transform tangent vector to Cholesky space
  Eigen::MatrixXd l_inv = l_ref.inverse();
  Eigen::MatrixXd temp = l_inv * v * l_inv.transpose();
  Eigen::MatrixXd temp_under_half = half_underscore_cpp(temp);
  Eigen::MatrixXd x_l = l_ref * temp_under_half;

  // Create tangent version
  Eigen::MatrixXd tan_version = Eigen::MatrixXd::Zero(x_l.rows(), x_l.cols());
  tan_version.triangularView<Eigen::StrictlyLower>() = x_l.triangularView<Eigen::StrictlyLower>();
  tan_version.diagonal() = lchol_inv.diagonal().array() * x_l.diagonal().array();

  // Return symmetric part scaled by 2
  Eigen::MatrixXd result = 2.0 * tan_version;
  return 0.5 * (result + result.transpose());
}

// Fast SPD isometry from identity computation
// [[Rcpp::export]]
Eigen::MatrixXd spd_isometry_from_identity_cpp(const Eigen::MatrixXd &sigma, const Eigen::MatrixXd &v)
{
  // Get Cholesky decomposition - lower triangular factor
  Eigen::LLT<Eigen::MatrixXd> llt_sigma(sigma);
  if (llt_sigma.info() != Eigen::Success) {
    Rcpp::stop("Cholesky decomposition of sigma failed");
  }
  Eigen::MatrixXd l_ref = llt_sigma.matrixL();

  // Apply half-underscore operation to v
  Eigen::MatrixXd x_l = half_underscore_cpp(v);

  // Create tangent version: lower triangular part without diagonal
  Eigen::MatrixXd tan_version = Eigen::MatrixXd::Zero(x_l.rows(), x_l.cols());
  tan_version.triangularView<Eigen::StrictlyLower>() = x_l.triangularView<Eigen::StrictlyLower>();

  // Set diagonal terms: diag(l_ref) * diag(x_l)
  tan_version.diagonal() = l_ref.diagonal().array() * x_l.diagonal().array();

  // Compute result: l_ref * tan_version' + tan_version * l_ref'
  Eigen::MatrixXd aux = l_ref * tan_version.transpose() + tan_version * l_ref.transpose();

  // Return symmetric part
  return 0.5 * (aux + aux.transpose());
}

// Fast vector scaling for log_cholesky_unvec
// [[Rcpp::export]]
Eigen::VectorXd scale_vector_for_unvec_cpp(const Eigen::VectorXd &w, int n)
{
  Eigen::VectorXd w_scaled = w;

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