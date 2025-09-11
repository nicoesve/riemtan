#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;
using namespace Eigen;

// Forward declaration for similarity_transform from airm_cpp.cpp
Eigen::MatrixXd similarity_transform(const Eigen::MatrixXd& A, const Eigen::MatrixXd& X);

// Forward declaration for symmpart_fast from airm_cpp.cpp
Eigen::MatrixXd symmpart_fast(const Eigen::MatrixXd& A);

// Matrix square root for symmetric positive definite matrices
// (Local version to avoid dependency issues)
Eigen::MatrixXd sqrtm_sympd_bw(const Eigen::MatrixXd& X) {
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(X);
  if (es.info() != Eigen::Success) {
    Rcpp::stop("Eigendecomposition failed");
  }
  
  Eigen::VectorXd eigenvalues = es.eigenvalues();
  
  // Check if all eigenvalues are positive
  if ((eigenvalues.array() <= 0).any()) {
    Rcpp::stop("Matrix is not positive definite");
  }
  
  // Compute sqrt of eigenvalues
  Eigen::VectorXd sqrt_eigenvalues = eigenvalues.array().sqrt();
  
  // Reconstruct the matrix square root
  Eigen::MatrixXd result = es.eigenvectors() * sqrt_eigenvalues.asDiagonal() * es.eigenvectors().transpose();
  
  return result;
}

// Optimized Bures-Wasserstein logarithm computation
// [[Rcpp::export]]
Eigen::MatrixXd bures_wasserstein_log_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& lambda) {
  // Compute matrix square root of sigma
  Eigen::MatrixXd sigma_sqrt;
  try {
    sigma_sqrt = sqrtm_sympd_bw(sigma);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of sigma");
  }
  
  // Compute inverse of sigma_sqrt
  Eigen::MatrixXd sigma_sqrt_inv;
  try {
    // For SPD matrices, we can use LDLT decomposition for efficient inverse
    sigma_sqrt_inv = sigma_sqrt.ldlt().solve(Eigen::MatrixXd::Identity(sigma_sqrt.rows(), sigma_sqrt.cols()));
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute inverse of sigma square root");
  }
  
  // Compute intermediate terms: sigma_sqrt * lambda * sigma_sqrt
  Eigen::MatrixXd intermediate = similarity_transform(sigma_sqrt, lambda);
  
  // Compute square root of intermediate
  Eigen::MatrixXd intermediate_sqrt;
  try {
    intermediate_sqrt = sqrtm_sympd_bw(intermediate);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of intermediate matrix");
  }
  
  // Compute aux: sigma_sqrt_inv * intermediate_sqrt * sigma_sqrt_inv
  Eigen::MatrixXd aux = similarity_transform(sigma_sqrt_inv, intermediate_sqrt);
  
  // Compute the logarithm: aux - I
  int n = sigma.rows();
  Eigen::MatrixXd result = aux - Eigen::MatrixXd::Identity(n, n);
  
  // Ensure symmetry
  return symmpart_fast(result);
}

// Optimized Bures-Wasserstein exponential computation
// [[Rcpp::export]]
Eigen::MatrixXd bures_wasserstein_exp_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& v) {
  // Get dimension
  int n = sigma.rows();
  
  // Create identity matrix
  Eigen::MatrixXd id_mat = Eigen::MatrixXd::Identity(n, n);
  
  // Compute (I + v)
  Eigen::MatrixXd i_plus_v = id_mat + v;
  
  // Compute (I + v) * sigma * (I + v) using similarity_transform
  Eigen::MatrixXd result = similarity_transform(i_plus_v, sigma);
  
  // Ensure symmetry
  return symmpart_fast(result);
}