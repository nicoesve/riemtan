#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;
using namespace Eigen;

// Optimized vectorization at identity
// [[Rcpp::export]]
Eigen::VectorXd vec_at_id_fast(const Eigen::MatrixXd& v) {
  int n = v.rows();
  Eigen::VectorXd w(n * (n + 1) / 2);
  
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
Eigen::MatrixXd similarity_transform(const Eigen::MatrixXd& A, const Eigen::MatrixXd& X) {
  // Computes A * X * A
  return A * X * A;
}

// Fast symmetric part computation
// [[Rcpp::export]]
Eigen::MatrixXd symmpart_fast(const Eigen::MatrixXd& A) {
  return 0.5 * (A + A.transpose());
}

// Safe matrix logarithm with fallback methods
// [[Rcpp::export]]
Eigen::MatrixXd safe_logm_cpp(const Eigen::MatrixXd& X) {
  // For SPD matrices, we can use eigendecomposition
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(X);
  if (es.info() != Eigen::Success) {
    Rcpp::stop("Eigendecomposition failed");
  }
  
  Eigen::VectorXd eigenvalues = es.eigenvalues();
  
  // Check if all eigenvalues are positive
  if ((eigenvalues.array() <= 0).any()) {
    Rcpp::stop("Matrix is not positive definite");
  }
  
  // Compute log of eigenvalues
  Eigen::VectorXd log_eigenvalues = eigenvalues.array().log();
  
  // Reconstruct the matrix logarithm
  Eigen::MatrixXd result = es.eigenvectors() * log_eigenvalues.asDiagonal() * es.eigenvectors().transpose();
  
  return result;
}

// Matrix square root for symmetric positive definite matrices
Eigen::MatrixXd sqrtm_sympd(const Eigen::MatrixXd& X) {
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

// Matrix exponential for symmetric matrices
Eigen::MatrixXd expm_sym(const Eigen::MatrixXd& X) {
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(X);
  if (es.info() != Eigen::Success) {
    Rcpp::stop("Eigendecomposition failed");
  }
  
  Eigen::VectorXd eigenvalues = es.eigenvalues();
  
  // Compute exp of eigenvalues
  Eigen::VectorXd exp_eigenvalues = eigenvalues.array().exp();
  
  // Reconstruct the matrix exponential
  Eigen::MatrixXd result = es.eigenvectors() * exp_eigenvalues.asDiagonal() * es.eigenvectors().transpose();
  
  return result;
}

// Optimized AIRM logarithm computation
// [[Rcpp::export]]
Eigen::MatrixXd airm_log_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& lambda) {
  // Compute matrix square root of sigma
  Eigen::MatrixXd sigma_sqrt;
  try {
    sigma_sqrt = sqrtm_sympd(sigma);
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
  
  // Compute similarity transform: sigma_sqrt_inv * lambda * sigma_sqrt_inv
  Eigen::MatrixXd temp = similarity_transform(sigma_sqrt_inv, lambda);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Compute matrix logarithm
  Eigen::MatrixXd log_temp = safe_logm_cpp(temp);
  
  // Final similarity transform: sigma_sqrt * log_temp * sigma_sqrt
  Eigen::MatrixXd result = similarity_transform(sigma_sqrt, log_temp);
  
  return result;
}

// Optimized AIRM exponential computation
// [[Rcpp::export]]
Eigen::MatrixXd airm_exp_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& v) {
  // Compute matrix square root of sigma
  Eigen::MatrixXd sigma_sqrt;
  try {
    sigma_sqrt = sqrtm_sympd(sigma);
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
  
  // Compute similarity transform: sigma_sqrt_inv * v * sigma_sqrt_inv
  Eigen::MatrixXd temp = similarity_transform(sigma_sqrt_inv, v);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Compute matrix exponential
  Eigen::MatrixXd exp_temp = expm_sym(temp);
  
  // Final similarity transform: sigma_sqrt * exp_temp * sigma_sqrt
  Eigen::MatrixXd result = similarity_transform(sigma_sqrt, exp_temp);
  
  return result;
}

// Optimized AIRM vectorization computation
// [[Rcpp::export]]
Eigen::VectorXd airm_vec_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& v) {
  // Compute matrix square root of sigma
  Eigen::MatrixXd sigma_sqrt;
  try {
    sigma_sqrt = sqrtm_sympd(sigma);
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
  
  // Compute similarity transform: sigma_sqrt_inv * v * sigma_sqrt_inv
  Eigen::MatrixXd temp = similarity_transform(sigma_sqrt_inv, v);
  
  // Ensure symmetry
  temp = symmpart_fast(temp);
  
  // Use existing vec_at_id_fast function to vectorize
  return vec_at_id_fast(temp);
}

// Forward declaration for scale_vector_for_unvec_cpp
Eigen::VectorXd scale_vector_for_unvec_cpp(const Eigen::VectorXd& w, int n);

// Optimized AIRM unvec (inverse vectorization) computation
// [[Rcpp::export]]
Eigen::MatrixXd airm_unvec_cpp(const Eigen::MatrixXd& sigma, const Eigen::VectorXd& w) {
  int n = sigma.rows();
  
  // Compute matrix square root of sigma
  Eigen::MatrixXd sigma_sqrt;
  try {
    sigma_sqrt = sqrtm_sympd(sigma);
  } catch (const std::exception& e) {
    Rcpp::stop("Failed to compute matrix square root of sigma");
  }
  
  // Scale the vector using the existing function
  Eigen::VectorXd w_scaled = scale_vector_for_unvec_cpp(w, n);
  
  // Create symmetric matrix from scaled vector
  Eigen::MatrixXd temp = Eigen::MatrixXd::Zero(n, n);
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
  Eigen::MatrixXd result = similarity_transform(sigma_sqrt, temp);
  
  // Ensure symmetry
  return symmpart_fast(result);
}