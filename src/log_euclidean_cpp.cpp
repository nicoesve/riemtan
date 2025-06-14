#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

// Forward declarations
Eigen::MatrixXd safe_logm_cpp(const Eigen::MatrixXd& X);
Eigen::MatrixXd dexp_cpp(const Eigen::MatrixXd& a, const Eigen::MatrixXd& x, int num_points = 100);
Eigen::MatrixXd dlog_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& h, int num_points = 100);

// [[Rcpp::export]]
Eigen::MatrixXd log_euclidean_log_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& lambda) {
    // Compute matrix logarithms
    Eigen::MatrixXd aux_matr_1 = safe_logm_cpp(sigma);
    Eigen::MatrixXd aux_matr_2 = safe_logm_cpp(lambda);
    
    // Compute difference
    Eigen::MatrixXd aux_matr_3 = aux_matr_2 - aux_matr_1;
    
    // Make symmetric
    aux_matr_1 = 0.5 * (aux_matr_1 + aux_matr_1.transpose());
    aux_matr_3 = 0.5 * (aux_matr_3 + aux_matr_3.transpose());
    
    // Call dexp
    return dexp_cpp(aux_matr_1, aux_matr_3);
}

// [[Rcpp::export]]
Eigen::MatrixXd log_euclidean_exp_cpp(const Eigen::MatrixXd& ref_pt, const Eigen::MatrixXd& v) {
    // Compute matrix logarithm of ref_pt using safe_logm_cpp
    Eigen::MatrixXd aux_matr_1 = safe_logm_cpp(ref_pt);
    
    // Compute dlog(ref_pt, v)
    Eigen::MatrixXd aux_matr_2 = dlog_cpp(ref_pt, v);
    
    // Add them
    Eigen::MatrixXd aux_matr_3 = aux_matr_1 + aux_matr_2;
    
    // Compute matrix exponential using eigendecomposition
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(aux_matr_3);
    if (es.info() != Eigen::Success) {
        Rcpp::stop("Eigendecomposition failed");
    }
    
    Eigen::VectorXd exp_eigenvalues = es.eigenvalues().array().exp();
    Eigen::MatrixXd result = es.eigenvectors() * exp_eigenvalues.asDiagonal() * es.eigenvectors().transpose();
    
    return result;
}