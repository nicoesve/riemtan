#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

// Forward declaration of safe_logm_cpp
Eigen::MatrixXd safe_logm_cpp(const Eigen::MatrixXd& X);

// [[Rcpp::export]]
Eigen::MatrixXd dexp_cpp(const Eigen::MatrixXd& a, const Eigen::MatrixXd& x, int num_points = 100) {
    int n = a.rows();
    double dt = 1.0 / (num_points - 1);
    
    Eigen::MatrixXd result = Eigen::MatrixXd::Zero(n, n);
    
    for (int i = 0; i < num_points; i++) {
        double t = i * dt;
        
        // Compute matrix exponentials using eigendecomposition
        Eigen::MatrixXd left_exp_arg = (1 - t) * a;
        Eigen::MatrixXd right_exp_arg = t * a;
        
        // Left exponential
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es_left(left_exp_arg);
        if (es_left.info() != Eigen::Success) {
            Rcpp::stop("Eigendecomposition failed for left exponential");
        }
        Eigen::VectorXd exp_eigenvalues_left = es_left.eigenvalues().array().exp();
        Eigen::MatrixXd gamma_left = es_left.eigenvectors() * exp_eigenvalues_left.asDiagonal() * es_left.eigenvectors().transpose();
        
        // Right exponential
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es_right(right_exp_arg);
        if (es_right.info() != Eigen::Success) {
            Rcpp::stop("Eigendecomposition failed for right exponential");
        }
        Eigen::VectorXd exp_eigenvalues_right = es_right.eigenvalues().array().exp();
        Eigen::MatrixXd gamma_right = es_right.eigenvectors() * exp_eigenvalues_right.asDiagonal() * es_right.eigenvectors().transpose();
        
        // Accumulate the integral
        result += gamma_left * x * gamma_right * dt;
    }
    
    return result;
}

// [[Rcpp::export]]
Eigen::MatrixXd dlog_cpp(const Eigen::MatrixXd& sigma, const Eigen::MatrixXd& h, int num_points = 100) {
    int n = sigma.rows();
    double dt = 1.0 / (num_points - 1);
    
    // Create identity matrix
    Eigen::MatrixXd aux_matr = Eigen::MatrixXd::Identity(n, n);
    Eigen::MatrixXd result = Eigen::MatrixXd::Zero(n, n);
    
    for (int i = 0; i < num_points; i++) {
        double t = i * dt;
        
        // Compute gamma_t = t * sigma + (1 - t) * I
        Eigen::MatrixXd gamma_t = t * sigma + (1 - t) * aux_matr;
        
        // Compute gamma_t_inv using LDLT decomposition for efficiency
        Eigen::MatrixXd gamma_t_inv = gamma_t.ldlt().solve(Eigen::MatrixXd::Identity(n, n));
        
        // Accumulate the integral
        result += gamma_t_inv * h * gamma_t_inv * dt;
    }
    
    return result;
}