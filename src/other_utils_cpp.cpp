#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Forward declaration of safe_logm_cpp
arma::mat safe_logm_cpp(const arma::mat& X);

// [[Rcpp::export]]
arma::mat dexp_cpp(const arma::mat& a, const arma::mat& x, int num_points = 100) {
    int n = a.n_rows;
    double dt = 1.0 / (num_points - 1);
    
    arma::mat result = arma::zeros(n, n);
    
    for (int i = 0; i < num_points; i++) {
        double t = i * dt;
        
        // Compute matrix exponentials
        arma::mat gamma_left = arma::expmat((1 - t) * a);
        arma::mat gamma_right = arma::expmat(t * a);
        
        // Accumulate the integral
        result += gamma_left * x * gamma_right * dt;
    }
    
    return result;
}

// [[Rcpp::export]]
arma::mat dlog_cpp(const arma::mat& sigma, const arma::mat& h, int num_points = 100) {
    int n = sigma.n_rows;
    double dt = 1.0 / (num_points - 1);
    
    // Create identity matrix
    arma::mat aux_matr = arma::eye(n, n);
    arma::mat result = arma::zeros(n, n);
    
    for (int i = 0; i < num_points; i++) {
        double t = i * dt;
        
        // Compute gamma_t = t * sigma + (1 - t) * I
        arma::mat gamma_t = t * sigma + (1 - t) * aux_matr;
        
        // Compute gamma_t_inv
        arma::mat gamma_t_inv = arma::inv(gamma_t);
        
        // Accumulate the integral
        result += gamma_t_inv * h * gamma_t_inv * dt;
    }
    
    return result;
}