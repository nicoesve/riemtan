#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Forward declarations
arma::mat safe_logm_cpp(const arma::mat& X);
arma::mat dexp_cpp(const arma::mat& a, const arma::mat& x, int num_points = 100);
arma::mat dlog_cpp(const arma::mat& sigma, const arma::mat& h, int num_points = 100);

// [[Rcpp::export]]
arma::mat log_euclidean_log_cpp(const arma::mat& sigma, const arma::mat& lambda) {
    // Compute matrix logarithms
    arma::mat aux_matr_1 = safe_logm_cpp(sigma);
    arma::mat aux_matr_2 = safe_logm_cpp(lambda);
    
    // Compute difference
    arma::mat aux_matr_3 = aux_matr_2 - aux_matr_1;
    
    // Make symmetric
    aux_matr_1 = 0.5 * (aux_matr_1 + aux_matr_1.t());
    aux_matr_3 = 0.5 * (aux_matr_3 + aux_matr_3.t());
    
    // Call dexp
    return dexp_cpp(aux_matr_1, aux_matr_3);
}

// [[Rcpp::export]]
arma::mat log_euclidean_exp_cpp(const arma::mat& ref_pt, const arma::mat& v) {
    // Compute matrix logarithm of ref_pt using safe_logm_cpp
    arma::mat aux_matr_1 = safe_logm_cpp(ref_pt);
    
    // Compute dlog(ref_pt, v)
    arma::mat aux_matr_2 = dlog_cpp(ref_pt, v);
    
    // Add them
    arma::mat aux_matr_3 = aux_matr_1 + aux_matr_2;
    
    // Compute matrix exponential
    arma::mat result = arma::expmat(aux_matr_3);
    
    return result;
}