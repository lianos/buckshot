#include "buckshot.h"

SEXP lasso(x_, y_, lambda_, path_length_, threshold_, max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    int path_length = Rcpp::as<int>(path_length_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    
    shotgun_data prob;
    // TODO: create shotgun_data object from x,y
    // convert_2_mat(matfile, &prob);
    // convert_2_vec(vecfile, &prob);
    
    solveLasso(&prob, lambda, K, threshold, maxiter, verbose);
    
    return R_NilValue;
END_RCPP
}

SEXP logreg(x_, y_, lambda_, threshold_, max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    
    shotgun_data prob;
    // TODO: create shotgun_data object from x,y
    // convert_2_mat(matfile, &prob);
    // convert_2_vec(vecfile, &prob);
    
    compute_logreg(&prob, lambda, threshold, maxiter, verbose, all_zero);
    
    return R_NilValue;
END_RCPP
}
