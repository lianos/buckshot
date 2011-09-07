#include "buckshot.h"

SEXP create_shotgun_data(SEXP rows_, SEXP cols_, SEXP vals_,
                         SEXP nrows_, SEXP ncols_, SEXP y_) {
BEGIN_RCPP
    Rcpp::IntegerVector rows(rows_);
    Rcpp::IntegerVector cols(cols_);
    Rcpp::NumericVector vals(vals_);
    Rcpp::NumericVector y(y_);
    
    int N = rows.size();
    int nrows = Rcpp::as<int>(nrows_);
    int ncols = Rcpp::as<int>(ncols_);
    int row, col;
    double val;
    
    SEXP out;
    
    shotgun_data *prob = new shogun_data;
    
    prob->A_rows.reserve(nrows);
    prob->A_cols.reserve(ncols);
    
    for (int i = 0; i < N; i++) {
        row = rows[i]--; // set to 0 index offset
        col = cols[i]--;
        val = vals[i];
        prob->A_rows[row].add(col, val);
        prob->A_cols[col].add(row, val);
    }
    
    prob->nx = ncols;
    prob->ny = nrows;
    std::copy(y.begin(), y.end(), prob->y);
    
    out = R_MakeExternalPtr(prob, R_NilValue, R_NilValue);
    R_RegisterCFinalizer(out, shotgun_data_finalizer);
    return out;
END_RCPP
}

SEXP lasso(prob_, lambda_, path_length_, threshold_, max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    int path_length = Rcpp::as<int>(path_length_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    
    solveLasso(prob, lambda, K, threshold, maxiter, verbose);
    return R_NilValue;
END_RCPP
}

SEXP logreg(prob_, lambda_, threshold_, max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    // TODO: create shotgun_data object from x,y
    // convert_2_mat(matfile, &prob);
    // convert_2_vec(vecfile, &prob);
    
    compute_logreg(prob, lambda, threshold, maxiter, verbose, all_zero);
    return R_NilValue;
END_RCPP
}
