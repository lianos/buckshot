#include "buckshot.h"

SEXP
create_shotgun_data(SEXP rows_, SEXP cols_, SEXP vals_,
                    SEXP nrows_, SEXP ncols_, SEXP y_) {
BEGIN_RCPP
    Rcpp::IntegerVector rows(rows_);
    Rcpp::IntegerVector cols(cols_);
    Rcpp::NumericVector vals(vals_);
    Rcpp::NumericVector y(y_);
    
    int nnz = rows.size();
    int nrows = Rcpp::as<int>(nrows_);
    int ncols = Rcpp::as<int>(ncols_);
    int row, col;
    double val;
    
    SEXP out;
    
    shotgun_data *prob = new shotgun_data;
    
    prob->A_rows.resize(nrows);
    prob->A_cols.resize(ncols);
    
    for (int i = 0; i < nnz; i++) {
        Rprintf("%d:\n", i);
        row = rows[i] - 1; // set to 0 index offset
        col = cols[i] - 1;
        val = vals[i];
        Rprintf("  (row, col, val) : %d %d %g\n",
            row, col, val);
        prob->A_rows[row].add(col, val);
        prob->A_cols[col].add(row, val);
    }
    
    prob->y.resize(nrows);
    for (int i = 0; i < nrows; i++) {
        Rprintf("  label %g\n", y[i]);
        prob->y.push_back(y[i]);
    }
    
    prob->nx = ncols;
    prob->ny = nrows;
    
    // std::copy(y.begin(), y.end(), prob->y);
    
    Rprintf("Making ext ptrs\n");
    out = R_MakeExternalPtr(prob, R_NilValue, R_NilValue);
    R_RegisterCFinalizer(out, shotgun_data_finalizer);
    return out;
END_RCPP
}

SEXP
buckshot_lasso(SEXP prob_, SEXP lambda_, SEXP path_length_,
               SEXP threshold_, SEXP max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    int path_length = Rcpp::as<int>(path_length_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    
    solveLasso(prob, lambda, path_length, threshold, max_iter, 0);
    return R_NilValue;
END_RCPP
}

SEXP
buckshot_logreg(SEXP prob_, SEXP lambda_, SEXP threshold_,
                SEXP max_iter_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    bool all_zero = false;
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    // TODO: create shotgun_data object from x,y
    // convert_2_mat(matfile, &prob);
    // convert_2_vec(vecfile, &prob);
    
    compute_logreg(prob, lambda, threshold, max_iter, 0, all_zero);
    return R_NilValue;
END_RCPP
}
