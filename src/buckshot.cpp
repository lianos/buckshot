#include "buckshot.h"

SEXP
create_shotgun_data_dense(SEXP matrix_, SEXP labels_) {
BEGIN_RCPP
    Rcpp::NumericMatrix matrix(matrix_);
    Rcpp::NumericVector labels(labels_);
    SEXP out;
    
    int nrows = matrix.nrow();
    int ncols = matrix.ncol();
    
    shotgun_data *prob = new shotgun_data;
    prob->A_rows.resize(nrows);
    prob->A_cols.resize(ncols);
    prob->y.resize(nrows);
    prob->nx = ncols;
    prob->ny = nrows;
    
    int i = 0;
    double val;
    
    for (int col = 0; col < ncols; col++) {
        for (int row = 0; row < nrows; row++) {
            val = matrix[i++];
            if (val != 0) {
                prob->A_cols[col].add(row, val);
                prob->A_rows[row].add(col, val);
            }
        }
    }
    
    for (i = 0; i < nrows; i++) {
        // prob->y.push_back(labels[i]);
        prob->y[i] = labels[i];
    }
    
    out = R_MakeExternalPtr(prob, R_NilValue, R_NilValue);
    R_RegisterCFinalizer(out, shotgun_data_finalizer);
    return out;
END_RCPP
}

SEXP
create_shotgun_data_csparse(SEXP matrix_, SEXP nnz_, SEXP nrows_, SEXP ncols_,
                            SEXP labels_) {
BEGIN_RCPP
    int nnz = Rcpp::as<int>(nnz_);
    int nrows = Rcpp::as<int>(nrows_);
    int ncols = Rcpp::as<int>(ncols_);
    Rcpp::NumericVector labels(labels_);
    SEXP out;
    
    shotgun_data *prob = new shotgun_data;
    prob->A_rows.resize(nrows);
    prob->A_cols.resize(ncols);
    prob->y.resize(nrows);
    prob->nx = ncols;
    prob->ny = nrows;
    
    
    // TODO: Fill in sparse matrix stuff
    
    for (int i = 0; i < nrows; i++) {
        prob->y[i] = labels[i];
    }
    
    out = R_MakeExternalPtr(prob, R_NilValue, R_NilValue);
    R_RegisterCFinalizer(out, shotgun_data_finalizer);
    return out;
END_RCPP
}

SEXP
buckshot_lasso(SEXP prob_, SEXP lambda_, SEXP path_length_,
               SEXP threshold_, SEXP max_iter_, SEXP threads_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    int path_length = Rcpp::as<int>(path_length_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    int numthreads = Rcpp::as<int>(threads_);
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    
    if (numthreads>0) {
        #ifndef DISABLE_OMP
        omp_set_num_threads(numthreads);
        Rprintf("OMP threads = %d\n", numthreads);
        #endif
    }
    
    solveLasso(prob, lambda, path_length, threshold, max_iter, 0);
    return R_NilValue;
END_RCPP
}

SEXP
buckshot_logreg(SEXP prob_, SEXP lambda_, SEXP threshold_,
                SEXP max_iter_, SEXP threads_) {
BEGIN_RCPP
    double lambda = Rcpp::as<double>(lambda_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    bool all_zero = false;
    int numthreads = Rcpp::as<int>(threads_);
    
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    //Rprintf("*prob info: (nrows,ny):(%d,%d) (ncol,nx):(%d,%d) labels=%d\n",
    //        prob->A_rows.size(), prob->ny,
    //        prob->A_cols.size(), prob->nx,
    //        prob->y.size());
    
    if (numthreads>0) {
        #ifndef DISABLE_OMP
        omp_set_num_threads(numthreads);
        Rprintf("OMP threads = %d\n", numthreads);
        #endif
    }
    
    compute_logreg(prob, lambda, threshold, max_iter, 0, all_zero);
    return R_NilValue;
END_RCPP
}

/* ---------------------------- retire me ---------------------------------- */
#if 0
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
#endif
