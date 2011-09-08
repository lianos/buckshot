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
do_buckshot(SEXP prob_, SEXP algo_, SEXP lambda_, SEXP path_length_,
            SEXP threshold_, SEXP max_iter_, SEXP threads_, SEXP verbose_) {
BEGIN_RCPP
    shotgun_data *prob = (shotgun_data*) R_ExternalPtrAddr(prob_);
    std::string algo = Rcpp::as<std::string>(algo_);
    double lambda = Rcpp::as<double>(lambda_);
    int path_length = Rcpp::as<int>(path_length_);
    double threshold = Rcpp::as<double>(threshold_);
    int max_iter = Rcpp::as<int>(max_iter_);
    int numthreads = Rcpp::as<int>(threads_);
    int verbose = (Rcpp::as<bool>(verbose_)) ? 1 : 0;
    std::vector<double> xout;
    
    bool all_zero = false;
    
    // Simon says to set threads via the OMP_NUM_THREADS environment var
    // http://article.gmane.org/gmane.comp.lang.r.devel/28836
    // if (numthreads > 0) {
    //     #ifndef DISABLE_OMP
    //     omp_set_num_threads(numthreads);
    //     if (verbose) {
    //       Rprintf("  Setting OMP threads: %d\n", numthreads);
    //     }
    //     #endif
    // }
    
    if (algo.compare("lasso") == 0) {
        solveLasso(prob, lambda, path_length, threshold, max_iter, verbose);
    } else if (algo.compare("logistic") == 0) {
        compute_logreg(prob, lambda, threshold, max_iter, verbose, all_zero);
    } else {
        throw std::runtime_error("Unknown algorithm");
    }
    
    xout.resize(prob->nx);
    for (int i = 0; i < prob->nx; i++) {
        xout[i] = prob->x[i];
    }
    
    return Rcpp::wrap(xout);
END_RCPP
}

