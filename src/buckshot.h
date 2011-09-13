#ifndef __BUCKSHOT_H__
#define __BUCKSHOT_H__

#include <RcppCommon.h>
// do you want to overlaod as<> and wrap?
#include <Rcpp.h>

#include "common.h"

extern "C" {

/** 
 * This function is run on the pointer to the shotgun_data object stored
 * in a BuckshotData@ptr slot when the BuckshotData object is GC'd on
 * the R side
 */
static void shotgun_data_finalizer(SEXP sptr) {
    // Rprintf("deleting shotgun_data pointer\n");
    shotgun_data *ptr = (shotgun_data *) R_ExternalPtrAddr(sptr);
    delete ptr;
}

}

/**
 * Creates a shotgun_data object from a dense (normal) R matrix and a vecctor
 * of labels (nrow(matrix_) == length(labels))
 */
RcppExport SEXP
create_shotgun_data_dense(SEXP matrix_, SEXP labels_);

/**
 * Creates a shotgun_data object from a Matrix::CsparseMatrix object
 * 
 * FIXME: This doesn't work yet, currently a CsparseMatrix is first converted
 * to a TsparseMatrix, then create_shotgun_data_tsparse is invoked.
 */
RcppExport SEXP
create_shotgun_data_csparse(SEXP vals_, SEXP rows_, SEXP cols_, 
                            SEXP nrows_, SEXP ncols_, SEXP labels_);

/**
 * Creates a shotgun_data object from a (essentially) TspraseMatrix object
 * 
 * @param vals_ double[] is a vector of values in the matrix
 * @param rows_ int[] is an integer vector indicating the 0-based row index of
 * corresponding item in vals_
 * @param cols_ int[] is an integer vector indicating the 0-based column index
 * of corresponding item in vals_
 * @param nrows_ int Number of corows of the design matrix
 * @param ncols_ int Number of columns of the design matrix
 */
RcppExport SEXP
create_shotgun_data_tsparse(SEXP vals_, SEXP rows_, SEXP cols_, 
                            SEXP nrows_, SEXP ncols_, SEXP labels_);

/**
 * Pulls the labels out of a shotgun_data object and returns it to R
 */
RcppExport SEXP
shotgun_data_labels(SEXP prob_);

/**
 * Pulls out the values, rows, and columns of the design matrix from a
 * shotgun_data object and returns a named list with this back to R.
 */
RcppExport SEXP
shotgun_design_matrix(SEXP prob_);

/**
 * Runs lasso/logist-regression on a shogtun_data object `prob_` with the
 * paremeters supplied.
 * 
 * Returns a (double) numeric vector of the estimeated coefficients.
 */
RcppExport SEXP
do_buckshot(SEXP prob_, SEXP algo_, SEXP lambda_, SEXP path_length_,
            SEXP threshold_, SEXP max_iter_, SEXP threads_, SEXP verbose_);

#endif /* __BUCKSHOT_H__ */
