#ifndef __BUCKSHOT_H__
#define __BUCKSHOT_H__

#include <RcppCommon.h>
// do you want to overlaod as<> and wrap?
#include <Rcpp.h>

#include "common.h"

extern "C" {

static void shotgun_data_finalizer(SEXP sptr) {
    Rprintf("kiling a data object\n");
    shotgun_data *ptr = (shotgun_data *) R_ExternalPtrAddr(sptr);
    delete ptr;
}

}

RcppExport SEXP
create_shotgun_data_dense(SEXP matrix_, SEXP labels_);

RcppExport SEXP
create_shotgun_data_csparse(SEXP matrix_, SEXP nnz_, SEXP nrows_, SEXP ncols_,
                            SEXP labels_);

RcppExport SEXP
buckshot_lasso(SEXP prob_, SEXP lambda_, SEXP path_length_, SEXP threshold_,
               SEXP max_iter_, SEXP threads_);

RcppExport SEXP
buckshot_logreg(SEXP prob_, SEXP lambda_, SEXP threshold_, SEXP max_iter_,
                SEXP threads_);

/* -------------------------------- retire me --------------------------------*/
#if 0
RcppExport SEXP
create_shotgun_data(SEXP rows_, SEXP cols_, SEXP vals_, SEXP nrows_,
                    SEXP ncols_, SEXP y_);
#endif

#endif /* __BUCKSHOT_H__ */