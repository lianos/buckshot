#ifndef __BUCKSHOT_H__
#define __BUCKSHOT_H__

#include <RcppCommon.h>
// do you want to overlaod as<> and wrap?
#include <Rcpp.h>

#include "common.h"

extern "C" {

static void shotgun_data_finalizer(SEXP sptr) {
    // Rprintf("deleting shotgun_data pointer\n");
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
do_buckshot(SEXP prob_, SEXP algo_, SEXP lambda_, SEXP path_length_,
            SEXP threshold_, SEXP max_iter_, SEXP threads_, SEXP verbose_);

#endif /* __BUCKSHOT_H__ */