#ifndef __BUCKSHOT_H__
#define __BUCKSHOT_H__

#include <RcppCommon.h>
// do you want to overlaod as<> and wrap?
#include <Rcpp.h>

#include "shotgun/common.h"

RcppExport SEXP
lasso(x_, y_, lambda_, path_length_, threshold_, max_iter_);

RcppExport SEXP
logreg(x_, y_, lambda_, threshold_, max_iter_);

#endif