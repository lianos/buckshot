#include "Rcpp.h"
#include "mmio.h"

RcppExport SEXP
read_matrix_market(SEXP file_path_, SEXP as_sparse_);
