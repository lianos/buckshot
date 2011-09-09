#include "buckshot_mm.h"

SEXP
read_matrix_market(SEXP file_path_, SEXP as_sparse_) {
BEGIN_RCPP
    throw std::runtime_error("TODO: Need to fix memory not mapped errors when reading banner");
    std::string file_path = Rcpp::as<std::string>(file_path_);
    const char *filename = file_path.c_str();
    bool as_sparse = Rcpp::as<bool>(as_sparse_);
    std::vector<int> rows, cols;
    std::vector<double> vals;
    
    int ret_code;
    MM_typecode matcode;
    FILE *f;
    int M, N, nz;   
    
    Rprintf("openning file\n");
    if ((f = fopen(filename, "r")) == NULL) {
        std::runtime_error("Could not find Matrix Market input file");
    }
    
    Rprintf("reading banner\n");
    if (mm_read_banner(f, &matcode) != 0) {
        std::runtime_error("Could not process Matrix Market banner in inputs file");
    }

    /*  This is how one can screen matrix types if their application */
    /*  only supports a subset of the Matrix Market data types.      */
    Rprintf("checking matrix type\n");
    if (mm_is_complex(matcode) && mm_is_matrix(matcode) && 
        mm_is_sparse(matcode) ) {
        std::runtime_error("Matrix Market type not supported");
    }

    /* find out size of sparse matrix .... */
    Rprintf("getting coords\n");
    if ((ret_code = mm_read_mtx_crd_size(f, &M, &N, &nz)) !=0){
        std::runtime_error("Could not process Matrix Market size in input file");
    }

    /* reseve memory for matrices */

    /* NOTE: when reading in doubles, ANSI C requires the use of the "l"  */
    /*   specifier as in "%lg", "%lf", "%le", otherwise errors will occur */
    /*  (ANSI C X3.159-1989, Sec. 4.9.6.2, p. 136 lines 13-15)            */

    int I,J; 
    double val;
    
    rows.reserve(nz);
    cols.reserve(nz);
    vals.reserve(nz);
    
    for (int i = 0; i < nz; i++) {
        // Indices are 1-based
        Rprintf("item %d", i);
        fscanf(f, "%d %d %lg\n", &I, &J, &val);
        rows.push_back(I);
        cols.push_back(J);
        vals.push_back(val);
    }
    
    if (f !=stdin) fclose(f);
    
    return Rcpp::List::create(
        Rcpp::Named("nrows") = Rcpp::wrap(M),
        Rcpp::Named("ncols") = Rcpp::wrap(N),
        Rcpp::Named("rows") = Rcpp::wrap(rows),
        Rcpp::Named("cols") = Rcpp::wrap(cols),
        Rcpp::Named("vals") = Rcpp::wrap(vals));
END_RCPP
}
