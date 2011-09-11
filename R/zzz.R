.onLoad <- function(lib, pkg) {
  if (.Platform$r_arch != 'x86_64') {
    warning("\n",
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n",
      ">>>> Sorry, the shotgun solver will only run on x86_64 architectures. <<<<\n",
      ">>>> Running `buckshot`, `logreg`, or `lasso` will result in error.   <<<<\n",
      ">>>>                                                                  <<<<\n",
      ">>>>     Perhaps you can try the glmnet package for your needs.       <<<<\n",
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n")
  }
  
  ## First call to X %*% b acts weird in X is a TsparseMatrix, so let's fire
  ## one off here.
  # M <- Matrix(1:20, 5, sparse=TRUE)
  # M[sample(20, 5)] <- 0
  # M <- as(M, 'dgTMatrix')
  # suppressMessages(M %*% 1:ncol(M))
  # 
  # options(Matrix.verbose=10)
  invisible(NULL)
}
