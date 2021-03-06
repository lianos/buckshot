.onLoad <- function(lib, pkg) {
  if (.Machine$sizeof.pointer != 8) {
    warning("\n",
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n",
      ">>>> Sorry, the shotgun solver will only run on x86_64 architectures. <<<<\n",
      ">>>> Running `buckshot`, `logreg`, or `lasso` will result in error.   <<<<\n",
      ">>>>                                                                  <<<<\n",
      ">>>>     Perhaps you can try the glmnet package for your needs.       <<<<\n",
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n")
  }
  invisible(NULL)
}
