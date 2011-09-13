##' Builds a BuckshotData object from a formula
##' 
##' TODO: Support data building from formula
setMethod("BuckshotData", c(x="formula"),
function(x, data=NULL, ..., na.action=na.omit, scaled=TRUE) {
  ## This majority of this formula fiddling code is taken from kernlab
  stop("building data objects from formulas is not supported yet")
  
  cl <- match.call()
  m <- match.call(expand.dots=FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) {
    m$data <- as.data.frame(data)
  }
  m$... <- NULL
  m$formula <- m$x
  m$x <- NULL
  m$scaled <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0    ## no intercept

  x <- model.matrix(Terms, m)
  y <- model.extract(m, response)

  if (length(scaled) == 1) {
    scaled <- rep(scaled, ncol(x))
  }
  if (any(scaled)) {
    remove <- unique(c(which(labels(Terms) %in% names(attr(x, "contrasts"))),
                       which(!scaled)))
    scaled <- !attr(x, "assign") %in% remove
  }
  
  ret <- BuckshotData(x, y, scaled=scaled, ...)
  
  ret@cache$call <- m
  ret@cache$scaled <- scaled
  # kcall(ret) <- cl
  # attr(Terms,"intercept") <- 0 ## no intercept
  # terms(ret) <- Terms
  #
  # if (!is.null(attr(m, "na.action"))) {
  #   n.action(ret) <- attr(m, "na.action")
  # }

  ret
})

preprocess.xy <- function(x, y) {
  if (!is.numeric(x[1L])) {
    stop("Only numeric data is supported")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("NA values not supported")
  }
  if (!is.matrix(x) && !inherits(x, 'Matrix')) {
    stop("x needs to be a matrix (or Matrix)")
  }
  
  ## y
  if (missing(y)) {
    stop("Numeric labels (y) are required")
  }
  if (is.matrix(y)) {
    dim(y) <- NULL
  }
  if (inherits(y, 'Matrix')) {
    y <- as.numeric(y)
  }
  if (!is.numeric(y)) {
    stop("Numeric labels (y) are required")
  }
  if (nrow(x) != length(y)) {
    stop("Number of labels != number of observations")
  }
  
  rm.cols <- which(colSums(x) == 0)
  if (length(rm.cols) > 0L) {
    warning("Removing ", length(rm.cols), " columns from design matrix ",
            "since they are all 0s. See ?BuckshotData for more info.")
    x <- x[, -rm.cols]
  }
  
  list(x=x, y=as.numeric(y), rm.cols=rm.cols)
}

setMethod("BuckshotData", c(x="matrix"),
function(x, y, scaled=TRUE, ...) {
  dat <- preprocess.xy(x, y)
  storage.mode(dat$x) <- 'numeric'
  ret <- .Call("create_shotgun_data_dense", dat$x, dat$y, PACKAGE="buckshot")
  new("BuckshotData", ptr=ret$ptr, dim=dim(dat$x),
      dimnames=list(rownames(dat$x), colnames(dat$x)),
      nnz=ret$nnz, rm.cols=dat$rm.cols)
})

setMethod("BuckshotData", c(x="Matrix"),
function(x, y, ...) {
  BuckshotData(as(x, "matrix"), y, ...)
})

setMethod("BuckshotData", c(x="TsparseMatrix"),
function(x, y, ...) {
  dat <- preprocess.xy(x, y)
  ret <- .Call("create_shotgun_data_tsparse", as.numeric(dat$x@x),
               dat$x@i, dat$x@j, nrow(dat$x), ncol(dat$x), dat$y,
               PACKAGE="buckshot")
  new("BuckshotData", ptr=ret$ptr, dim=dim(x), dimnames=dimnames(dat$x),
      nnz=ret$nnz, rm.cols=dat$rm.cols)
})

setMethod("BuckshotData", c(x="CsparseMatrix"),
function(x, y, ...) {
  ## TODO: implement BuckshotData,CsparseMatrix -- a TsparseMatrix
  ## will have to be converted to a CsparseMatrix in the predict function
  ## anyway.
  return(BuckshotData(as(x, "TsparseMatrix"), y, ...))
  
  ## TODO: Figure out how to properly index CsparseMatrix in order to
  ##       making a copy of the "default" sparse matrix type in the Matrix
  ##       package.
  dat <- preprocess.xy(x, y)
  x <- dat$x
  y <- dat$y
  
  ## x@x : The non-zero values of the matrix, column order
  ## x@p : 0-based-index position of the *first* element the n-th column
  ## x@i : 0-based row index for the i-5h value in @x
  ## x@Dimnames : length(2) of dimnames
  ## x@factors : empty list (of wut?)
  ## x@Dim : length 2 integer vector of row,col dims
  
  ret <- .Call("create_shotgun_data_csparse", as.numeric(x@x), x@i, x@p,
               nrow(x), ncol(x), y, PACKAGE="buckshot")
  new("BuckshotData", ptr=ret$ptr, dim=dim(dat$x), dimnames=dimnames(dat$x),
      nnz=ret$nnz, rm.cols=dat$rm.cols)
})

# setMethod("BuckshotData", c(x="sparseMatrix"),
# function(x, y, ...) {
#   stop("TODO: BuckshotData,sparseMatrix")
# })

setMethod("designMatrix", c(x="BuckshotData"),
function(x, ...) {
  ## TODO: Return a CsparseMatrix matrix designMatrx,BuckshotData
  ret <- .Call("shotgun_design_matrix", x@ptr, PACKAGE="buckshot")
  new("dgTMatrix", x=ret$vals, i=ret$rows, j=ret$cols, Dim=dim(x),
      Dimnames=dimnames(x))
})

setMethod("labels", c(object="BuckshotData"),
function(object, ...) {
  .Call("shotgun_data_labels", object@ptr, PACKAGE="buckshot")
})
