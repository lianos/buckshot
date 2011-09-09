##' Reads in data in matrix market format
##' 
##' Warning: This is slow.
##' 
##' TODO: Wrap matrix market C code (mmio.{h|c})
##' 
##' @param mtx.file The path to the matrix market file
##' @return A (dense) matrix
read.matrix.mart <- function(mtx.file) {
  mtx <- read.table(mtx.file, header=FALSE, comment.char="%")
  nr <- mtx[1,1]
  nc <- mtx[1,2]
  nnz <- mtx[1,3]
  
  mtx <- mtx[-1,]
  m <- matrix(0, nrow=nr, ncol=nc)
  m[mtx[,1] * mtx[,2]] <- mtx[,3]
  m
}

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

setMethod("BuckshotData", c(x="matrix"),
function(x, y, scaled=TRUE, ...) {
  if (!is.numeric(x)) {
    stop("Only numeric data is supported")
  }
  if (missing(y) || !is.numeric(y)) {
    stop("Numberic labels (y) are required")
  }
  if (length(y) != nrow(x)) {
    stop("Number of labels != number of observations")
  }
  
  storage.mode(x) <- 'numeric'
  y <- as.numeric(y)
  
  ret <- .Call("create_shotgun_data_dense", x, y, PACKAGE="buckshot")
  new("BuckshotData", ptr=ret$ptr, dim=dim(x), nnz=ret$nnz)
})

setMethod("BuckshotData", c(x="Matrix"),
function(x, y, ...) {
  BuckshotData(as(x, "matrix"), y, ...)
})

setMethod("BuckshotData", c(x="CsparseMatrix"),
function(x, y, ...) {
  stop("TODO: BuckshotData,sparseMatrix")
})

# setMethod("BuckshotData", c(x="sparseMatrix"),
# function(x, y, ...) {
#   stop("TODO: BuckshotData,sparseMatrix")
# })
