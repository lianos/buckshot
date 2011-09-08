setMethod("BuckshotData", c(x="formula"),
function(x, data=NULL, ..., na.action=na.omit, scaled=TRUE) {
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
  
  ptr <- .Call("create_shotgun_data_dense", x, y, PACKAGE="buckshot")
  new("BuckshotData", ptr=ptr)
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

# setMethod("BuckshotData", c(x="integer"),
# function(x, cols, values, nrows, ncols, y, ...) {
#   ## Double check types of all parameters
#   lengths <- c(length(x), length(cols), length(values))
#   stopifnot(length(unique(lengths)) == 1L)
#   stopifnot(length(y) == nrows)
#   
#   stopifnot(is.integer(x))
#   stopifnot(is.integer(cols))
#   stopifnot(is.numeric(y))
#   
#   stopifnot(is.integer(nrows))
#   stopifnot(is.integer(ncols))
#   
#   stopifnot(all(x >= 1 & x <= nrows))
#   stopifnot(all(cols >= 1 & cols <= ncols))
#   
#   ptr <- .Call("create_shotgun_data", x, cols, values, nrows, ncols, y,
#                PACKAGE="buckshot")
#   new('BuckshotData', ptr=ptr)
# })

load.matrix.mart <- function(mtx.file) {
  mtx <- read.table(mtx.file, header=FALSE, comment.char="%")
  nr <- mtx[1,1]
  nc <- mtx[1,2]
  nnz <- mtx[1,3]
  
  mtx <- mtx[-1,]
  m <- matrix(0, nrow=nr, ncol=nc)
  m[mtx[,1] * mtx[,2]] <- mtx[,3]
  m
}
