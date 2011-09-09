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
  
  rm.cols <- which(colSums(x) == 0)
  if (length(rm.cols) > 0L) {
    warning("Removing ", length(rm.cols), " columns from design matrix ",
            "since they are all 0s. See ?BuckshotData for more info.")
    x <- x[, -rm.cols]
  }
  
  ret <- .Call("create_shotgun_data_dense", x, y, PACKAGE="buckshot")
  new("BuckshotData", ptr=ret$ptr, dim=dim(x), nnz=ret$nnz, rm.cols=rm.cols)
})

setMethod("BuckshotData", c(x="Matrix"),
function(x, y, ...) {
  BuckshotData(as(x, "matrix"), y, ...)
})

setMethod("BuckshotData", c(x="CsparseMatrix"),
function(x, y, ...) {
  stop("TODO: BuckshotData,sparseMatrix")
  
  rm.cols <- which(colSums(x) == 0)
  if (length(rm.cols) > 0L) {
    x <- x[, -rm.cols]
  }
  
  ret <- .Call("create_shotgun_data_csparse", x)
  new("BuckshotData", ptr=ret$ptr, dim=dim(x), nnz=ret$nnz, rm.cols=rm.cols)
})

# setMethod("BuckshotData", c(x="sparseMatrix"),
# function(x, y, ...) {
#   stop("TODO: BuckshotData,sparseMatrix")
# })

setMethod("designMatrix", c(x="BuckshotData"),
function(x, ...) {
  stop("Reconstructing designMatrix not supported yet")
  ret <- .Call("shotgun_design_matrix", x@ptr, PACKAGE="buckshot")
})

setMethod("labels", c(object="BuckshotData"),
function(object, ...) {
  .Call("shotgun_data_labels", object@ptr, PACKAGE="buckshot")
})
