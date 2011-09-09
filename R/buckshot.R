##' Perform lasso regression
lasso <- function(x, ...) {
  buckshot(x, type='lasso', ...)
}

##' Perform logistic regression
logreg <- function(x, ...) {
  buckshot(x, type='logistic', ...)
}

##' Builds a buckshot model from a formula object
##' 
##' TODO: Support formula model creation
setMethod("buckshot", c(x="formula"),
function(x, data=NULL, type='lasso', ..., na.action=na.omit, scaled=TRUE) {
  type <- matchLearningAlgo(type)
  bdata <- BuckshotData(x, data=data, na.action=na.action, scaled=scaled)
  buckshot(bdata, type=type, ...)
})

setMethod("buckshot", c(x="matrix"),
function(x, y, type='lasso', na.action=na.omit, scaled=TRUE, ...) {
  type <- buckshot:::matchLearningAlgo(type)
  if (missing(y)) {
    stop("labels `y` required")
  }
  bdata <- BuckshotData(x, y, na.action=na.omit, scaled=scaled, ...)
  buckshot(bdata, type=type, ...)
})

setMethod("buckshot", c(x="BuckshotData"),
function(x, type='lasso', lambda=1, path.length=1L, max.iter=100L,
         convergence.threshold=1e-5, threads=1, verbose=FALSE, ...) {
  type <- matchLearningAlgo(type)
  if (is.null(x@ptr)) {
    stop("BuckshotData object (x) is not properly initialized")
  }
  
  ## Coerce variables to correct data type
  lambda <- as.numeric(lambda[1L])
  path.length <- as.integer(path.length[1L])
  max.iter <- as.integer(max.iter[1L])
  convergence.threshold <- as.numeric(convergence.threshold[1L])
  threads <- as.integer(threads[1L])
  
  ## Simon says to set threads for OpenMP via the environment vars
  ## http://article.gmane.org/gmane.comp.lang.r.devel/28836
  orig.threads <- Sys.getenv("OMP_NUM_THREADS")
  if (!is.numeric(orig.threads)) orig.threads <- 1L
  on.exit(Sys.setenv(OMP_NUM_THREADS=as.integer(orig.threads)))
  Sys.setenv(OMP_NUM_THREADS=threads)
  
  if (!is.logical(verbose)) {
    verbose <- FALSE
  }
  verbose <- verbose[1L]
  
  ret <- .Call("do_buckshot", x@ptr, type, lambda, path.length,
               convergence.threshold, max.iter, threads, verbose,
               PACKAGE="buckshot")
  
  model <- new("BuckshotModel", data=x, type=type, lambda=lambda,
               path.length=path.length, max.iter=max.iter,
               convergence.threshold=convergence.threshold,
               coefs=ret)
  model
})

# data <- function(...) {
#   if (inherits(..1, 'BuckshotModel')) {
#     ..1@data
#   } else {
#     utils::data(...)
#   }
# }

setMethod("coef", "BuckshotModel",
function(object, ...) {
  object@coefs
})

setMethod("fitted", "BuckshotModel",
function(object, ...) {
  stop("fitted not implemented")
  predict(object, newdata=NULL, type="response")
})

setMethod("predict", "BuckshotModel",
function(object, newdata=NULL, type="decision", ...) {
  if (is.null(newdata)) {
    stop("Autopredicting on training data not supported yet")
  }
  stopifnot(is.matrix(newdata))
  type <- match.arg(type, c('decision', 'response', 'probabilities'))
  if (type == 'probabilities') {
    stop("type=probabilities not implemented")
  }
  
  x <- coef(object)
  if (ncol(newdata) != length(x)) {
    stop("bad dimmensions for `newdata`, ", length(x),
         " columns reqiured")
  }
  
  y <- newdata %*% x

  if (object@type == 'logistic' && type == 'decision') {
    y <- sign(y)
  }
  
  y
})

## ----------------------------------------------------------------------------
## Not exported
matchLearningAlgo <- function(x, as.int=FALSE) {
  algos <- c('lasso', 'logistic')
  algo <- match.arg(x, algos)
  if (as.int) {
    algo <- match(algo, algos)
  }
  algo
}
