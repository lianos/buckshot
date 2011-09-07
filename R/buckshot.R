matchLearningAlgo <- function(x, as.int=FALSE) {
  algos <- c('lasso', 'logistic')
  algo <- match.arg(x, algos)
  if (as.int) {
    algo <- match(algo, algos)
  }
  algo
}

setGeneric("buckshot", function(x, ...), standardGeneric("buckshot"))
setMethod("buckshot", c(x="formula"),
function(x, data=NULL, ..., subset, na.action=na.omit, scaled=TRUE) {
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

  ret <- buckshot(x, y, scaled=scaled, ...)
  # kcall(ret) <- cl
  # attr(Terms,"intercept") <- 0 ## no intercept
  # terms(ret) <- Terms

  if (!is.null(attr(m, "na.action"))) {
    n.action(ret) <- attr(m, "na.action")
  }

  ret
})

setMethod("buckshot", c(x="matrix"),
function(x, y=NULL, type='lasso', lambda=1, path.length=1L,
         convergence.threshold=1e-5, max.iter=100L, scaled=TRUE, ...) {
  if (!is.numeric(x) || is.integer(x)) {
    stop("A numeric matrix is reqeuired for `x`")
  }
  if (missing(y) || is.null(y)) {
    stop("Labels (y) required")
  }
  if (length(y) != nrow(x)) {
    stop("Number of labels != number of observations")
  }
  type <- matchLearningAlgo(type)
  
  ## Ensure date types are correct
  y <- as.numeric(y) ## shotgun values are `double`s
  lambda <- as.numeric(lambda)
  path.length <- as.integer(path.length)
  max.iter <- as.integer(max.iter)
  convergence.threshold <- as.numeric(convergence.threshold)
  
  if (type == 'lasso') {
    ret <- .Call("lasso", x, y, lambda, path.length, convergence.threshold,
                 max.iter, PACKAGE="buckshot")
  } else {
    ret <- .Call("logreg", x, y, lambda, convergence.threshold, max.iter,
                 PACKAGE="buckshot")
  }
})