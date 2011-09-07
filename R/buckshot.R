matchLearningAlgo <- function(x, as.int=FALSE) {
  algos <- c('lasso', 'logistic')
  algo <- match.arg(x, algos)
  if (as.int) {
    algo <- match(algo, algos)
  }
  algo
}

setGeneric("buckshot", function(x, ...) {
  standardGeneric("buckshot")
})

setMethod("buckshot", c(x="formula"),
function(x, data=NULL, type='lasso', ..., na.action=na.omit, scaled=TRUE) {
  # cl <- match.call()
  # m <- match.call(expand.dots=FALSE)
  # if (is.matrix(eval(m$data, parent.frame()))) {
  #   m$data <- as.data.frame(data)
  # }
  # m$... <- NULL
  # m$formula <- m$x
  # m$x <- NULL
  # m$scaled <- NULL
  # m[[1]] <- as.name("model.frame")
  # m <- eval(m, parent.frame())
  # Terms <- attr(m, "terms")
  # attr(Terms, "intercept") <- 0    ## no intercept
  # 
  # x <- model.matrix(Terms, m)
  # y <- model.extract(m, response)
  # 
  # if (length(scaled) == 1) {
  #   scaled <- rep(scaled, ncol(x))
  # }
  # if (any(scaled)) {
  #   remove <- unique(c(which(labels(Terms) %in% names(attr(x, "contrasts"))),
  #                      which(!scaled)))
  #   scaled <- !attr(x, "assign") %in% remove
  # }
  # 
  # ret <- buckshot(x, y, scaled=scaled, ...)
  # # kcall(ret) <- cl
  # # attr(Terms,"intercept") <- 0 ## no intercept
  # # terms(ret) <- Terms
  # 
  # if (!is.null(attr(m, "na.action"))) {
  #   n.action(ret) <- attr(m, "na.action")
  # }
  type <- matchLearningAlgo(type)
  bdata <- BuckshotData(x, data=NULL, na.action=na.action, scaled=scaled)
  buckshot(bdata, type=type, ...)
})

setMethod("buckshot", c(x="matrix"),
function(x, y=NULL, type='lasso', na.action=na.omit, scaled=TRUE, ...) {
  type <- matchLearningAlgo(type)
  bdata <- BuckshotData(x, y, na.action=na.omit, scaled=scaled, ...)
  buckshot(bdata, type=type, ...)
})

setMethod("buckshot", c(x="BuckshotData"),
function(x, type='lasso', lambda=1, path.length=1L, max.iter=100L,
         convergence.threshold=1e-5, ...) {
  type <- matchLearningAlgo(type)
  if (is.null(x@ptr)) {
    stop("BuckshotData object (x) is not properly initialized")
  }
  
  ## Coerce variables to correct data type
  y <- as.numeric(y) ## shotgun values are `double`s
  lambda <- as.numeric(lambda)
  path.length <- as.integer(path.length)
  max.iter <- as.integer(max.iter)
  convergence.threshold <- as.numeric(convergence.threshold)
  
  ## NOTE: matlab examples expect rows=observations, columns=features
  if (type == 'lasso') {
    ret <- .Call("lasso", x@ptr, lambda, path.length, convergence.threshold,
                 max.iter, PACKAGE="buckshot")
  } else {
    ret <- .Call("logreg", x@ptr, lambda, convergence.threshold, max.iter,
                 PACKAGE="buckshot")
  }
})


setMethod("bias", "BuckshotModel",
function(object, ...) {
  stop("TODO: Implement bias,BuckshotModel")
})

setMethod("coef", "BuckshotModel",
function(object, ...) {
  stop("TODO: Implement coef,BuckshotModel")
  ## Return a column matrix
})

setMethod("fitted", "BuckshotModel",
function(object, ...) {
  predict(object, newdata=NULL, type="response")
})

setMethod("predict", "BuckshotModel",
function(object, newdata=NULL, type="response", ...) {
  ## NOTE: There is no bias term, so data should be centered to 0
  type <- match.arg(type, c('response', 'decision', 'probabilities'))
  newdata * coef(object)
}

