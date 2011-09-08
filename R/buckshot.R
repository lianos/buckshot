matchLearningAlgo <- function(x, as.int=FALSE) {
  algos <- c('lasso', 'logistic')
  algo <- match.arg(x, algos)
  if (as.int) {
    algo <- match(algo, algos)
  }
  algo
}

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
function(x, y, type='lasso', na.action=na.omit, scaled=TRUE, ...) {
  type <- matchLearningAlgo(type)
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
  y <- as.numeric(y) ## shotgun values are `double`s
  lambda <- as.numeric(lambda[1L])
  path.length <- as.integer(path.length[1L])
  max.iter <- as.integer(max.iter[1L])
  convergence.threshold <- as.numeric(convergence.threshold[1L])
  
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

setMethod("bias", "BuckshotModel",
function(object, ...) {
  stop("TODO: Implement bias,BuckshotModel")
  ## return 0, or something from scale
})

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
  type <- match.arg(type, c('decision', 'response', 'probabilities'))
  if (is.null(newdata)) {
    stop("Autopredicting on training data not supported yet")
  }
  stopifnot(is.matrix(newdata))
  
  x <- coef(object)
  if (ncol(newdata) != length(x)) {
    stop("bad dimmensions for `newdata`")
  }
  
  y <- newdata %*% x

  if (object@type == 'logistic' && type == 'decision') {
    y <- sign(y)
  }
  
  y
})

