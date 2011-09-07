setGeneric("BuckshotData", function(x, ...) standardGeneric("BuckshotData"))

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
  if (missing(y) || !is.numeric(y))
    stop("Numberic labels (y) are required")
  }
  if (length(y) != nrow(x)) {
    stop("Number of labels != number of observations")
  }
  
  storage.mode(x) <- 'numeric'
  y <- as.numeric(y)
  
  idxs <- which(x != 0, arr.ind=TRUE)
  
  ret <- BuckshotData(idxs[, 1L], idxs[, 2L], x[idxs], nrow(x), ncol(x), y)
  ret
})

setMethod("BuckshotData", c(x="Matrix"),
function(x, ...) {
  stop("TODO: BuckshotData,Matrix")
})

setMethod("BuckshotData", c(x="sparseMatrix"),
function(x, ...) {
  stop("TODO: BuckshotData,sparseMatrix")
})

setMethod("BuckshotData", c(x="integer"),
function(x, cols, values, nrows, ncols, labels, ...) {
  lengths <- c(length(x), length(cols), length(values))
  stopifnot(length(unique(lengths)) == 1L)
  
  stopifnot(is.integer(x))
  stopifnot(is.integer(cols))
  
  stopifnot(is.integer(nrows))
  stopifnot(is.integer(ncols))
  
  ret <- .Call("create_shotgun_data", x, cols, values, nrows, ncols,
               PACKAGE="buckshot")
})
