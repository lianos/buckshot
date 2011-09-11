###############################################################################
## Classes
## ----------------------------------------------------------------------------

setClassUnion("ptrOrNULL", c("externalptr", "NULL"))
setClassUnion("MatrixLike", c("matrix", "Matrix"))

##' Base object for Buckshot classes
setClass("BuckshotObject", contains="VIRTUAL",
         representation=representation(cache="environment"))
setMethod("initialize", "BuckshotObject",
function(.Object, ..., cache=new.env()) {
  callNextMethod(.Object, cache=cache, ...)
})

##' Wrapper to the C++ shotgun_data object, which stores the
##' (sparse) design matrix and labels in a struct.
setClass("BuckshotData", contains="BuckshotObject",
         representation=representation(
           ptr="ptrOrNULL",
           dim="integer",
           dimnames="list",
           nnz="integer",
           rm.cols="integer"),
         prototype=prototype(
           ptr=NULL,
           dim=integer(),
           dimnames=list(NULL, NULL),
           nnz=0L,
           rm.cols=integer()))

##' A trained Buckshot model
##' 
##' Defaults taken from default parameters defined in shotgun's
##' command line tool (shotgun/mm_lasso.cpp)
setClass("BuckshotModel", contains="BuckshotObject",
         representation=representation(
           type="character",
           convergence.threshold="numeric",
           path.length="integer",
           max.iter="integer",
           lambda="numeric",
           data='BuckshotData',
           coefs='numeric'),
         prototype=prototype(
           type='lasso',
           convergence.threshold=1e-5,
           path.length=1L,
           max.iter=100L,
           lambda=1,
           data=new('BuckshotData'),
           coefs=numeric()))

###############################################################################
## Methods
## ----------------------------------------------------------------------------

##' Build a data object used for buckshot models
##' 
##' The shotgun library stores the design matrix and labels in a shotgun_data
##' object. This function builds that object from the inputs given.
##' 
##' Currently only matrices are supported.
##' TODO: Support the formula interface
setGeneric("BuckshotData", function(x, ...) standardGeneric("BuckshotData"))
setGeneric("designMatrix", function(x, ...) standardGeneric("designMatrix"))
setGeneric("labels", function(object, ...) standardGeneric("labels"))

##' Builds a lasso or logistic regression model.
##' 
##' @usage
##' lasso.model <- buckshot(X1, y1,, 'lasso', lambda=0.5)
##' logistic.model <- buckshot(X2, y2, 'logistic', lambda=0.5)
setGeneric("buckshot", function(x, ...) standardGeneric("buckshot"))

##' Access the data object from a buckshot model
# setGeneric("data", function(x, ...) standardGeneric("data"))
