setClassUnion("ptrOrNULL", c("externalptr", "NULL"))
##' Base object for Buckshot classes
setClass("BuckshotObject", contains="VIRTUAL",
         representation=representation(cache="environment"))
setMethod("initialize", "BuckshotObject",
function(.Object, ..., cache=new.env()) {
  cache$threads <- 1L
  callNextMethod(.Object, cache=cache, ...)
})

##' Wrapper to the C++ shotgun_data object, which stores the
##' (sparse) design matrix and labels in a struct.
setClass("BuckshotData", contains="BuckshotObject",
         representation=representation(
           ptr="ptrOrNULL"),
         prototype=prototype(ptr=NULL))

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
           data='BuckshotData'),
         prototype=prototype(
           type='lasso',
           convergence.threshold=1e-5,
           path.length=1L,
           max.iter=100L,
           lambda=1,
           data=new('BuckshotData')
           ))

## Methods
setGeneric("BuckshotData", function(x, ...) standardGeneric("BuckshotData"))
setGeneric("buckshot", function(x, ...) standardGeneric("buckshot"))

setGeneric("bias", function(object, ...) standardGeneric("bias"))

## setGeneric("train", function(x, ...) standardGeneric("train"))
## setGeneric("trained", function(x, ...) standardGeneric("trained"))
