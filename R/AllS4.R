##' Base object for Buckshot classes
setClass("BuckshotObject", contains="VIRTUAL",
         representation=representation(cache="environment"))
setMethod("initialize", "BuckshotObject",
function(.Object, ..., cache=new.env()) {
  cache$threads <- 1L
  callNextMethod(.Object, cache=cache, ...)
})


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
           lambda="numeric"),
         prototype=prototype(
           type='lasso',
           convergence.threshold=1e-5,
           path.length=1L,
           max.iter=100L,
           lambda=1))
