## For non-trivial roxygen2 usage, see https://github.com/ggobi/cranvas

##' A parallel lasso and sparse logistic regression solver for large-scale data
##' 
##' This package implements versions of lasso and sparse logistic
##' regression that are appropriate for learning over big data. It does so by
##' wrapping the (C++) shotgun library, developed in the SELECT Lab at CMU
##' (http://select.cs.cmu.edu/code).
##' 
##' More information on shotgun
##' 
##' See the ICML 2011  (http://arxiv.org/abs/1105.5379)) for performance
##' characteristics and comparisons to other state of the art solutions,
##' as you might expect shotgun compares favorably.
##' 
##' If you're working with big data and are not having any luck with other
##' tools, you might try this package. If you are generally trying to do
##' different types of machine learning task on big data, you might be
##' interested in lookig at their GraphLab project (http://graphlab.org/).
NULL


##' Arcene data for two-class classification tasks
##' 
##' ARCENE's task is to distinguish cancer versus normal patterns from
##' mass-spectrometric data. This is a two-class classification problem 
##' with continuous input variables. This dataset is one of 5 datasets of 
##' the NIPS 2003 feature selection challenge.
##' 
##' @name arcene
##' @docType data
##' @usage data(arcene, package="buckshot")
##' @format A.arcene: matrix 100 obs. of 9920 vars, y.arcene: labels
##' @keywords datasets
##' @source \url{http://archive.ics.uci.edu/ml/datasets/Arcene}
##' @examples
##' data(arcene, package="buckshot")
##' m <- logreg(A.arcene, y.arcene, lambda=0.2)
##' table(predict(m, A.arcne), y.arcene)
NULL
