\name{SA24231055-package}
\alias{SA24231055-package}
\docType{package}
\title{A Collection of Machine Learning Algorithms and Visualization Tools}
\description{
  This package provides implementations of various machine learning algorithms, including decision trees, k-means clustering, Naive Bayes, softmax, and spectral clustering, along with visualization tools to plot the results of these algorithms. It is designed to be used for both educational purposes and practical applications in data analysis.
  
  The package includes:
  
  \enumerate{
    \item \strong{Classification Algorithms}:
      \itemize{
        \item \code{softmaxR} and \code{softmaxCpp} for Softmax multi-class classification.
        \item \code{decisionTreeR} for Decision Tree classification with information gain and depth limitation.
        \item \code{naiveBayesR} and \code{naiveBayesCpp} for Naive Bayes classification.
      }
    \item \strong{Clustering Algorithms}:
      \itemize{
        \item \code{kmeansR} and \code{kmeansCpp} for K-Means clustering.
        \item \code{kmeansppR} and \code{kmeansppCpp} for K-Means++ clustering with improved initialization.
        \item \code{spectralClusteringR} and \code{spectralClusteringRcpp} for Spectral Clustering.
      }
    \item \strong{Performance Benchmarking}:
      Utilizes the \pkg{microbenchmark} package to compare the performance of R and Rcpp implementations of the algorithms.
    \item \strong{Visualization Tools}:
      Provides various plotting functions such as \code{plotSoftmaxLoss}, \code{plotSoftmaxAccuracy}, \code{plotDecisionTreeAccuracy}, \code{plotKMeansResult}, etc., to visualize training processes, performance metrics, and clustering results.
    \item \strong{Homework Aggregation}:
      Aggregates all assignments from the \emph{Statistics Computing} course, available in \code{inst/doc/homework.html}, covering theoretical derivations, code implementations, and performance analyses.
  }
}
\author{
Haowen Kang

Maintainer:Haowen Kang <2205229324@qq.com>
}
\keyword{package}
