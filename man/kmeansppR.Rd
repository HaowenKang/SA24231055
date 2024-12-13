% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeansppR.R
\name{kmeansppR}
\alias{kmeansppR}
\title{K-Means++ Clustering Algorithm (R Version)}
\usage{
kmeansppR(X, k, max_iter = 100)
}
\arguments{
\item{X}{Data matrix, where each row represents a sample}

\item{k}{Number of clusters}

\item{max_iter}{Maximum number of iterations, default is 100}
}
\value{
A list containing the clustering results and cluster centers
}
\description{
Implementation of the K-Means++ clustering algorithm in R.
}
\examples{
\dontrun{
data(ori_data_x)
result <- kmeansppR(ori_data_x, k = 3)
}
}
