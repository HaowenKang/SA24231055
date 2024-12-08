#' @title K-Means Clustering Algorithm (R Version)
#' @description Implementation of the K-Means clustering algorithm in R.
#' @param X Data matrix, where each row represents a sample
#' @param k Number of clusters
#' @param max_iter Maximum number of iterations, default is 100
#' @return A list containing the clustering results and cluster centers
#' @examples
#' \dontrun{
#' data(ori_data)
#' result <- kmeansR(ori_data, k = 3)
#' }
#' @export
kmeansR <- function(X, k, max_iter = 100) {
  if (!is.matrix(X)) {
    stop("Input data must be a matrix.")
  }
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Randomly initialize cluster centers
  set.seed(123)
  centers <- X[sample(1:n_samples, k), ]
  
  cluster_assignments <- rep(0, n_samples)
  for (iter in 1:max_iter) {
    # Calculate the distance from each sample to each center
    distances <- as.matrix(dist(rbind(X, centers)))[1:n_samples, (n_samples + 1):(n_samples + k)]
    # Assign each sample to the nearest center
    new_assignments <- apply(distances, 1, which.min)
    
    # If assignments do not change, stop iterating
    if (all(cluster_assignments == new_assignments)) {
      break
    }
    cluster_assignments <- new_assignments
    
    # Update cluster centers
    for (i in 1:k) {
      centers[i, ] <- colMeans(X[cluster_assignments == i, , drop = FALSE])
    }
  }
  
  return(list(clusters = cluster_assignments, centers = centers))
}
