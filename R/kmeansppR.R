#' @title K-Means++ Clustering Algorithm (R Version)
#' @description Implementation of the K-Means++ clustering algorithm in R.
#' @param X Data matrix, where each row represents a sample
#' @param k Number of clusters
#' @param max_iter Maximum number of iterations, default is 100
#' @return A list containing the clustering results and cluster centers
#' @examples
#' \dontrun{
#' data(ori_data_x)
#' result <- kmeansppR(ori_data_x, k = 3)
#' }
#' @export
kmeansppR <- function(X, k, max_iter = 100) {
  if (!is.matrix(X)) {
    stop("Input data must be a matrix.")
  }
  n_samples <- nrow(X)
  n_features <- ncol(X)
  
  # Initialize cluster centers using K-Means++ method
  set.seed(123)
  centers <- matrix(0, nrow = k, ncol = n_features)
  centers[1, ] <- X[sample(1:n_samples, 1), ]
  
  for (i in 2:k) {
    distances <- sapply(1:n_samples, function(j) {
      min(rowSums((t(centers[1:(i - 1), ]) - X[j, ])^2))
    })
    probs <- distances / sum(distances)
    centers[i, ] <- X[sample(1:n_samples, 1, prob = probs), ]
  }
  
  # Subsequent steps are the same as K-Means
  cluster_assignments <- rep(0, n_samples)
  for (iter in 1:max_iter) {
    distances <- as.matrix(dist(rbind(X, centers)))[1:n_samples, (n_samples + 1):(n_samples + k)]
    new_assignments <- apply(distances, 1, which.min)
    
    if (all(cluster_assignments == new_assignments)) {
      break
    }
    cluster_assignments <- new_assignments
    
    for (i in 1:k) {
      centers[i, ] <- colMeans(X[cluster_assignments == i, , drop = FALSE])
    }
  }
  
  return(list(clusters = cluster_assignments, centers = centers))
}


