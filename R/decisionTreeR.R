#' @title Decision Tree Classification Algorithm (R Version)
#' @description Implementation of a decision tree classification algorithm in R. The tree depth does not exceed 5 and uses the information gain method for attribute splitting.
#' @param data Training dataset, including features and labels
#' @param depth Current tree depth, default is 1
#' @param max_depth Maximum tree depth, default is 5
#' @return Decision tree model
#' @examples
#' \dontrun{
#' data(filmtrain)
#' tree <- decisionTreeR(filmtrain)
#' }
#' @export
decisionTreeR <- function(data, depth = 1, max_depth = 5) {
  features <- colnames(data)[-ncol(data)]
  labels <- data[, ncol(data)]
  
  # If all labels are the same or maximum depth is reached, stop splitting
  if (length(unique(labels)) == 1 || depth > max_depth) {
    return(list(type = "leaf", class = names(sort(table(labels), decreasing = TRUE))[1]))
  }
  
  # Calculate information gain for each feature
  gains <- sapply(features, function(feature) {
    information_gain(data, feature)
  })
  
  # Select the feature with the highest information gain
  best_feature <- features[which.max(gains)]
  
  # Create subtrees
  tree <- list(type = "node", feature = best_feature, branches = list())
  feature_values <- unique(data[[best_feature]])
  
  for (value in feature_values) {
    subset <- data[data[[best_feature]] == value, ]
    tree$branches[[as.character(value)]] <- decisionTreeR(subset, depth + 1, max_depth)
  }
  
  return(tree)
}

# Helper function to calculate information gain
information_gain <- function(data, feature) {
  total_entropy <- entropy(data[, ncol(data)])
  feature_values <- unique(data[[feature]])
  feature_entropy <- 0
  
  for (value in feature_values) {
    subset <- data[data[[feature]] == value, ]
    weight <- nrow(subset) / nrow(data)
    feature_entropy <- feature_entropy + weight * entropy(subset[, ncol(subset)])
  }
  
  gain <- total_entropy - feature_entropy
  return(gain)
}

# Helper function to calculate entropy
entropy <- function(labels) {
  probs <- table(labels) / length(labels)
  return(-sum(probs * log2(probs)))
}
