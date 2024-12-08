#' @title 朴素贝叶斯分类算法（R版本）
#' @description 使用 R 语言实现的朴素贝叶斯分类算法
#' @param data 训练数据集，包括特征和标签
#' @return 训练好的朴素贝叶斯模型
#' @examples
#' \dontrun{
#' data(filmtrain)
#' model <- naiveBayesR(filmtrain)
#' }
#' @export
naiveBayesR <- function(data) {
  X <- data[, -ncol(data)]
  y <- data[, ncol(data)]
  labels <- unique(y)
  num_features <- ncol(X)
  
  # 计算先验概率
  prior <- prop.table(table(y))
  
  # 计算条件概率
  likelihood <- list()
  for (label in labels) {
    subset <- X[y == label, ]
    feature_likelihood <- list()
    for (j in 1:num_features) {
      feature_likelihood[[j]] <- prop.table(table(subset[, j]))
    }
    likelihood[[as.character(label)]] <- feature_likelihood
  }
  
  model <- list(prior = prior, likelihood = likelihood, labels = labels)
  return(model)
}
