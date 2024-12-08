#' @title 谱聚类算法（R 版本）
#' @description 使用 R 语言实现的谱聚类算法，完全按照提供的 Python 代码逻辑
#' @param X 数据矩阵，每行是一个样本
#' @param k 聚类的类别数
#' @return 一个向量，表示每个样本的聚类标签
#' @examples
#' \dontrun{
#' data(ori_data_x)
#' clusters <- spectralClusteringR(ori_data_x, k = 3)
#' }
#' @export
spectralClusteringR <- function(X, k) {
  # 1. 数据归一化处理，将每个特征缩放到 [0, 1]
  data_min <- colMins(X)
  data_max <- colMaxs(X)
  data_norm <- sweep(X, 2, data_min, "-")
  data_norm <- sweep(data_norm, 2, data_max - data_min, "/")
  
  # 2. 计算相似度矩阵，W[i,j] = exp(-||x_i -x_j||^2)
  n <- nrow(data_norm)
  similarity <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      similarity[i, j] <- exp(-sum((data_norm[i, ] - data_norm[j, ])^2))
    }
  }
  
  # 3. 构建拉普拉斯矩阵 L = D - W
  D <- diag(rowSums(similarity))
  L <- D - similarity
  
  # 4. 计算特征值和特征向量
  eig <- eigen(L, symmetric = TRUE)
  
  # 5. 取最小的 k 个特征值对应的特征向量
  # eigen函数返回的特征值按降序排列，因此取最后 k 个特征向量
  eigenvectors <- eig$vectors[, (ncol(eig$vectors) - k + 1):ncol(eig$vectors)]
  
  # 6. 定义聚类中心为前 k 个特征向量的 **行**（每个聚类中心是一个 k 维向量）
  centroids <- eigenvectors[1:k, ]  # centroids 是 k x k 矩阵，每行是一个聚类中心
  
  # 7. 对每个样本分配标签，基于与每个中心的距离
  labels <- numeric(n)
  
  for (i in 1:n) {
    distances <- numeric(k)
    for (j in 1:k) {
      # 计算欧氏距离的平方
      distances[j] <- sum((centroids[j, ] - eigenvectors[i, ])^2)
    }
    labels[i] <- which.min(distances)
  }
  
  return(labels)
}
