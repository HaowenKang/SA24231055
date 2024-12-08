#' @title 绘制 K-Means 聚类结果
#' @description 使用 PCA 降维后对 K-Means 聚类结果进行可视化，并以红色叉形标记展示聚类中心
#' @param X 数据矩阵，每行是一个样本
#' @param clusters 聚类标签
#' @return 无返回值，直接绘制图形
#' @examples
#' \dontrun{
#' data(ori_data_x)
#' result <- kmeansR(ori_data_x, k = 3)
#' plotKMeansResult(ori_data_x, result$clusters)
#' }
#' @export
plotKMeansResult <- function(X, clusters) {
  # 加载所需的包
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("包 'ggplot2' 未安装。请先安装该包。")
  }
  library(ggplot2)
  
  # PCA 降维到 2 维
  pca_result <- prcomp(X, scale. = TRUE)
  
  # 创建包含 PCA 结果和聚类标签的数据框，修正变量命名
  df <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Cluster = as.factor(clusters)
  )
  
  # 计算每个聚类的中心点
  centers <- aggregate(cbind(PC1, PC2) ~ Cluster, data = df, FUN = mean)
  
  # 绘制散点图和聚类中心
  ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 2, alpha = 0.6) +  # 绘制数据点
    geom_point(data = centers, aes(x = PC1, y = PC2), 
               color = "red", shape = 4, size = 5, stroke = 2) +  # 添加聚类中心
    labs(title = "K-Means 聚类结果", x = "主成分 1", y = "主成分 2") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold")
    )
}
