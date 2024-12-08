#' @title 绘制谱聚类结果
#' @description 对谱聚类结果进行可视化展示
#' @param X 数据矩阵，每行是一个样本
#' @param clusters 聚类标签
#' @return 无返回值，直接绘制图形
#' @examples
#' \dontrun{
#' plotSpectralClusteringResult(ori_data_x, clusters)
#' }
#' @export
plotSpectralClusteringResult <- function(X, clusters) {
  # 加载必要的库
  # install.packages("ggplot2")
  # install.packages("RColorBrewer")
  library(ggplot2)
  library(RColorBrewer)
  
  # 1. 使用 PCA 降维到 2 维以便可视化
  pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
  data_pca <- pca_result$x[, 1:2]  # 选择前两个主成分
  
  # 2. 创建数据框用于绘图
  df <- data.frame(
    PC1 = data_pca[, 1],
    PC2 = data_pca[, 2],
    Cluster = as.factor(clusters)
  )
  
  # 3. 绘制散点图，颜色表示不同的聚类标签
  ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 2, shape = 17) +  # 使用星形点
    scale_color_brewer(palette = "Set1") +  # 使用 RColorBrewer 的 "Set1" 颜色调色板
    labs(
      title = "谱聚类结果的 PCA 可视化",
      x = "主成分 1",
      y = "主成分 2",
      color = "聚类标签"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold")
    )
}
