#' @title 绘制聚类中心热图
#' @description 展示聚类中心的特征值
#' @param centers 聚类中心矩阵
#' @return 无返回值，直接绘制图形
#' @examples
#' \dontrun{
#' plotClusterCenters(result$centers)
#' }
#' @export
plotClusterCenters <- function(centers) {
  library(reshape2)
  library(ggplot2)
  
  df <- as.data.frame(centers)
  df$Cluster <- factor(1:nrow(df))
  df_melt <- melt(df, id.vars = "Cluster")
  
  ggplot(df_melt, aes(x = variable, y = value, fill = Cluster)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "聚类中心特征值", x = "特征", y = "值") +
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}
