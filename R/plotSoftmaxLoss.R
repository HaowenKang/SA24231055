#' @title 绘制 Softmax 分类器的准确率曲线
#' @description 绘制模型在训练集和测试集上的准确率随迭代次数的变化
#' @param model 由 softmaxR 或 softmaxCpp 返回的模型列表
#' @param X_train 训练集特征矩阵
#' @param y_train 训练集标签
#' @param X_test 测试集特征矩阵
#' @param y_test 测试集标签
#' @return 无返回值，直接绘制曲线
#' @examples
#' \dontrun{
#' data(filmtrain)
#' data(filmtest)
#' X_train <- as.matrix(filmtrain[, 1:(ncol(filmtrain)-1)])
#' y_train <- filmtrain[, ncol(filmtrain)]
#' X_test <- as.matrix(filmtest[, 1:(ncol(filmtest)-1)])
#' y_test <- filmtest[, ncol(filmtest)]
#' model <- softmaxR(X_train, y_train, learning_rate = 0.01, epochs = 100)
#' plotSoftmaxAccuracy(model, X_train, y_train, X_test, y_test)
#' }
#' @export
plotSoftmaxLoss <- function(loss_df) {
  ggplot(loss_df, aes(x = Epoch, y = Loss)) +
    geom_line(color = "#2c7fb8", size = 1) +          # 绘制线条，设置颜色和线宽
    geom_point(color = "#2c7fb8", size = 0.5) +       # 添加数据点，设置颜色和大小
    labs(
      title = "Softmax 损失曲线",
      x = "迭代次数",
      y = "损失值"
    ) +
    theme_minimal(base_size = 15) +                   # 使用简洁主题并设置基准字体大小
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),  # 设置标题样式
      axis.title = element_text(face = "bold", color = "black"),              # 设置轴标题样式
      panel.grid.major = element_line(color = "#e0e0e0"),                       # 设置主网格线颜色
      panel.grid.minor = element_blank()                                        # 移除次网格线
    )
}
