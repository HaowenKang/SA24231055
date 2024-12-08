#' @title 绘制决策树分类器的准确率
#' @description 使用 ggplot2 绘制更加美观的准确率对比图
#' @param tree 决策树模型
#' @param data_train 训练集数据
#' @param data_test 测试集数据
#' @return 无返回值，直接绘制图形
#' @examples
#' \dontrun{
#' data(filmtrain)
#' data(filmtest)
#' tree <- decisionTreeR(filmtrain)
#' plotDecisionTreeAccuracy(tree, filmtrain, filmtest)
#' }
#' @export
plotDecisionTreeAccuracy <- function(tree, data_train, data_test) {
  # 定义预测函数
  predict_tree <- function(tree, sample) {
    if (tree$type == "leaf") {
      return(tree$class)
    } else {
      feature_value <- sample[[tree$feature]]
      branch <- tree$branches[[as.character(feature_value)]]
      if (is.null(branch)) {
        return(tree$class)  # 如果没有分支，返回默认类别
      } else {
        return(predict_tree(branch, sample))
      }
    }
  }
  
  # 计算训练集准确率
  preds_train <- apply(data_train, 1, function(row) predict_tree(tree, as.list(row)))
  accuracy_train <- mean(preds_train == data_train[, ncol(data_train)])
  
  # 计算测试集准确率
  preds_test <- apply(data_test, 1, function(row) predict_tree(tree, as.list(row)))
  accuracy_test <- mean(preds_test == data_test[, ncol(data_test)])
  
  # 绘制准确率对比图
  accuracy_data <- data.frame(
    Dataset = c("训练集", "测试集"),
    Accuracy = c(accuracy_train, accuracy_test)
  )
  
  ggplot(accuracy_data, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = c("训练集" = "#0073C2FF", "测试集" = "#EFC000FF")) +
    labs(y = "准确率", title = "决策树分类器准确率对比") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)  # 居中并加粗标题
    ) +
    geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = 5, hjust = 0.5, size = 5, color = "black")  # 在条形中间显示准确率
}
