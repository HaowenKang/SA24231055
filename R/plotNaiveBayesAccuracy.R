#' @title 绘制朴素贝叶斯分类器的准确率
#' @description 使用 ggplot2 绘制更美观的准确率对比图
#' @param model 朴素贝叶斯模型
#' @param data_train 训练集数据
#' @param data_test 测试集数据
#' @return 无返回值，直接绘制图形
#' @examples
#' \dontrun{
#' data(filmtrain)
#' data(filmtest)
#' model <- naiveBayesR(filmtrain)
#' plotNaiveBayesAccuracy(model, filmtrain, filmtest)
#' }
#' @export
plotNaiveBayesAccuracy <- function(model, data_train, data_test) {
  library(ggplot2)
  
  # 定义预测函数
  predict_nb <- function(model, X) {
    n_samples <- nrow(X)
    preds <- numeric(n_samples)
    for (i in 1:n_samples) {
      probs <- numeric(length(model$labels))
      names(probs) <- model$labels
      for (label in model$labels) {
        prob <- log(model$prior[as.character(label)])
        for (j in 1:ncol(X)) {
          value <- X[i, j]
          likelihood <- model$likelihood[[as.character(label)]][[j]][as.character(value)]
          if (is.null(likelihood)) {
            likelihood <- 1e-6  # 拉普拉斯平滑
          }
          prob <- prob + log(likelihood)
        }
        probs[as.character(label)] <- prob
      }
      preds[i] <- as.numeric(names(which.max(probs)))
    }
    return(preds)
  }
  
  # 计算训练集准确率
  preds_train <- predict_nb(model, data_train[, -ncol(data_train)])
  accuracy_train <- mean(preds_train == data_train[, ncol(data_train)])
  
  # 计算测试集准确率
  preds_test <- predict_nb(model, data_test[, -ncol(data_test)])
  accuracy_test <- mean(preds_test == data_test[, ncol(data_test)])
  
  # 创建数据框用于 ggplot 绘图
  accuracy_data <- data.frame(
    Dataset = c("训练集", "测试集"),
    Accuracy = c(accuracy_train, accuracy_test)
  )
  
  # 绘制准确率对比图
  ggplot(accuracy_data, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Accuracy)), 
              vjust = 5, color = "black", size = 6, fontface = "bold") +  # 在条形图上添加数值
    scale_fill_manual(values = c("训练集" = "#1f77b4", "测试集" = "#ff7f0e")) +  # 设置颜色
    labs(y = "准确率", title = "朴素贝叶斯分类器准确率对比") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # 设置标题居中并加粗
      axis.title.x = element_blank(),  # 不显示 x 轴标题
      axis.text.x = element_text(size = 12),  # x 轴文字大小
      axis.text.y = element_text(size = 12),  # y 轴文字大小
      axis.ticks = element_line(size = 0.5)  # 设置坐标轴刻度线
    )
}
