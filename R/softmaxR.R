#' @title Softmax 多分类算法（R版本：4.4.1）
#' @description 使用 R 语言实现的 Softmax 多分类算法
#' @param X 训练数据的特征矩阵（不包括标签列）
#' @param y 训练数据的标签向量
#' @param learning_rate 学习率
#' @param epochs 迭代次数
#' @return 一个列表，包含训练好的权重矩阵和每次迭代的损失值
#' @useDynLib SA24231055
#' @examples
#' \dontrun{
#' data(filmtrain)
#' X <- as.matrix(filmtrain[, 1:(ncol(filmtrain)-1)])
#' y <- filmtrain[, ncol(filmtrain)]
#' model <- softmaxR(X, y, learning_rate = 0.01, epochs = 100)
#' }
#' @export
softmaxR <- function(x_train, y_train, x_test, y_test, learning_rate=0.01, num_iters=1000) {
  
  # Softmax函数，带有数值稳定性处理
  softmax <- function(x) {
    # 为了数值稳定性，减去每行的最大值
    x_max <- apply(x, 1, max)
    x_stable <- sweep(x, 1, x_max, "-")
    exp_x <- exp(x_stable)
    sum_exp_x <- rowSums(exp_x)
    probs <- sweep(exp_x, 1, sum_exp_x, "/")
    return(probs)
  }
  
  # 交叉熵损失函数
  cross_entropy_loss <- function(y_pred, y_true) {
    num_samples <- nrow(y_pred)
    epsilon <- 1e-15  # 防止概率为零
    probs <- softmax(y_pred)
    probs <- pmax(probs, epsilon)
    log_probs <- -log(probs[cbind(1:num_samples, y_true + 1)])
    loss <- sum(log_probs) / num_samples
    return(loss)
  }
  
  # 准确率计算
  accuracy <- function(y_pred, y_true) {
    return(mean(y_pred == y_true))
  }
  
  # 预测函数
  predict_classes <- function(x, w) {
    y_pred <- x %*% w
    probs <- softmax(y_pred)
    return(max.col(probs) - 1)  # 减1以匹配从0开始的标签
  }
  
  # 梯度计算
  calc_gradient <- function(x, y_true, y_pred) {
    num_samples <- nrow(x)
    probs <- softmax(y_pred)
    # 对真实类别的概率减1
    probs[cbind(1:num_samples, y_true + 1)] <- probs[cbind(1:num_samples, y_true + 1)] - 1
    probs <- probs / num_samples
    grad <- t(x) %*% probs
    return(grad)
  }
  
  # 梯度下降优化
  set.seed(42)  # 为了结果可重复
  num_features <- ncol(x_train)
  num_classes <- length(unique(y_train))
  w <- matrix(rnorm(num_features * num_classes), nrow=num_features, ncol=num_classes)
  
  # 初始化记录变量
  train_accuracy_list <- c()
  test_accuracy_list <- c()
  loss_list <- c()
  iter_list <- c()
  
  for (i in 0:(num_iters - 1)) {
    y_pred <- x_train %*% w
    grad <- calc_gradient(x_train, y_train, y_pred)
    w <- w - learning_rate * grad
    
    if (i %% 100 == 0) {
      current_loss <- cross_entropy_loss(y_pred, y_train)
      y_train_pred <- predict_classes(x_train, w)
      train_acc <- accuracy(y_train_pred, y_train)
      y_test_pred <- predict_classes(x_test, w)
      test_acc <- accuracy(y_test_pred, y_test)
      
      cat(sprintf("迭代 %d: 训练损失 = %.4f, 训练准确率 = %.4f, 测试准确率 = %.4f\n",
                  i, current_loss, train_acc, test_acc))
      
      loss_list <- c(loss_list, current_loss)
      train_accuracy_list <- c(train_accuracy_list, train_acc)
      test_accuracy_list <- c(test_accuracy_list, test_acc)
      iter_list <- c(iter_list, i)
    }
  }
  
  # 创建损失历史的数据框
  loss_df <- data.frame(
    Epoch = iter_list,
    Loss = loss_list
  )
  
  # 创建准确率历史的数据框
  accuracy_df <- data.frame(
    Epoch = rep(iter_list, 2),
    Accuracy = c(train_accuracy_list, test_accuracy_list),
    Dataset = rep(c("训练准确率", "测试准确率"), each=length(iter_list))
  )
  
  # 返回模型参数、损失历史和准确率历史
  return(list(weights = w, loss_df = loss_df, accuracy_df = accuracy_df))
}
