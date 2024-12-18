# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title 信息增益计算（Rcpp 版本）
#' @description 使用 Rcpp 实现的信息增益计算函数
#' @param data 数据框，包含特征和标签
#' @param feature 要计算的信息增益的特征名称
#' @return 信息增益值
#' @examples
#' \dontrun{
#' data(filmtrain)
#' gain <- informationGainCpp(filmtrain, "V1")
#' }
#' @export
informationGainCpp <- function(data, feature) {
    .Call('_SA24231055_informationGainCpp', PACKAGE = 'SA24231055', data, feature)
}

#' @title K-Means 聚类算法（Rcpp 版本）
#' @description 使用 Rcpp 实现的 K-Means 聚类算法
#' @param X 数据矩阵，每行是一个样本
#' @param k 聚类的类别数
#' @param max_iter 最大迭代次数，默认为 100
#' @return 一个列表，包含聚类结果和聚类中心
#' @examples
#' \dontrun{
#' data(ori_data_x)
#' result <- kmeansCpp(as.matrix(ori_data_x), k = 3)
#' }
#' @export
kmeansCpp <- function(X, k, max_iter = 100L) {
    .Call('_SA24231055_kmeansCpp', PACKAGE = 'SA24231055', X, k, max_iter)
}

#' @title K-Means++ 聚类算法（Rcpp 版本）
#' @description 使用 Rcpp 实现的 K-Means++ 聚类算法
#' @param X 数据矩阵，每行是一个样本
#' @param k 聚类的类别数
#' @param max_iter 最大迭代次数，默认为 100
#' @return 一个列表，包含聚类结果和聚类中心
#' @examples
#' \dontrun{
#' data(ori_data_x)
#' result <- kmeansppCpp(as.matrix(ori_data_x), k = 3)
#' }
#' @export
kmeansppCpp <- function(X, k, max_iter = 100L) {
    .Call('_SA24231055_kmeansppCpp', PACKAGE = 'SA24231055', X, k, max_iter)
}

#' @title 朴素贝叶斯分类算法（Rcpp 版本）
#' @description 使用 Rcpp 实现的朴素贝叶斯分类算法
#' @param data 训练数据集，包括特征和标签
#' @return 训练好的朴素贝叶斯模型
#' @examples
#' \dontrun{
#' data(filmtrain)
#' model <- naiveBayesCpp(as.matrix(filmtrain))
#' }
#' @export
naiveBayesCpp <- function(data) {
    .Call('_SA24231055_naiveBayesCpp', PACKAGE = 'SA24231055', data)
}

#' @title Softmax 多分类算法（Rcpp 版本）
#' @description 使用 Rcpp 实现的 Softmax 多分类算法，并计算测试集的准确率
#' @param X 训练数据的特征矩阵（不包括标签列）
#' @param y 训练数据的标签向量
#' @param X_test 测试数据的特征矩阵（不包括标签列）
#' @param y_test 测试数据的标签向量
#' @param learning_rate 学习率
#' @param epochs 迭代次数
#' @return 一个列表，包含训练好的权重矩阵、每次迭代的损失值和测试集的准确率
#' @examples
#' \dontrun{
#' data(filmtrain)
#' data(filmtest)
#' X_train <- as.matrix(filmtrain[, 1:(ncol(filmtrain)-1)])
#' y_train <- filmtrain[, ncol(filmtrain)]
#' X_test <- as.matrix(filmtest[, 1:(ncol(filmtest)-1)])
#' y_test <- filmtest[, ncol(filmtest)]
#' model <- softmaxCpp(X_train, y_train, X_test, y_test, learning_rate = 0.01, epochs = 100)
#' }
#' @export
softmaxCpp <- function(X, y, X_test, y_test, learning_rate = 0.01, epochs = 100L) {
    .Call('_SA24231055_softmaxCpp', PACKAGE = 'SA24231055', X, y, X_test, y_test, learning_rate, epochs)
}

#' @title 谱聚类算法（Rcpp 版本）
#' @description 使用 Rcpp 实现的谱聚类算法，将数据映射到低维空间后进行聚类。
#' @param X 数据矩阵，每行是一个样本。
#' @param k 聚类的类别数。
#' @return 一个整数向量，表示每个样本的聚类标签。
#' @export
spectralClusteringRcpp <- function(X, k) {
    .Call('_SA24231055_spectralClusteringRcpp', PACKAGE = 'SA24231055', X, k)
}

