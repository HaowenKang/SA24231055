## ----eval=TRUE----------------------------------------------------------------
# 创建一个自定义的安全加载函数

safe_library <- function(pkg) {
  suppressPackageStartupMessages(
    suppressWarnings(
      library(pkg, character.only = TRUE)
    )
  )
}

# 加载需要的包
safe_library("Rcpp")
safe_library("microbenchmark")
safe_library("data.tree")
safe_library("DiagrammeR")
safe_library("matrixStats")
safe_library("knitr")
library(devtools)
load_all()

## ----eval=TRUE----------------------------------------------------------------
library("SA24231055")  

# 加载数据集
data(films_train)
data(films_test)

## ----eval=TRUE----------------------------------------------------------------
# 加载包
library(SA24231055)
head(films_train, 5)

## ----eval=TRUE----------------------------------------------------------------
head(films_test, 5)

## ----eval=TRUE----------------------------------------------------------------
# 加载数据
data(ori_data)

## ----eval=TRUE----------------------------------------------------------------
head(ori_data, 5)

## ----eval=TRUE----------------------------------------------------------------
library("ggplot2")
# 加载训练和测试数据
data(films_train)  # 确保已加载films_train数据集
data(films_test)   # 确保已加载films_test数据集


# 提取特征矩阵和标签向量
X_train <- as.matrix(films_train[, 1:(ncol(films_train) - 1)])
y_train <- films_train[, ncol(films_train)]

X_test <- as.matrix(films_test[, 1:(ncol(films_test) - 1)])
y_test <- films_test[, ncol(films_test)]

# 标签映射
unique_labels <- sort(unique(c(y_train, y_test)))
label_map <- setNames(0:(length(unique_labels) - 1), unique_labels)
y_train_mapped <- label_map[as.character(y_train)]
y_test_mapped <- label_map[as.character(y_test)]


# 调用softmaxR函数进行训练
model_softmax_R <- softmaxR(X_train, y_train_mapped, X_test, y_test_mapped, learning_rate=0.01, num_iters=1000)

# 获取损失历史数据框
loss_df <- model_softmax_R$loss_df

# 使用绘图函数绘制损失曲线
plotSoftmaxLoss(loss_df)

# 获取准确率历史数据框
accuracy_df <- model_softmax_R$accuracy_df

# 提取测试准确率的数据
test_accuracy <- accuracy_df$Accuracy[accuracy_df$Dataset == "测试准确率"]

# 获取最终的测试准确率
final_test_acc <- tail(test_accuracy, 1)

# 打印最终的测试准确率
cat(sprintf("最终的测试准确率: %.4f\n", final_test_acc))

## ----eval=TRUE----------------------------------------------------------------
# 调用 Softmax Rcpp 版本函数进行训练
model_softmax_Cpp <- softmaxCpp(X_train, y_train, X_test, y_test, learning_rate = 0.01, epochs = 100)

# 创建一个数据框，包含迭代次数和对应的损失值
loss_df <- data.frame(
  Epoch = 1:length(model_softmax_Cpp$loss_history),
  Loss = model_softmax_Cpp$loss_history
)

# 使用 ggplot2 绘制损失曲线
ggplot(loss_df, aes(x = Epoch, y = Loss)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +          # 绘制线条，设置颜色和线宽
  geom_point(color = "#2c7fb8", size = 0.5) +       # 添加数据点，设置颜色和大小
  geom_smooth(method = "loess", se = FALSE, color = "#fc8d59", linetype = "dashed") +  # 添加平滑曲线
  labs(
    title = "Softmax Rcpp 版本 - 损失曲线",
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

print(paste("Softmax Rcpp版本在测试集上的准确率为", model_softmax_Cpp$test_accuracy))

## ----eval=TRUE----------------------------------------------------------------
# 设置测试参数
learning_rate <- 0.01
epochs <- 1000

# 定义测试函数
benchmark_softmax <- microbenchmark(
  Softmax_R = {
    model_softmax_R <- softmaxR(X_train, y_train, X_test, y_test, learning_rate, epochs)
  },
  Softmax_Rcpp = {
    model_softmax_Cpp <- softmaxCpp(X_train, y_train, X_test, y_test, learning_rate, epochs)
  },
  times = 10
)

# 查看执行时间结果
print(benchmark_softmax)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_softmax)

# 计算平均执行时间（毫秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_ms <- mean_times$time / 1e6  # 纳秒转换为毫秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_ms")], caption = "Softmax 多分类算法执行时间比较")


## ----eval=TRUE----------------------------------------------------------------
# 获取softmaxR的准确率历史数据框
accuracy_df <- model_softmax_R$accuracy_df

# 使用绘图函数绘制准确率曲线
plotSoftmaxAccuracy(accuracy_df)

## ----eval=TRUE----------------------------------------------------------------
# 加载数据
data(films_train)
data(films_test)

# 使用决策树算法训练模型
tree <- decisionTreeR(films_train, max_depth = 5)

# 定义预测函数
predict_tree <- function(tree, sample) {
  if (tree$type == "leaf") {
    return(tree$class)
  } else {
    feature_value <- sample[[tree$feature]]
    branch <- tree$branches[[as.character(feature_value)]]
    if (is.null(branch)) {
      # 若没有对应的分支，返回当前节点中样本最多的类别
      return(tree$class)
    } else {
      return(predict_tree(branch, sample))
    }
  }
}

# 在测试集上进行预测
predictions <- apply(films_test, 1, function(row) predict_tree(tree, as.list(row)))

# 计算准确率
accuracy <- mean(predictions == films_test[, ncol(films_test)])
print(paste("R语言实现的决策树算法在测试集上的准确率：", round(accuracy * 100, 2), "%"))

## ----eval=TRUE----------------------------------------------------------------
# 计算某个特征的信息增益（例如第二个特征）
gain <- informationGainCpp(films_train, "V2")
print(paste("特征 V2 的信息增益：", gain))

## ----eval=TRUE----------------------------------------------------------------
# 加载 microbenchmark 包
library(microbenchmark)

# 定义测试函数
benchmark_info_gain <- microbenchmark(
  InfoGain_R = {
    gain_R <- information_gain(films_train, "V2")
  },
  InfoGain_Rcpp = {
    gain_Cpp <- informationGainCpp(films_train, "V2")
  },
  times = 100
)

# 查看执行时间结果
print(benchmark_info_gain)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_info_gain)

# 计算平均执行时间（微秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_us <- mean_times$time / 1e3  # 纳秒转换为微秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_us")], caption = "信息增益计算执行时间比较")

## ----eval=TRUE----------------------------------------------------------------
# 绘制决策树的准确率对比图
plotDecisionTreeAccuracy(tree, films_train, films_test)

## ----eval=TRUE----------------------------------------------------------------
# 绘制决策树的结构图
plotDecisionTreeStructure(tree)

## ----eval=TRUE----------------------------------------------------------------
# 加载数据
data(films_train)
data(films_test)

# 训练朴素贝叶斯模型
model_nb_R <- naiveBayesR(films_train)

## ----eval=TRUE----------------------------------------------------------------
# 训练朴素贝叶斯模型（Rcpp 版本）
model_nb_Cpp <- naiveBayesCpp(as.matrix(films_train))

## ----eval=TRUE----------------------------------------------------------------
# 加载 microbenchmark 包
library(microbenchmark)

# 定义测试函数
benchmark_nb <- microbenchmark(
  NaiveBayes_R = {
    model_nb_R <- naiveBayesR(films_train)
  },
  NaiveBayes_Rcpp = {
    model_nb_Cpp <- naiveBayesCpp(as.matrix(films_train))
  },
  times = 100
)

# 查看执行时间结果
print(benchmark_nb)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_nb)

# 计算平均执行时间（微秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_us <- mean_times$time / 1e3  # 纳秒转换为微秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_us")], caption = "朴素贝叶斯训练执行时间比较")

## ----eval=TRUE----------------------------------------------------------------
# 绘制朴素贝叶斯分类器的准确率对比图
plotNaiveBayesAccuracy(model_nb_R, films_train, films_test)

## ----eval=TRUE----------------------------------------------------------------
# 加载数据
data(ori_data)

# 检查数据类型
class(ori_data)  # 查看数据类型

# 将数据转换为矩阵
ori_data_matrix <- as.matrix(ori_data)

# 使用 K-Means 算法进行聚类
result_kmeans_R <- kmeansR(ori_data_matrix, k = 3)

# 查看聚类结果
table(result_kmeans_R$clusters)

# 绘制聚类中心热图
plotClusterCenters(result_kmeans_R$centers)

## ----eval=TRUE----------------------------------------------------------------
# 使用 K-Means Rcpp 版本进行聚类
result_kmeans_Cpp <- kmeansCpp(as.matrix(ori_data), k = 3)

## ----eval=TRUE----------------------------------------------------------------
# 加载 microbenchmark 包
library(microbenchmark)

# 定义测试函数
benchmark_kmeans <- microbenchmark(
  KMeans_R = {
    result_kmeans_R <- kmeansR(as.matrix(ori_data), k = 3)
  },
  KMeans_Rcpp = {
    result_kmeans_Cpp <- kmeansCpp(as.matrix(ori_data), k = 3)
  },
  times = 10
)

# 查看执行时间结果
print(benchmark_kmeans)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_kmeans)

# 计算平均执行时间（毫秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_ms <- mean_times$time / 1e6  # 纳秒转换为毫秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_ms")], caption = "K-Means 算法执行时间比较")

## ----eval=TRUE----------------------------------------------------------------
# 绘制 K-Means 聚类结果
plotKMeansResult(as.matrix(ori_data), result_kmeans_R$clusters)

## ----eval=TRUE----------------------------------------------------------------
# 使用 K-Means++ 算法进行聚类
result_kmeanspp_R <- kmeansppR(as.matrix(ori_data), k = 5)

# 查看聚类结果
table(result_kmeanspp_R$clusters)

# 绘制聚类中心热图
plotClusterCenters(result_kmeanspp_R$centers)

## ----eval=TRUE----------------------------------------------------------------
# 使用 K-Means++ Rcpp 版本进行聚类
result_kmeanspp_Cpp <- kmeansppCpp(as.matrix(ori_data), k = 5)

## ----eval=TRUE----------------------------------------------------------------
# 定义测试函数
benchmark_kmeanspp <- microbenchmark(
  KMeanspp_R = {
    result_kmeanspp_R <- kmeansppR(as.matrix(ori_data), k = 5)
  },
  KMeanspp_Rcpp = {
    result_kmeanspp_Cpp <- kmeansppCpp(as.matrix(ori_data), k = 5)
  },
  times = 10
)

# 查看执行时间结果
print(benchmark_kmeanspp)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_kmeanspp)

# 计算平均执行时间（毫秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_ms <- mean_times$time / 1e6  # 纳秒转换为毫秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_ms")], caption = "K-Means++ 算法执行时间比较")

## ----eval=TRUE----------------------------------------------------------------
# 绘制 K-Means++ 聚类结果
plotKMeansResult(as.matrix(ori_data), result_kmeanspp_R$clusters)

## ----eval=TRUE----------------------------------------------------------------
# 使用谱聚类算法进行聚类
clusters_spectral_R <- spectralClusteringR(as.matrix(ori_data), k = 3)

# 查看聚类结果
table(clusters_spectral_R)

## ----eval=TRUE----------------------------------------------------------------
# 应用谱聚类算法
clusters_spectral_cpp <- spectralClusteringRcpp(as.matrix(ori_data), 3)

# 查看聚类结果
print(table(clusters_spectral_cpp))

## ----eval=TRUE----------------------------------------------------------------
library(microbenchmark)
# 进行性能比较
benchmark_result <- microbenchmark(
  R_version = spectralClusteringR(as.matrix(ori_data), 3),
  Rcpp_version = spectralClusteringRcpp(as.matrix(ori_data), 3),
  times = 3  # 可以根据需要调整重复次数
)

# 查看结果
print(benchmark_result)

# 将结果转换为数据框
benchmark_df <- as.data.frame(benchmark_result)

# 计算平均执行时间（纳秒转为秒）
mean_times <- aggregate(time ~ expr, data = benchmark_df, FUN = mean)
mean_times$time_s <- mean_times$time / 1e9  # 纳秒转换为秒

# 显示平均执行时间
knitr::kable(mean_times[, c("expr", "time_s")], caption = "谱聚类算法 R 版本与 Rcpp 版本的平均执行时间 (秒)")

# 可视化比较结果
ggplot(benchmark_df, aes(x = expr, y = time)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  ggtitle("谱聚类算法 R 版本与 Rcpp 版本执行时间比较") +
  ylab("时间 (纳秒)") +
  xlab("方法") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## ----eval=TRUE----------------------------------------------------------------
# 绘制谱聚类结果
plotSpectralClusteringResult(as.matrix(ori_data), clusters_spectral_R)

