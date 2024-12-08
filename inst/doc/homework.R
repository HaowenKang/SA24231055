## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
library(ggplot2)  # 用于绘图

## ----eval=TRUE----------------------------------------------------------------
# 创建 x 的取值范围，从 0 到 2π，共 1000 个点
x_values <- seq(0, 2 * pi, length.out = 1000)

# 计算对应的 y 值，即正弦函数值
y_values <- sin(x_values)

# 创建数据框保存 x 和 y 的值
data <- data.frame(x = x_values, y = y_values)

# 使用 ggplot2 绘制正弦函数曲线
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +  # 绘制曲线，颜色为蓝色
  ggtitle("正弦函数曲线") +     # 添加标题
  xlab("x") +                  # 添加 x 轴标签
  ylab("sin(x)")+              # 添加 y 轴标签
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中

## ----eval=TRUE----------------------------------------------------------------
# 使用 integrate 函数计算正弦函数在 [0, π] 上的定积分
result <- integrate(sin, lower = 0, upper = pi)

# 打印积分结果
print(paste("定积分结果为：", result$value))

## ----eval=TRUE----------------------------------------------------------------
# 设置随机种子以确保结果可重复
set.seed(111)

# 生成自变量 x，范围在 0 到 10 之间的 1000 个随机数
x <- runif(1000, min = 0, max = 10)

# 生成误差项，服从均值为 0，标准差为 1 的正态分布
epsilon <- rnorm(1000, mean = 0, sd = 1)

# 生成因变量 y，假设 y = 2x + 1 + 误差
y <- 2 * x + 1 + epsilon

# 创建数据框保存 x 和 y 的值
data <- data.frame(x = x, y = y)

## ----eval=TRUE----------------------------------------------------------------
# 进行线性回归分析，y 关于 x 的回归
model <- lm(y ~ x, data = data)

# 查看模型概要
summary(model)

## ----eval=TRUE----------------------------------------------------------------
# 绘制散点图和回归直线
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +                     # 绘制散点图
  geom_smooth(method = "lm", color = "red") +      # 添加回归直线
  ggtitle("线性回归分析结果") +                      # 添加标题
  xlab("自变量 x") +                               # 添加 x 轴标签
  ylab("因变量 y") +                                # 添加 y 轴标签
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中

## ----eval=TRUE----------------------------------------------------------------
# 定义解函数 y(x)
y <- function(x) {
  exp(-x) * (x + 1)
}

# 创建 x 的取值范围，从 0 到 5，共 100 个点
x_values <- seq(0, 5, length.out = 100)

# 计算对应的 y 值
y_values <- y(x_values)

# 创建数据框保存 x 和 y 的值
data <- data.frame(x = x_values, y = y_values)

# 使用 ggplot2 绘制解的曲线
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green") +      # 绘制曲线，颜色为绿色
  ggtitle("微分方程解的曲线") +       # 添加标题
  xlab("x") +                       # 添加 x 轴标签
  ylab("y(x)") +                     # 添加 y 轴标签
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中

## ----eval=TRUE----------------------------------------------------------------
# 加载 Iris 数据集
data(iris)

# 查看数据集的结构
str(iris)

## ----eval=TRUE----------------------------------------------------------------
# 提取测量变量（前 4 列）
iris_data <- iris[, 1:4]

# 进行 PCA 分析，数据中心化和标准化
pca_result <- prcomp(iris_data, center = TRUE, scale. = TRUE)

# 查看 PCA 结果的概要信息
summary(pca_result)

## ----eval=TRUE----------------------------------------------------------------
# 提取主成分得分，并添加物种信息
scores <- data.frame(pca_result$x, Species = iris$Species)

# 使用 ggplot2 绘制前两个主成分的散点图
ggplot(scores, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 2) +                   # 绘制散点，设置点的大小
  ggtitle("Iris 数据集的 PCA 分析") +         # 添加标题
  xlab("主成分 1") +                       # 添加 x 轴标签
  ylab("主成分 2") +                        # 添加 y 轴标签
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中

## ----eval=TRUE----------------------------------------------------------------
# 定义目标函数
f <- function(x, y) {
  x * y
}

# 创建可能的解点
points <- data.frame(
  x = c(1 / sqrt(2), -1 / sqrt(2), 1 / sqrt(2), -1 / sqrt(2), 0, 0),
  y = c(1 / sqrt(2), -1 / sqrt(2), -1 / sqrt(2), 1 / sqrt(2), 1, -1)
)

# 计算目标函数值
points$f_value <- with(points, f(x, y))

# 打印结果
print(points)

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
library(ggplot2)
library(ggforce)  # 用于绘制圆

# 创建 x 和 y 的网格
x_seq <- seq(-1, 1, length.out = 100)
y_seq <- seq(-1, 1, length.out = 100)
grid <- expand.grid(x = x_seq, y = y_seq)

# 定义目标函数
f <- function(x, y) {
  x * y
}

# 计算目标函数值
grid$z <- with(grid, f(x, y))

# 标记满足约束条件的点
grid$constraint <- with(grid, x^2 + y^2 <= 1)

# 过滤满足约束条件的点
grid <- grid[grid$constraint, ]

# 绘制目标函数的等高线图和约束条件
ggplot() +
  geom_contour_filled(data = grid, aes(x = x, y = y, z = z)) +  # 绘制等高线填充图
  geom_circle(aes(x0 = 0, y0 = 0, r = 1),                      # 绘制单位圆
              color = "red", inherit.aes = FALSE) +
  geom_point(data = points, aes(x = x, y = y),                 # 标记解点
             color = "blue", size = 3) +
  ggtitle("目标函数等高线图及约束条件") +                          # 添加标题
  xlab("x") +                                                  # 添加 x 轴标签
  ylab("y") +                                                   # 添加 y 轴标签
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子以确保结果可重复
set.seed(110)

# 定义生成 Rayleigh 分布随机样本的函数
generate_rayleigh <- function(n, sigma) {
  U <- runif(n)  # 生成 n 个均匀分布随机数
  X <- sigma * sqrt(-2 * log(U))  # 计算 Rayleigh 随机变量
  return(X)
}

# 选择不同的 σ 值
sigma_values <- c(0.5, 1, 2)

# 对每个 σ，生成样本并绘制直方图
par(mfrow = c(1, length(sigma_values)))  # 设置绘图区域

for (sigma in sigma_values) {
  samples <- generate_rayleigh(10000, sigma)  # 生成样本
  hist(samples, breaks = 50, probability = TRUE,
       main = paste("Rayleigh 分布 (σ =", sigma, ")"),
       xlab = "x", ylab = "密度", col = "gray", border = "white")
  
  # 添加理论密度曲线
  x_seq <- seq(0, max(samples), length.out = 1000)
  y_density <- (x_seq / sigma^2) * exp(-x_seq^2 / (2 * sigma^2))
  lines(x_seq, y_density, col = "red", lwd = 2)
  
  # 添加理论众数的垂直线
  abline(v = sigma, col = "blue", lwd = 2, lty = 2)
  
  # 添加图例
  legend("topright", legend = c("样本直方图", "理论密度", "理论众数"),
         col = c("gray", "red", "blue"), lwd = c(10, 2, 2), lty = c(1, 1, 2), bty = "n", cex = 0.8)
}


## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子以确保结果可重复
set.seed(111)

# 定义生成 Rayleigh 分布随机样本的函数
generate_rayleigh <- function(n, sigma) {
  U <- runif(n)  # 生成 n 个均匀分布随机数
  X <- sigma * sqrt(-2 * log(U))  # 计算 Rayleigh 随机变量
  return(X)
}

# 选择更多的 σ 值
sigma_values <- c(0.5, 1, 1.5, 2, 3, 5)

# 对每个 σ，生成样本并绘制直方图
par(mfrow = c(2, 3))  # 设置绘图区域为 2 行 3 列

for (sigma in sigma_values) {
  samples <- generate_rayleigh(1e5, sigma)  # 生成 1e5 个样本
  hist(samples, breaks = 100, probability = TRUE,
       main = paste("Rayleigh 分布 (σ =", sigma, ")"),
       xlab = "x", ylab = "密度", col = "lightblue", border = "white")
  
  # 添加理论密度曲线
  x_seq <- seq(0, max(samples), length.out = 1000)
  y_density <- (x_seq / sigma^2) * exp(-x_seq^2 / (2 * sigma^2))
  lines(x_seq, y_density, col = "red", lwd = 2)
  
  # 添加理论众数的垂直线
  abline(v = sigma, col = "blue", lwd = 2, lty = 2)
  
  # 添加图例
  legend("topright", legend = c("样本直方图", "理论密度", "理论众数"),
         col = c("lightblue", "red", "blue"), lwd = c(10, 2, 2), lty = c(1, 1, 2), bty = "n", cex = 0.8)

}

## ----eval=TRUE----------------------------------------------------------------
library(modeest)  # 用于计算众数的包

for (sigma in sigma_values) {
  samples <- generate_rayleigh(1e5, sigma)
  sample_mode <- mlv(samples, method = "parzen")  # 计算样本众数
  cat("σ =", sigma, "理论众数 =", sigma, "样本众数 =", sample_mode, "\n")
}

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(123)

# 定义生成混合正态分布样本的函数
generate_mixture_normal <- function(n, p1, mu1, mu2, sigma1, sigma2) {
  # 生成指示变量，1 表示来自第一个正态分布
  indicators <- rbinom(n, size = 1, prob = p1)
  
  # 初始化样本向量
  samples <- numeric(n)
  
  # 从对应的正态分布中生成随机数
  samples[indicators == 1] <- rnorm(sum(indicators == 1), mean = mu1, sd = sigma1)
  samples[indicators == 0] <- rnorm(sum(indicators == 0), mean = mu2, sd = sigma2)
  
  return(samples)
}

# 设置参数
n <- 1000
mu1 <- 0
mu2 <- 3
sigma1 <- 1
sigma2 <- 1
p1_values <- c(0.75, 0.5, 0.25)

# 调整图形参数，使其具有更高的宽高比例
par(mfrow = c(length(p1_values), 1), mar = c(4, 4, 2, 2))  # 调整边距

# 调整颜色
colors <- c("lightblue", "lightgreen", "lightcoral")

for (i in 1:length(p1_values)) {
  p1 <- p1_values[i]
  samples <- generate_mixture_normal(n, p1, mu1, mu2, sigma1, sigma2)
  
  # 绘制直方图，调整颜色和透明度
  hist(samples, breaks = 30, probability = TRUE,
       main = paste("混合正态分布 (p1 =", p1, ")"),
       xlab = "x", ylab = "密度", col = colors[i], border = "white", 
       xlim = range(-4, 8), ylim = c(0, 0.5))
  
  # 叠加理论密度曲线
  x_seq <- seq(min(samples), max(samples), length.out = 1000)
  y_density <- p1 * dnorm(x_seq, mean = mu1, sd = sigma1) +
               (1 - p1) * dnorm(x_seq, mean = mu2, sd = sigma2)
  lines(x_seq, y_density, col = "red", lwd = 2)
  
  # 添加图例
  legend("topright", legend = c("样本直方图", "理论密度曲线"),
         col = c(colors[i], "red"), lwd = c(10, 2), bty = "n", cex = 0.8)
}


## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(124)

# 定义生成混合正态分布样本的函数
generate_mixture_normal <- function(n, p1, mu1, mu2, sigma1, sigma2) {
  # 生成指示变量，1 表示来自第一个正态分布
  indicators <- rbinom(n, size = 1, prob = p1)
  
  # 初始化样本向量
  samples <- numeric(n)
  
  # 从对应的正态分布中生成随机数
  samples[indicators == 1] <- rnorm(sum(indicators == 1), mean = mu1, sd = sigma1)
  samples[indicators == 0] <- rnorm(sum(indicators == 0), mean = mu2, sd = sigma2)
  
  return(samples)
}

# 设置参数
n <- 10000
mu1 <- 0
mu2 <- 3
sigma1 <- 1
sigma2 <- 1
p1_values <- seq(0.1, 0.9, by = 0.1)  # 从 0.1 到 0.9，步长为 0.1

# 调整图形比例和边距，使每个图更大
par(mfrow = c(3, 3), mar = c(4, 4, 2, 2), oma = c(2, 2, 2, 2))  # 外边距参数调整

# 循环绘制每个 p1 值对应的混合正态分布
for (p1 in p1_values) {
  samples <- generate_mixture_normal(n, p1, mu1, mu2, sigma1, sigma2)
  
  # 绘制直方图，调整颜色和透明度
  hist(samples, breaks = 50, probability = TRUE,
       main = paste("混合正态分布 (p1 =", round(p1, 1), ")"),
       xlab = "x", ylab = "密度", col = rgb(144, 238, 144, maxColorValue = 255, alpha = 180), border = "white",
       xlim = c(-4, 8), ylim = c(0, 0.5))  # 统一x轴和y轴范围
  
  # 叠加理论密度曲线
  x_seq <- seq(min(samples), max(samples), length.out = 1000)
  y_density <- p1 * dnorm(x_seq, mean = mu1, sd = sigma1) +
               (1 - p1) * dnorm(x_seq, mean = mu2, sd = sigma2)
  lines(x_seq, y_density, col = "red", lwd = 2)
  
  # 将图例放在下方，确保不遮挡图形
  legend("bottom", legend = c("样本直方图", "理论密度"),
         col = c(rgb(144, 238, 144, maxColorValue = 255, alpha = 180), "red"),
         lwd = c(10, 2), bty = "n", cex = 0.8, horiz = TRUE)
}



## ----eval=TRUE----------------------------------------------------------------
library(moments,warn.conflicts = F)  # 用于计算峰度和偏度的包
# 由于moments 包中的 skewness 、kurtosis函数和 modeest 包中的 skewness 、kurtosis函数重名了
# 因此，使用warn.conflicts = F 忽略包重名时的警告

# 定义 p1 值
p1_values <- seq(0.1, 0.9, by = 0.1)
skewness_values <- numeric(length(p1_values))
kurtosis_values <- numeric(length(p1_values))

# 循环计算不同 p1 下的偏度和峰度
for (i in seq_along(p1_values)) {
  p1 <- p1_values[i]
  samples <- generate_mixture_normal(n, p1, mu1, mu2, sigma1, sigma2)
  
  # 明确调用 moments 包中的 skewness 和 kurtosis 函数
  skewness_values[i] <- moments::skewness(samples)
  kurtosis_values[i] <- moments::kurtosis(samples)
}

# 绘制偏度和峰度随 p1 的变化
par(mfrow = c(1, 2))

# 偏度随 p1 的变化
plot(p1_values, skewness_values, type = "b", pch = 19, col = "blue",
     xlab = expression(p[1]), ylab = "偏度", main = "偏度随 p1 的变化")
abline(h = 0, lty = 2)

# 峰度随 p1 的变化
plot(p1_values, kurtosis_values, type = "b", pch = 19, col = "red",
     xlab = expression(p[1]), ylab = "峰度", main = "峰度随 p1 的变化")


## ----eval=TRUE----------------------------------------------------------------
mu_differences <- c(1, 2, 3, 4, 5, 6)
p1 <- 0.5  # 固定 p1 为 0.5

par(mfrow = c(2, 3))  # 设置绘图区域

for (mu_diff in mu_differences) {
  mu1 <- 0
  mu2 <- mu_diff
  samples <- generate_mixture_normal(n, p1, mu1, mu2, sigma1, sigma2)
  
  hist(samples, breaks = 50, probability = TRUE,
       main = paste("均值差 =", mu_diff),
       xlab = "x", ylab = "密度", col = "lightblue", border = "white")
  
  x_seq <- seq(min(samples), max(samples), length.out = 1000)
  y_density <- p1 * dnorm(x_seq, mean = mu1, sd = sigma1) +
               (1 - p1) * dnorm(x_seq, mean = mu2, sd = sigma2)
  lines(x_seq, y_density, col = "red", lwd = 2)
}

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(133)

# 定义模拟复合泊松–Gamma 过程的函数
simulate_compound_poisson_gamma <- function(lambda, t, shape, rate, n_sim) {
  X_values <- numeric(n_sim)  # 存储模拟结果
  
  for (i in 1:n_sim) {
    N_t <- rpois(1, lambda * t)  # 生成 N(t)
    
    if (N_t > 0) {
      Y_i <- rgamma(N_t, shape = shape, rate = rate)  # 生成 Y_i
      X_values[i] <- sum(Y_i)  # 计算 X(t)
    } else {
      X_values[i] <- 0
    }
  }
  
  return(X_values)
}

# 设置参数
lambda <- 2     # 泊松过程参数
t <- 10         # 时间点
shape <- 3      # Gamma 分布形状参数 α
rate <- 2       # Gamma 分布速率参数 β
n_sim <- 10000  # 模拟次数

# 模拟复合泊松–Gamma 过程
X_values <- simulate_compound_poisson_gamma(lambda, t, shape, rate, n_sim)

# 估计均值和方差
estimated_mean <- mean(X_values)
estimated_var <- var(X_values)

# 计算理论均值和方差
E_Y1 <- shape / rate  # Gamma 分布均值
E_Y1_sq <- (shape * (shape + 1)) / rate^2  # Gamma 分布二阶原点矩

theoretical_mean <- lambda * t * E_Y1
theoretical_var <- lambda * t * E_Y1_sq

# 输出结果
cat("估计的均值：", estimated_mean, "理论均值：", round(theoretical_mean),"\n")
cat("估计的方差：", estimated_var,  "理论方差：", round(theoretical_var),"\n")


## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(134)

# 定义模拟复合泊松–Gamma 过程的函数
simulate_compound_poisson_gamma <- function(lambda, t, shape, scale, n_sim) {
  X_values <- numeric(n_sim)  # 存储模拟结果
  
  for (i in 1:n_sim) {
    N_t <- rpois(1, lambda * t)  # 生成 N(t)
    
    if (N_t > 0) {
      Y_i <- rgamma(N_t, shape = shape, scale = scale)  # 生成 Y_i
      X_values[i] <- sum(Y_i)  # 计算 X(t)
    } else {
      X_values[i] <- 0
    }
  }
  
  return(X_values)
}

# 参数集合
lambda_values <- c(0.5, 1, 2, 5)
shape_values <- c(1, 2, 3)
scale_values <- c(0.5, 1, 2)
t <- 10         # 时间点
n_sim <- 10000  # 模拟次数

# 遍历参数组合
for (lambda in lambda_values) {
  for (shape in shape_values) {
    for (scale in scale_values) {
      X_values <- simulate_compound_poisson_gamma(lambda, t, shape, scale, n_sim)
      
      # 估计均值和方差
      estimated_mean <- mean(X_values)
      estimated_var <- var(X_values)
      
      # 计算理论均值和方差
      E_Y1 <- shape * scale  # Gamma 分布均值（θ = scale）
      E_Y1_sq <- (shape * (shape + 1)) * scale^2  # Gamma 分布二阶原点矩
      
      theoretical_mean <- lambda * t * E_Y1
      theoretical_var <- lambda * t * E_Y1_sq
      
      # 输出结果
      cat("参数：lambda =", lambda, ", shape =", shape, ", scale =", scale, "\n")
      cat("估计的均值：", round(estimated_mean, 4), "理论均值：", round(theoretical_mean, 4), "\n")
      cat("估计的方差：", round(estimated_var, 4), "理论方差：", round(theoretical_var, 4), "\n\n")
    }
  }
}

## ----eval=TRUE----------------------------------------------------------------
# 选择一组参数
lambda <- 2
shape <- 2
scale <- 1

# 模拟
X_values <- simulate_compound_poisson_gamma(lambda, t, shape, scale, n_sim)

# 绘制直方图
hist(X_values, breaks = 50, probability = TRUE,
     main = paste("复合泊松–Gamma 过程分布 (λ =", lambda, ", α =", shape, ", θ =", scale, ")"),
     xlab = "X(t)", ylab = "密度", col = "lightcoral", border = "white")

# 添加核密度估计曲线
lines(density(X_values), col = "blue", lwd = 2)

# 添加理论均值和方差的垂直线
abline(v = mean(X_values), col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("直方图", "核密度估计", "均值"),
       col = c("lightcoral", "blue", "green"), lwd = c(10, 2, 2), lty = c(1, 1, 2))

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子，保证结果可重复
set.seed(101)

# 定义估计 Beta(3,3) 分布 CDF 的函数
estimate_beta_cdf <- function(x_values, N = 1e6) {
  # 生成 N 个 Beta(3,3) 分布的随机样本
  samples <- rbeta(N, shape1 = 3, shape2 = 3)
  
  # 初始化存储估计值的向量
  estimates <- numeric(length(x_values))
  
  # 对每个 x，计算估计的 CDF 值
  for (i in seq_along(x_values)) {
    x <- x_values[i]
    estimates[i] <- mean(samples <= x)
  }
  
  return(estimates)
}

# 定义 x 的取值
x_values <- seq(0.1, 0.9, by = 0.1)

# 估计 CDF 值
estimated_cdf <- estimate_beta_cdf(x_values)

# 使用 pbeta 函数计算真实的 CDF 值
true_cdf <- pbeta(x_values, shape1 = 3, shape2 = 3)

# 将结果整理成数据框
comparison <- data.frame(
  x = x_values,
  Estimated_CDF = estimated_cdf,
  True_CDF = true_cdf,
  Difference = abs(estimated_cdf - true_cdf)
)

# 打印比较结果
print(comparison)

## ----eval=TRUE----------------------------------------------------------------
# 绘制估计值和精确值的对比图
plot(x_values, true_cdf, type='l', col='red', lwd=2,
     ylab='F(x)', xlab='x', main='Beta(3,3) 累积分布函数')
points(x_values, estimated_cdf, col='blue', pch=16)
legend('bottomright', legend=c('精确值 (pbeta)', '蒙特卡罗估计'),
       col=c('red', 'blue'), lty=c(1, NA), pch=c(NA, 16))

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子，保证结果可重复
set.seed(102)

# 定义使用对偶变量生成 Rayleigh(σ) 分布样本的函数
generate_rayleigh_antithetic <- function(n, sigma) {
  # n 必须是偶数
  if (n %% 2 != 0) {
    stop("样本数量 n 必须是偶数")
  }
  
  # 生成 n/2 个均匀分布随机数
  U <- runif(n / 2)
  U_prime <- 1 - U  # 对偶变量
  
  # 计算对应的 Rayleigh 分布样本
  X <- sigma * sqrt(-2 * log(U))
  X_prime <- sigma * sqrt(-2 * log(U_prime))
  
  # 返回合并的样本
  return(c(X, X_prime))
}

# 定义参数
# 总样本数量，必须是偶数
n <- 1e6
# σ 值
sigma <- 1  

# 使用对偶变量生成样本
samples_antithetic <- generate_rayleigh_antithetic(n, sigma)
# 计算 (X + X') / 2
mean_antithetic <- (samples_antithetic[1:(n/2)] + samples_antithetic[(n/2 + 1):n]) / 2

# 生成独立的样本
X1 <- sigma * sqrt(-2 * log(runif(n/2)))
X2 <- sigma * sqrt(-2 * log(runif(n/2)))
mean_independent <- (X1 + X2) / 2

# 计算方差
var_antithetic <- var(mean_antithetic)
var_independent <- var(mean_independent)

# 计算方差减少的百分比
variance_reduction <- (1 - var_antithetic / var_independent) * 100

# 输出结果
cat("使用对偶变量的方差：", var_antithetic, "\n")
cat("使用独立变量的方差：", var_independent, "\n")
cat("方差减少百分比：", variance_reduction, "%\n")

## ----eval=TRUE----------------------------------------------------------------
# 绘制方差比较的柱状图
variance_values <- c(var_independent, var_antithetic)
names(variance_values) <- c("独立样本", "对偶变量")
barplot(variance_values, col=c("red", "blue"),
        main="使用独立样本和对偶变量的方差比较",
        ylab="方差")

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(103)

# 定义目标密度函数 g(x)
g <- function(x){
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# 定义重要性函数 f1(x)：截断的标准正态分布
f1_density <- function(x){
  dnorm(x) / (1 - pnorm(1))
}

# 从 f1 中采样
sample_f1 <- function(n){
  # 逆变换采样法
  u <- runif(n, min = pnorm(1), max = 1)
  x <- qnorm(u)
  return(x)
}

# 定义重要性函数 f2(x)：Gamma 分布，形状参数 k=3，尺度参数 θ=2
f2_density <- function(x){
  dgamma(x, shape = 3, scale = 2)  # 这里使用 Gamma 分布的密度函数
}

# 从 Gamma 分布中采样
sample_f2 <- function(n){
  x <- rgamma(n, shape = 3, scale = 2)  # 这里使用 Gamma 分布的采样函数
  # 只保留 x > 1 的部分
  x <- x[x > 1]
  # 如果数量不足 n，则继续采样
  while(length(x) < n){
    x_add <- rgamma(n - length(x), shape = 3, scale = 2)
    x <- c(x, x_add[x_add > 1])
  }
  x <- x[1:n]
  return(x)
}

# 计算重要性采样估计量和方差
importance_sampling <- function(n, f_sample, f_density, f_name){
  # 从重要性函数中采样
  x <- f_sample(n)
  # 计算权重 w = g(x) / f(x)
  w <- g(x) / f_density(x)
  # 计算估计量
  I_hat <- mean(w)
  # 计算方差
  var_hat <- var(w) / n
  # 返回结果
  return(list(I_hat = I_hat, var_hat = var_hat, x = x, w = w, f_name = f_name))
}

# 设置模拟次数
n <- 10000

# 使用 f1 进行重要性采样
result_f1 <- importance_sampling(n, sample_f1, f1_density, "截断正态分布 f1")

# 使用 f2 进行重要性采样（Gamma 分布）
result_f2 <- importance_sampling(n, sample_f2, f2_density, "Gamma 分布 f2")

# 理论值 I
I_true <- 1 - pnorm(1)

# 输出结果
cat("使用 f1 的估计量：", result_f1$I_hat, "\n")
cat("使用 f1 的方差：", result_f1$var_hat, "\n\n")

cat("使用 f2 的估计量：", result_f2$I_hat, "\n")
cat("使用 f2 的方差：", result_f2$var_hat, "\n\n")

cat("理论值 I：", I_true, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 绘制权重的直方图
par(mfrow = c(1,2))

# 使用 f1 的权重分布
hist(result_f1$w, breaks = 50, probability = TRUE, main = "使用 f1 的权重分布",
     xlab = "权重", col = "skyblue", xlim = c(0, max(result_f1$w)))

# 使用 f2 的权重分布
hist(result_f2$w, breaks = 50, probability = TRUE, main = "使用 f2 的权重分布",
     xlab = "权重", col = "pink", xlim = c(0, max(result_f2$w)))

## ----eval=TRUE----------------------------------------------------------------
# 清空工作环境
rm(list = ls())

# 1. 定义x的范围
x <- seq(1, 10, length.out = 1000)  # 从1到10，生成1000个点

# 2. 计算f1(x)：截断的标准正态分布
# f1(x) = φ(x) / (1 - Φ(1))，其中 φ(x) 是标准正态密度，Φ(x) 是标准正态累积分布
phi_x <- dnorm(x)  # 标准正态密度
Phi_1 <- pnorm(1)  # 标准正态在x=1处的累积分布值
f1 <- phi_x / (1 - Phi_1)  # 截断标准正态密度

# 3. 计算f2(x)：Gamma(3,2)分布
# f2(x) = x^2 * e^(-x/2) / 16
f2 <- dgamma(x, shape = 3, scale = 2)  # R内置的Gamma分布密度函数

# 4. 计算g(x)
# g(x) = (x^2 / sqrt(2π)) * e^(-x^2 / 2)
g <- (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)

# 5. 绘制图形
# 设置绘图区域，确保所有函数都能完整显示
plot(x, f1, type = 'l', col = 'blue', lwd = 2, 
     ylim = c(0, max(f1, f2, g)), 
     ylab = '密度', 
     xlab = 'x', 
     main = 'f₁(x)、f₂(x) 和 g(x) 的比较')

# 添加f2(x)和g(x)到同一图形
lines(x, f2, col = 'green', lwd = 2, lty = 2)  # f2(x)：绿色，虚线
lines(x, g, col = 'red', lwd = 2, lty = 3)     # g(x)：红色，点划线

# 添加图例
legend('topright', 
       legend = c('f₁(x)：截断标准正态', 'f₂(x)：Gamma(3,2)', 'g(x)'), 
       col = c('blue', 'green', 'red'), 
       lwd = 2, 
       lty = c(1, 2, 3))

# 6. 添加参考线（可选）
abline(v = 1, col = 'gray', lty = 4)  # 标记x=1的位置

## ----eval=TRUE----------------------------------------------------------------
# 自定义快速排序函数
quick_sort <- function(x) {
  if(length(x) <= 1) {
    return(x)
  }
  
  # 选择一个基准值（通常为第一个元素）
  pivot <- x[1]
  
  # 分成小于、等于和大于基准值的部分
  less <- x[x < pivot]
  equal <- x[x == pivot]
  greater <- x[x > pivot]
  
  # 递归调用
  return(c(quick_sort(less), equal, quick_sort(greater)))
}

# 设置随机数种子，保证结果可重复
set.seed(110)

# 定义样本规模 n 的取值
n_values <- c(1e4, 2e4, 4e4, 6e4, 8e4)

# 初始化存储平均计算时间 a_n 的向量
a_n <- numeric(length(n_values))

# 对于每个 n，进行 100 次模拟
for (i in seq_along(n_values)) {
  n <- n_values[i]
  times <- numeric(100)  # 存储每次模拟的计算时间
  
  for (j in 1:100) {
    # 生成 1 到 n 的随机排列
    x <- sample(n)
    
    # 记录排序开始时间
    start_time <- proc.time()
    # 使用自定义的快速排序对 x 进行排序
    sorted_x <- quick_sort(x)
    # 记录排序结束时间
    end_time <- proc.time()
    
    # 计算排序消耗的时间（以秒为单位）
    times[j] <- (end_time - start_time)["elapsed"]
  }
  
  # 计算 100 次模拟的平均计算时间
  a_n[i] <- mean(times)
  
  # 打印当前进度
  cat("n =", n, "平均计算时间 a_n =", a_n[i], "秒\n")
}

# 计算 t_n = n * log_2(n)
t_n <- n_values * log(n_values, base = 2)

# 对 a_n 与 t_n 进行线性回归
model <- lm(a_n ~ t_n)

# 打印回归结果
summary(model)


## ----eval=TRUE----------------------------------------------------------------
# 绘制散点图和回归线
plot(t_n, a_n, main = "平均计算时间 a_n 与 n log(n) 的关系",
     xlab = expression(t[n] == n * log(n)),
     ylab = expression(a[n]~"(秒)"),
     pch = 16, col = "blue")

# 添加回归线
abline(model, col = "red", lwd = 2)

# 添加图例
legend("topleft", legend = c("模拟数据", "回归线"),
       col = c("blue", "red"), pch = c(16, NA),
       lty = c(NA, 1), lwd = c(NA, 2))

## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子
set.seed(101)

# 1、设置参数
n_sample <- 50     # 样本容量
n_sim <- 1000000     # 模拟次数

# 2、模拟数据并计算偏度
# 初始化存储偏度值的向量
skewness_values <- numeric(n_sim)

# 定义计算样本偏度的函数
compute_skewness <- function(x) {
  n <- length(x)
  m3 <- sum((x - mean(x))^3) / n       # 三阶中心矩
  m2 <- sum((x - mean(x))^2) / n       # 二阶中心矩
  sqrt_b1 <- m3 / (m2^(3/2))           # 样本偏度
  return(sqrt_b1)
}

# 运行模拟
for (i in 1:n_sim) {
  sample_data <- rnorm(n_sample)                # 生成正态样本
  skewness_values[i] <- compute_skewness(sample_data)  # 计算偏度
}

# 3、估计分位数
quantile_levels <- c(0.025, 0.05, 0.95, 0.975)
estimated_quantiles <- quantile(skewness_values, probs = quantile_levels)

# 4、计算标准误差
# 计算偏度的方差和标准差
sigma2 <- 6 / n_sample    # 偏度的方差
sigma <- sqrt(sigma2)     # 偏度的标准差

# 在估计的分位数处计算密度函数值
f_xq <- dnorm(estimated_quantiles, mean = 0, sd = sigma)

# 使用公式 (2.14) 计算方差和标准误差
n <- n_sim   # 模拟次数
var_xq <- quantile_levels * (1 - quantile_levels) / (n * f_xq^2)
se_xq <- sqrt(var_xq)

# 5、计算理论分位数
theoretical_quantiles <- qnorm(quantile_levels, mean = 0, sd = sigma)

# 6、展示结果
results <- data.frame(
  Quantile_Level = quantile_levels,
  Estimated_Quantile = estimated_quantiles,
  SE = se_xq,
  Theoretical_Quantile = theoretical_quantiles
)

print(results)

## ----eval=TRUE----------------------------------------------------------------
# 绘制估计值和理论值的对比图
plot(quantile_levels, estimated_quantiles, type = "b", col = "blue", pch = 16,
     xlab = "分位数水平", ylab = "偏度 √b₁",
     main = "偏度的估计分位数与理论分位数比较")
lines(quantile_levels, theoretical_quantiles, type = "b", col = "red", pch = 17)
legend("bottomright", legend = c("估计分位数", "理论分位数"),
       col = c("blue", "red"), pch = c(16, 17), lty = 1)

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)  # 用于生成多元正态分布数据

# 设置参数
n <- 30       # 样本量
N <- 1000     # 模拟次数
rho <- 0.5    # 相关系数
alpha <- 0.05 # 显著性水平

# 存储拒绝原假设的次数
reject_counts_normal <- c(Pearson = 0, Spearman = 0, Kendall = 0)

# 设置随机种子以便复现
set.seed(102)

# 模拟过程
for (i in 1:N) {
  # 生成二维正态分布样本
  Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
  sample_data <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
  X <- sample_data[,1]
  Y <- sample_data[,2]
  
  # Pearson 检验
  test_pearson <- cor.test(X, Y, method = "pearson")
  if (test_pearson$p.value < alpha) {
    reject_counts_normal["Pearson"] <- reject_counts_normal["Pearson"] + 1
  }
  
  # Spearman 检验
  test_spearman <- cor.test(X, Y, method = "spearman")
  if (test_spearman$p.value < alpha) {
    reject_counts_normal["Spearman"] <- reject_counts_normal["Spearman"] + 1
  }
  
  # Kendall 检验
  test_kendall <- cor.test(X, Y, method = "kendall")
  if (test_kendall$p.value < alpha) {
    reject_counts_normal["Kendall"] <- reject_counts_normal["Kendall"] + 1
  }
}

# 计算功效
power_normal <- reject_counts_normal / N

# 将功效结果转换为数据框
power_normal_df <- data.frame(
  Test = names(power_normal),
  Power = as.numeric(power_normal)
)

# 展示功效数据框
print(power_normal_df)



## ----eval=TRUE----------------------------------------------------------------
# 绘制功效比较柱状图，并获取条形中点位置
bp <- barplot(power_normal, 
              main = "功效比较（二维正态分布）",
              ylab = "功效",
              ylim = c(0,1),
              col = c("steelblue", "forestgreen", "orange"),
              names.arg = power_normal_df$Test)  # 添加条形名称标签

# 在条形上方居中添加数值标签
text(x = bp, 
     y = power_normal + 0.02,  # 在条形高度上方添加0.02的偏移量
     label = round(power_normal_df$Power, 3), 
     cex = 0.8, 
     col = "black", 
     adj = c(0.5, 0))  # 水平居中，垂直靠下


## ----eval=TRUE----------------------------------------------------------------
# 设置参数
n <- 30        # 样本量
N <- 1000      # 模拟次数
alpha <- 0.05  # 显著性水平

# 存储拒绝原假设的次数，使用命名向量
reject_counts_nonnormal <- c(Pearson = 0, Spearman = 0, Kendall = 0)

# 设置随机种子以便复现
set.seed(103)

# 模拟过程
for (i in 1:N) {
  # 生成非正态分布样本
  X <- runif(n, -1, 1)            # X ~ U(-1, 1)
  epsilon <- rnorm(n, mean = 0, sd = 0.1)  # ε ~ N(0, 0.1²)
  Y <- X^2 + epsilon               # Y = X² + ε
  
  # Pearson 检验
  test_pearson <- cor.test(X, Y, method = "pearson")
  if (test_pearson$p.value < alpha) {
    reject_counts_nonnormal["Pearson"] <- reject_counts_nonnormal["Pearson"] + 1
  }
  
  # Spearman 检验
  test_spearman <- cor.test(X, Y, method = "spearman")
  if (test_spearman$p.value < alpha) {
    reject_counts_nonnormal["Spearman"] <- reject_counts_nonnormal["Spearman"] + 1
  }
  
  # Kendall 检验
  test_kendall <- cor.test(X, Y, method = "kendall")
  if (test_kendall$p.value < alpha) {
    reject_counts_nonnormal["Kendall"] <- reject_counts_nonnormal["Kendall"] + 1
  }
}

# 计算功效
power_nonnormal <- reject_counts_nonnormal / N

# 将功效结果转换为数据框
power_nonnormal_df <- data.frame(
  Test = names(power_nonnormal),
  Power = as.numeric(power_nonnormal)
)

# 展示功效数据框
print(power_nonnormal_df)



## ----eval=TRUE----------------------------------------------------------------
# 可视化功效结果
barplot(power_nonnormal_df$Power, 
        names.arg = power_nonnormal_df$Test,
        main = "功效比较（非正态二维分布）",
        ylab = "功效",
        ylim = c(0,1),
        col = c("steelblue", "forestgreen", "orange"),
        las = 1)

# 在条形上添加数值标签
text(x = bp, 
     y = power_nonnormal + 0.02,  # 在条形高度上方添加0.02的偏移量
     label = round(power_nonnormal_df$Power, 3), 
     cex = 0.8, 
     col = "black", 
     adj = c(0.5, 0))  # 水平居中，垂直靠下



## ----eval=TRUE----------------------------------------------------------------
# 绘制散点图
plot(X, Y, main = "非正态分布下的散点图", xlab = "X", ylab = "Y")

## ----eval=TRUE----------------------------------------------------------------
# 设置参数
n <- 10000  # 总实验次数
p1_hat <- 0.651  # 方法一的功效估计
p2_hat <- 0.676  # 方法二的功效估计

# 计算成功次数
x1 <- n * p1_hat
x2 <- n * p2_hat

# 计算合并成功率
p_pool <- (x1 + x2) / (2 * n)

# 计算标准误差
SE <- sqrt(p_pool * (1 - p_pool) * (2 / n))

# 计算 Z 统计量
z_value <- (p1_hat - p2_hat) / SE

# 计算双侧 P 值
p_value <- 2 * pnorm(-abs(z_value))

# 输出结果
cat("Z 统计量 =", z_value, "\n")
cat("P 值 =", p_value, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 绘制标准正态分布曲线
x_vals <- seq(-5, 5, length = 1000)
y_vals <- dnorm(x_vals)
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue", main = "标准正态分布", xlab = "Z 值", ylab = "密度")

# 标注 Z 统计量位置
abline(v = z_value, col = "red", lwd = 2, lty = 2)
abline(v = -z_value, col = "red", lwd = 2, lty = 2)

# 添加图例
legend("topright", legend = c("Z 统计量"), col = c("red"), lwd = 2, lty = 2)

## ----eval=TRUE----------------------------------------------------------------
Sys.setlocale("LC_ALL", "Chinese")  # 设置为中文环境

# 设置参数
N <- 10000  # 总实验次数
p1 <- 0.651 # 方法1的功效
p2 <- 0.676 # 方法2的功效

set.seed(104)  # 设置随机种子，保证结果可重复

# 生成方法1的检验结果（0：不拒绝，1：拒绝）
result1 <- rbinom(N, size = 1, prob = p1)

# 生成方法2的检验结果（0：不拒绝，1：拒绝）
result2 <- rbinom(N, size = 1, prob = p2)

# 构建四格表
# a：方法1拒绝，方法2拒绝
a <- sum(result1 == 1 & result2 == 1)

# b：方法1拒绝，方法2不拒绝
b <- sum(result1 == 1 & result2 == 0)

# c：方法1不拒绝，方法2拒绝
c <- sum(result1 == 0 & result2 == 1)

# d：方法1不拒绝，方法2不拒绝
d <- sum(result1 == 0 & result2 == 0)

# 构建列联表
contingency_table <- matrix(c(a, b, c, d), nrow = 2,
                            dimnames = list("方法1" = c("拒绝", "不拒绝"),
                                            "方法2" = c("拒绝", "不拒绝")))
print("列联表：")
print(contingency_table)

# 计算 McNemar 检验统计量
# 如果 b + c >= 25，可以使用近似的卡方分布
# 否则，应使用精确的二项检验
if ((b + c) >= 25) {
  # 近似卡方检验
  chi_square <- (abs(b - c) - 1)^2 / (b + c)
  p_value <- 1 - pchisq(chi_square, df = 1)
} else {
  # 精确二项检验
  p_value <- 2 * pbinom(min(b, c), size = b + c, prob = 0.5)
}

cat("McNemar 检验统计量：", chi_square, "\n")
cat("p 值：", p_value, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 加载绘图包
library(ggplot2)
library(reshape2)

# 将四格列联表转换为数据框
table_df <- as.data.frame.table(contingency_table)
colnames(table_df) <- c("方法1", "方法2", "次数")

# 绘制热力图
ggplot(table_df, aes(x = 方法2, y = 方法1, fill = 次数)) +
  geom_tile(color = "white") +
  geom_text(aes(label = 次数), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  ggtitle("列联表热力图") + 
  theme(plot.title = element_text(hjust = 0.5))


## ----eval=TRUE----------------------------------------------------------------
# 绘制卡方分布曲线
x_vals <- seq(0, 25, by = 0.01)
y_vals <- dchisq(x_vals, df = 1)

plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = expression(chi^2), ylab = "密度",
     main = "自由度为1的卡方分布")

# 标注检验统计量的位置
abline(v = chi_square, col = "red", lwd = 2, lty = 2)
text(chi_square, max(y_vals)/2, labels = paste("统计量 =", round(chi_square, 2)),
     pos = 4, col = "red")

# 计算自由度为1，显著性水平α = 0.05时的卡方分布临界值
critical_value <- qchisq(0.95, df = 1)

# 标注临界值的位置
abline(v = critical_value, col = "green", lwd = 2, lty = 2)
text(critical_value, max(y_vals)/2, labels = paste("临界值 =", round(critical_value, 2)),
     pos = 4, col = "green")


## ----eval=TRUE----------------------------------------------------------------
    d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, 
            -0.937, 0.779, -1.409, 0.027, -1.569);
    d2  <- c(1.608, 1.009,  0.878,  1.600, -0.263,  
             0.680, 2.280,  2.390, 1.793,  8.091, 1.468)

## ----eval=TRUE----------------------------------------------------------------
d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, -0.937, 0.779, -1.409, 0.027, -1.569)
d2 <- c(1.608, 1.009, 0.878, 1.600, -0.263, 0.680, 2.280, 2.390, 1.793, 8.091, 1.468)

## ----eval=TRUE----------------------------------------------------------------
# 计算样本d1的均值
mean_d1 <- mean(d1)

# 计算样本d2的均值
mean_d2 <- mean(d2)

# 计算均值差异（d1均值 - d2均值）
mean_diff <- mean_d1 - mean_d2

# 输出结果
cat("样本d1的均值为：", mean_d1, "\n")
cat("样本d2的均值为：", mean_d2, "\n")
cat("原始样本的均值差异为：", mean_diff, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 计算样本d1的方差和大小
var_d1 <- var(d1)
n1 <- length(d1)

# 计算样本d2的方差和大小
var_d2 <- var(d2)
n2 <- length(d2)

# 计算标准误差
SE <- sqrt(var_d1 / n1 + var_d2 / n2)

# 输出结果
cat("均值差异的样本标准误差为：", SE, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 设置重复抽样次数
R <- 10000

# 初始化向量用于存储均值差异
boot_diff <- numeric(R)

# 设置随机数种子以保证结果可重复
set.seed(123)

# 开始Bootstrap抽样
for (i in 1:R) {
  # 对d1和d2进行有放回的抽样
  boot_d1 <- sample(d1, size = n1, replace = TRUE)
  boot_d2 <- sample(d2, size = n2, replace = TRUE)
  
  # 计算抽样后的均值差异
  boot_diff[i] <- mean(boot_d1) - mean(boot_d2)
}

# 计算Bootstrap标准误差
boot_SE <- sd(boot_diff)

# 输出结果
cat("均值差异的Bootstrap标准误差为：", boot_SE, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 原始数据
d1 <- c(-2.961, 0.478, -0.391, -0.869, -0.460, -0.937, 0.779, -1.409, 0.027, -1.569)
d2 <- c(1.608, 1.009, 0.878, 1.600, -0.263, 0.680, 2.280, 2.390, 1.793, 8.091, 1.468)

# 1. 计算原始样本的均值差异
mean_d1 <- mean(d1)
mean_d2 <- mean(d2)
mean_diff <- mean_d1 - mean_d2
cat("样本d1的均值为：", mean_d1, "\n")
cat("样本d2的均值为：", mean_d2, "\n")
cat("原始样本的均值差异为：", mean_diff, "\n")

# 2. 计算样本标准误差
var_d1 <- var(d1)
n1 <- length(d1)
var_d2 <- var(d2)
n2 <- length(d2)
SE <- sqrt(var_d1 / n1 + var_d2 / n2)
cat("均值差异的样本标准误差为：", SE, "\n")

# 3. 使用Bootstrap方法计算Bootstrap标准误差
R <- 10000
boot_diff <- numeric(R)
set.seed(123)
for (i in 1:R) {
  boot_d1 <- sample(d1, size = n1, replace = TRUE)
  boot_d2 <- sample(d2, size = n2, replace = TRUE)
  boot_diff[i] <- mean(boot_d1) - mean(boot_d2)
}
boot_SE <- sd(boot_diff)
cat("均值差异的Bootstrap标准误差为：", boot_SE, "\n")

## ----eval=TRUE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(101)  # 设置随机数种子，保证结果可重复

# 参数设置
N <- 1000       # 总假设数
n_null <- 950   # 原假设数
n_alt <- 50     # 备择假设数
m <- 10000      # 模拟次数
alpha <- 0.1    # 显著性水平

# 初始化结果矩阵
results <- matrix(0, nrow = 3, ncol = 2)
rownames(results) <- c("FWER", "FDR", "TPR")
colnames(results) <- c("Bonferroni校正", "B-H校正")

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的库
library(ggplot2)

# 初始化存储变量
fwer_bonf <- numeric(m)
fdr_bonf <- numeric(m)
tpr_bonf <- numeric(m)

fwer_bh <- numeric(m)
fdr_bh <- numeric(m)
tpr_bh <- numeric(m)

# 执行模拟
for (i in 1:m) {
  # 生成 p 值
  p_null <- runif(n_null)               # 原假设的 p 值
  p_alt <- rbeta(n_alt, 0.1, 1)         # 备择假设的 p 值
  p_values <- c(p_null, p_alt)          # 合并所有 p 值
  true_labels <- c(rep(0, n_null), rep(1, n_alt))  # 标签：0-原假设，1-备择假设
  
  # Bonferroni 校正
  p_bonf <- p.adjust(p_values, method = "bonferroni")
  rejected_bonf <- which(p_bonf < alpha)
  
  V_bonf <- sum(true_labels[rejected_bonf] == 0)  # 误拒的原假设数
  S_bonf <- sum(true_labels[rejected_bonf] == 1)  # 正确拒绝的备择假设数
  R_bonf <- length(rejected_bonf)                 # 总共拒绝的假设数
  
  fwer_bonf[i] <- as.numeric(V_bonf > 0)          # 至少一个假阳性
  fdr_bonf[i] <- ifelse(R_bonf > 0, V_bonf / R_bonf, 0)  # 错误发现率
  tpr_bonf[i] <- S_bonf / n_alt                   # 真阳性率
  
  # B-H 校正
  p_bh <- p.adjust(p_values, method = "BH")
  rejected_bh <- which(p_bh < alpha)
  
  V_bh <- sum(true_labels[rejected_bh] == 0)
  S_bh <- sum(true_labels[rejected_bh] == 1)
  R_bh <- length(rejected_bh)
  
  fwer_bh[i] <- as.numeric(V_bh > 0)
  fdr_bh[i] <- ifelse(R_bh > 0, V_bh / R_bh, 0)
  tpr_bh[i] <- S_bh / n_alt
  
  # 可视化 p 值分布（密度图）
  if (i == 1) { # 只绘制第一次模拟的 p 值分布
    # 创建一个数据框，用于 ggplot2 绘图
    p_data <- data.frame(
      p_value = p_values,
      hypothesis = factor(true_labels, labels = c("Null Hypothesis", "Alternative Hypothesis"))
    )
    
    # 绘制密度图
    p_plot <- ggplot(p_data, aes(x = p_value, fill = hypothesis, color = hypothesis)) +
      geom_density(alpha = 0.4) +  # 绘制密度曲线
      geom_vline(xintercept = alpha, linetype = "dashed", color = "red") +  # 显著性水平线
      labs(title = "P-value Density for Null and Alternative Hypotheses",
           x = "P-value",
           y = "Density") +
      theme_minimal()
    
    # 打印图形
    print(p_plot)
  }
  
  # 可视化 p 值分布（散点图）
  if (i == m) { # 只绘制第m次模拟的 p 值分布
    # 创建一个数据框，用于 ggplot2 绘图
    p_data <- data.frame(
      p_value = p_values,
      hypothesis = factor(true_labels, labels = c("Null Hypothesis", "Alternative Hypothesis"))
    )
  
    # 绘制散点图
    p_plot <- ggplot(p_data, aes(x = hypothesis, y = p_value, color = hypothesis)) +
      geom_jitter(width = 0.2, height = 0) +  # 使用散点抖动效果
      geom_hline(yintercept = alpha, linetype = "dashed", color = "red") +  # 显著性水平线
      labs(title = "P-value Distribution for Null and Alternative Hypotheses",
         x = "Hypothesis Type",
         y = "P-value") +
      theme_minimal()
  
    # 打印图形
    print(p_plot)
  }
}

## ----eval=TRUE----------------------------------------------------------------
# 计算平均值
results["FWER", "Bonferroni校正"] <- mean(fwer_bonf)
results["FDR", "Bonferroni校正"] <- mean(fdr_bonf)
results["TPR", "Bonferroni校正"] <- mean(tpr_bonf)

results["FWER", "B-H校正"] <- mean(fwer_bh)
results["FDR", "B-H校正"] <- mean(fdr_bh)
results["TPR", "B-H校正"] <- mean(tpr_bh)

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的库
library(knitr)
library(kableExtra)

# 示例结果数据框（将行和列调整）
results <- data.frame(
  Metric = c("FWER", "FDR", "TPR"),
  Bonferroni = c(mean(fwer_bonf), mean(fdr_bonf), mean(tpr_bonf)),
  `B-H` = c(mean(fwer_bh), mean(fdr_bh), mean(tpr_bh))
)

# 使用 kableExtra 美化表格
kable(results, digits = 4, caption = "不同校正方法下的 FWER、FDR 和 TPR") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  column_spec(1, bold = TRUE, background = "#D3D3D3") %>%  # 加粗第一列（指标列），背景浅灰
  column_spec(2:3, color = "black", background = "#F0F8FF") %>%  # 校正方法的列
  row_spec(0, bold = TRUE, font_size = 14) %>%  # 标题行加粗并增大字号
  add_header_above(c(" " = 1, "校正方法" = 2))  # 合并标题：校正方法

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的库
library(reshape2)
library(ggplot2)

# 将结果数据框转换为长格式
results_melt <- melt(results, id.vars = "Metric", variable.name = "CorrectionMethod", value.name = "Value")

# 绘制柱状图
ggplot(results_melt, aes(x = Metric, y = Value, fill = CorrectionMethod)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "不同校正方法下的 FWER、FDR 和 TPR 比较",
       x = "指标",
       y = "值",
       fill = "校正方法") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # 标题居中且加粗
    axis.text.x = element_text(size = 12),  # 调整X轴标签的字体大小
    axis.text.y = element_text(size = 12),  # 调整Y轴标签的字体大小
    legend.title = element_text(size = 12),  # 调整图例标题的字体大小
    legend.text = element_text(size = 10)  # 调整图例文本的字体大小
  )

## ----eval=TRUE----------------------------------------------------------------
# 加载 boot 包和数据
library(boot)
data("aircondit")
x <- aircondit$hours

# 样本大小
n <- length(x)

## ----eval=TRUE----------------------------------------------------------------
# 计算 λ 的 MLE
lambda_hat <- n / sum(x)
lambda_hat

## ----eval=TRUE----------------------------------------------------------------
# 定义用于 Bootstrap 的统计量函数
boot_lambda <- function(data, indices) {
  # 从数据中抽样
  sample_data <- data[indices]
  # 计算 λ 的 MLE
  n_boot <- length(sample_data)
  sum_boot <- sum(sample_data)
  lambda_boot <- n_boot / sum_boot
  return(lambda_boot)
}

## ----eval=TRUE----------------------------------------------------------------
# 设置 Bootstrap 重复次数
R <- 10^5
# 进行 Bootstrap
set.seed(110)
boot_results <- boot(data = x, statistic = boot_lambda, R = R)

## ----eval=TRUE----------------------------------------------------------------
# 估计偏差
bias_lambda <- mean(boot_results$t) - lambda_hat
bias_lambda

# 估计标准差
se_lambda <- sd(boot_results$t)
se_lambda

## ----eval=TRUE----------------------------------------------------------------
# 绘制 λ 的 Bootstrap 分布直方图
hist(boot_results$t, breaks = 30, main = expression("Bootstrap 分布 (" * lambda * ")"), xlab = expression(lambda), col = "lightblue", probability = TRUE)
abline(v = lambda_hat, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("MLE 估计值"), col = c("red"), lwd = 2, lty = 2)

## ----eval=TRUE----------------------------------------------------------------
cat(sprintf("λ 的 MLE 估计值为：%.6f\n", lambda_hat))

## ----eval=TRUE----------------------------------------------------------------
cat(sprintf("偏差估计为：%.6f\n", bias_lambda))

## ----eval=TRUE----------------------------------------------------------------
cat(sprintf("标准差估计为：%.6f\n", se_lambda))

## ----eval=TRUE----------------------------------------------------------------
# 将 λ 转换为 θ = 1/λ
boot_theta <- boot_results
boot_theta$t <- 1 / boot_theta$t
theta_hat <- 1 / lambda_hat
theta_hat

## ----eval=TRUE----------------------------------------------------------------
# 定义一个用于 Bootstrap 方法的统计量函数，用于计算平均故障间隔时间（θ = 1/λ）
mean_time_boot <- function(data, indices) {
  # 从原始数据中抽取 Bootstrap 样本
  # 'data' 是原始数据集，'indices' 是抽样索引（包含有放回的抽样位置）
  sample_data <- data[indices]
  
  # 计算抽样样本的大小（观测值数量）
  n_boot <- length(sample_data)
  
  # 计算抽样样本中所有故障时间的总和
  sum_boot <- sum(sample_data)
  
  # 计算 Bootstrap 样本的最大似然估计（MLE）λ的估计值
  # λ 的 MLE 公式为 λ_hat = n / Σx_i，其中 n 是样本大小，Σx_i 是故障时间总和
  lambda_hat <- n_boot / sum_boot
  
  # 计算平均故障间隔时间 θ 的估计值，θ = 1/λ
  theta_boot <- 1 / lambda_hat
  
  # 返回 θ 的估计值作为 Bootstrap 样本的统计量
  return(theta_boot)
}


## ----eval=TRUE----------------------------------------------------------------
set.seed(133)  # 设置随机数种子，保证结果可重复

# 数据：空调设备的故障时间（小时）
failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 使用bootstrap方法计算
bootstrap_mean_time_results <- boot(data = failure_times, statistic = mean_time_boot, R = 10000)


## ----eval=TRUE----------------------------------------------------------------

# 标准正态法置信区间
norm_ci <- boot.ci(bootstrap_mean_time_results, type = "norm")

# 基本法置信区间
basic_ci <- boot.ci(bootstrap_mean_time_results, type = "basic")

# 百分位法置信区间
perc_ci <- boot.ci(bootstrap_mean_time_results, type = "perc")

# BCa 法置信区间
bca_ci <- boot.ci(bootstrap_mean_time_results, type = "bca")

## ----eval=TRUE----------------------------------------------------------------
# 加载 kableExtra 包
library(kableExtra)

# 创建数据框存储置信区间
ci_methods <- c("标准正态法", "基本法", "百分位法", "BCa 法")
lower_bounds <- c(norm_ci$normal[2], basic_ci$basic[4], perc_ci$percent[4], bca_ci$bca[4])
upper_bounds <- c(norm_ci$normal[3], basic_ci$basic[5], perc_ci$percent[5], bca_ci$bca[5])

ci_table <- data.frame(
  方法 = ci_methods,
  下限 = lower_bounds,
  上限 = upper_bounds
)

# 使用 kableExtra 美化表格
ci_table %>%
  kbl(digits = 4, caption = "平均故障间隔时间的 95% Bootstrap 置信区间") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center", 
                font_size = 14) %>%
  column_spec(1, bold = TRUE, color = "white", background = "gray") %>%  # 第一列加粗、加背景色
  column_spec(2:3, width = "10em") %>%  # 设置列宽
  add_header_above(c(" " = 1, "置信区间" = 2))  # 添加合并表头

## ----eval=TRUE----------------------------------------------------------------
library(ggplot2)

ci_table$方法 <- factor(ci_table$方法, levels = ci_methods)

ggplot(ci_table, aes(x = 方法, y = (下限 + 上限) / 2)) +
  geom_errorbar(aes(ymin = 下限, ymax = 上限), width = 0.2, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "不同方法的 95% Bootstrap 置信区间",
       x = "方法",
       y = expression("平均故障间隔时间 (" * theta * ")")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # 将标题居中

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的库
library(bootstrap)    # 包含scor数据集
library(ggplot2)     # 绘图
library(gridExtra)   # 排版多个图形

# 读取数据
data("scor")  # 加载scor数据集

# 查看数据集的基本信息
str(scor)
summary(scor)

# 样本容量
n <- nrow(scor)

## ----eval=TRUE----------------------------------------------------------------
# 计算全样本的协方差矩阵
cov_mat <- cov(scor)

# 计算协方差矩阵的特征值（从大到小排序）
eigen_values <- eigen(cov_mat)$values

# 计算全样本的估计量 hat_theta
hat_theta <- eigen_values[1] / sum(eigen_values)
print(paste("全样本估计量 hat_theta =", round(hat_theta, 5)))

## ----eval=TRUE----------------------------------------------------------------
# 初始化向量，存储每个删失样本的估计量
theta_jackknife <- numeric(n)

# 开始Jackknife过程
for (i in 1:n) {
  # 构建删失样本，删除第i个观测值
  scor_minus_i <- scor[-i, ]
  
  # 计算删失样本的协方差矩阵
  cov_mat_i <- cov(scor_minus_i)
  
  # 计算特征值
  eigen_values_i <- eigen(cov_mat_i)$values
  
  # 计算删失样本的估计量
  theta_jackknife[i] <- eigen_values_i[1] / sum(eigen_values_i)
}

## ----eval=TRUE----------------------------------------------------------------
# 计算Jackknife均值
theta_bar <- mean(theta_jackknife)

# 计算偏差的Jackknife估计
bias_jackknife <- (n - 1) * (theta_bar - hat_theta)

# 计算标准误差的Jackknife估计
se_jackknife <- sqrt((n - 1) * mean((theta_jackknife - theta_bar)^2))

# 输出结果
print(paste("Jackknife估计的偏差 =", round(bias_jackknife, 5)))
print(paste("Jackknife估计的标准误差 =", round(se_jackknife, 5)))

## ----eval=TRUE----------------------------------------------------------------
# 绘制删失样本估计量的分布直方图
df <- data.frame(Theta = theta_jackknife)
p1 <- ggplot(df, aes(x = Theta)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 15) +
  geom_vline(xintercept = hat_theta, color = "red", linetype = "dashed") +
  labs(title = "删失样本估计量的分布", x = expression(hat(theta)[(i)]), y = "频数") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

# 绘制删失样本估计量的箱线图
p2 <- ggplot(df, aes(y = Theta)) +
  geom_boxplot(fill = "lightgreen") +
  geom_hline(yintercept = hat_theta, color = "red", linetype = "dashed") +
  labs(title = "删失样本估计量的箱线图", y = expression(hat(theta)[(i)])) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

# 显示图形
grid.arrange(p1, p2, nrow = 1)

# 绘制删失样本估计量的QQ图，检验正态性
p3 <- ggplot(df, aes(sample = Theta)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "删失样本估计量的QQ图") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

# 显示QQ图
print(p3)


## ----eval=TRUE----------------------------------------------------------------
# 加载DAAG包，包含ironslag数据集
library(DAAG)

# 加载ironslag数据集，并附加到搜索路径
data(ironslag)
attach(ironslag)

## ----eval=TRUE----------------------------------------------------------------
# 获取数据集的样本数量
n <- length(magnetic)

# 初始化四个模型的预测误差向量
e1 <- e2 <- e3 <- e4 <- numeric(n)

# 创建用于绘制拟合曲线的自变量序列（在chemical范围内等间距取100个点）
a <- seq(min(chemical), max(chemical), length.out = 100)

## ----eval=TRUE----------------------------------------------------------------
# 对每个观测值进行循环
for (k in 1:n) {
  # 留出第k个观测值作为验证集，其余作为训练集
  y_train <- magnetic[-k]       # 训练集的响应变量
  x_train <- chemical[-k]       # 训练集的自变量
  
  # 线性模型：Y = β0 + β1 * X
  model1 <- lm(y_train ~ x_train)
  y_pred1 <- predict(model1, newdata = data.frame(x_train = chemical[k]))
  e1[k] <- magnetic[k] - y_pred1
  
  # 二次多项式模型：Y = β0 + β1 * X + β2 * X^2
  model2 <- lm(y_train ~ x_train + I(x_train^2))
  y_pred2 <- predict(model2, newdata = data.frame(x_train = chemical[k]))
  e2[k] <- magnetic[k] - y_pred2
  
  # 指数模型：log(Y) = β0 + β1 * X
  model3 <- lm(log(y_train) ~ x_train)
  log_y_pred3 <- predict(model3, newdata = data.frame(x_train = chemical[k]))
  y_pred3 <- exp(log_y_pred3)  # 反变换
  e3[k] <- magnetic[k] - y_pred3
  
  # 三次多项式模型：Y = β0 + β1 * X + β2 * X^2 + β3 * X^3
  model4 <- lm(y_train ~ x_train + I(x_train^2) + I(x_train^3))
  y_pred4 <- predict(model4, newdata = data.frame(x_train = chemical[k]))
  e4[k] <- magnetic[k] - y_pred4
  # 清理临时变量
  rm(y_train, x_train, model1, y_pred1, model2, y_pred2, 
     model3, log_y_pred3, y_pred3, model4, y_pred4)
  
  # 调用垃圾回收
  gc()
}

## ----eval=TRUE----------------------------------------------------------------
# 计算残差平方的平均值，即MSE
mse1 <- mean(e1^2)  # 线性模型的MSE
mse2 <- mean(e2^2)  # 二次多项式模型的MSE
mse3 <- mean(e3^2)  # 指数模型的MSE
mse4 <- mean(e4^2)  # 三次多项式模型的MSE

# 将MSE结果存入向量并命名
mse_values <- c(mse1, mse2, mse3, mse4)
names(mse_values) <- c("线性模型", "二次多项式模型", "指数模型", "三次多项式模型")

# 输出MSE结果
print("各模型的交叉验证均方误差（MSE）：")
print(mse_values)

## ----eval=TRUE----------------------------------------------------------------
# 模型1：线性模型
full_model1 <- lm(magnetic ~ chemical)
adj_r2_1 <- summary(full_model1)$adj.r.squared

# 模型2：二次多项式模型
full_model2 <- lm(magnetic ~ chemical + I(chemical^2))
adj_r2_2 <- summary(full_model2)$adj.r.squared

# 模型3：指数模型
full_model3 <- lm(log(magnetic) ~ chemical)
adj_r2_3 <- summary(full_model3)$adj.r.squared

# 模型4：三次多项式模型
full_model4 <- lm(magnetic ~ chemical + I(chemical^2) + I(chemical^3))
adj_r2_4 <- summary(full_model4)$adj.r.squared

# 将调整后的R平方结果存入向量并命名
adj_r2_values <- c(adj_r2_1, adj_r2_2, adj_r2_3, adj_r2_4)
names(adj_r2_values) <- c("线性模型", "二次多项式模型", "指数模型", "三次多项式模型")

# 输出调整后的R平方结果
print("各模型的调整后的R平方：")
print(adj_r2_values)

## ----eval=TRUE----------------------------------------------------------------
# 设置绘图区域为2行2列
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), mgp = c(2, 0.5, 0))  # 设置边距

# 线性模型拟合曲线
plot(chemical, magnetic, main = "线性模型", pch = 16, col = "black",
     xlab = "Chemical Concentration", ylab = "Magnetic Response", col.main = "blue")
y_fit1 <- predict(full_model1, newdata = data.frame(chemical = a))
lines(a, y_fit1, lwd = 2, col = "blue")

# 二次多项式模型拟合曲线
plot(chemical, magnetic, main = "二次多项式模型", pch = 16, col = "black",
     xlab = "Chemical Concentration", ylab = "Magnetic Response", col.main = "red")
y_fit2 <- predict(full_model2, newdata = data.frame(chemical = a))
lines(a, y_fit2, lwd = 2, col = "red")

# 指数模型拟合曲线
plot(chemical, magnetic, main = "指数模型", pch = 16, col = "black",
     xlab = "Chemical Concentration", ylab = "Magnetic Response", col.main = "green")
log_y_fit3 <- predict(full_model3, newdata = data.frame(chemical = a))
y_fit3 <- exp(log_y_fit3)
lines(a, y_fit3, lwd = 2, col = "green")

# 三次多项式模型拟合曲线
plot(chemical, magnetic, main = "三次多项式模型", pch = 16, col = "black",
     xlab = "Chemical Concentration", ylab = "Magnetic Response", col.main = "purple")
y_fit4 <- predict(full_model4, newdata = data.frame(chemical = a))
lines(a, y_fit4, lwd = 2, col = "purple")

# 重置绘图区域
par(mfrow = c(1, 1))


## ----eval=TRUE----------------------------------------------------------------
# 设置绘图区域为2行2列，以及图形边距
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), mgp = c(2, 0.5, 0))

# 线性模型残差图
plot(fitted(full_model1), resid(full_model1), main = "线性模型残差", xlab = "拟合值", ylab = "残差", 
     pch = 16, col = "blue", cex = 0.6)
abline(h = 0, col = "red", lwd = 2)

# 二次多项式模型残差图
plot(fitted(full_model2), resid(full_model2), main = "二次多项式模型残差", xlab = "拟合值", ylab = "残差", 
     pch = 16, col = "green", cex = 0.6)
abline(h = 0, col = "red", lwd = 2)

# 指数模型残差图
plot(fitted(full_model3), resid(full_model3), main = "指数模型残差", xlab = "拟合值", ylab = "残差", 
     pch = 16, col = "orange", cex = 0.6)
abline(h = 0, col = "red", lwd = 2)

# 三次多项式模型残差图
plot(fitted(full_model4), resid(full_model4), main = "三次多项式模型残差", xlab = "拟合值", ylab = "残差", 
     pch = 16, col = "purple", cex = 0.6)
abline(h = 0, col = "red", lwd = 2)

# 重置绘图区域
par(mfrow = c(1, 1))


## ----eval=TRUE----------------------------------------------------------------
# 输出线性模型的系数
print("线性模型系数：")
print(coef(full_model1))

# 输出二次多项式模型的系数
print("二次多项式模型系数：")
print(coef(full_model2))

# 输出指数模型的系数
print("指数模型系数：")
print(coef(full_model3))

# 输出三次多项式模型的系数
print("三次多项式模型系数：")
print(coef(full_model4))

# 分析完成后，清理内存
detach("ironslag")  # 从搜索路径中移除数据集
rm(ironslag)        # 从全局环境中删除数据集

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
library(ggplot2)

# 加载chickwts数据集并提取所需数据
#--------------------------------------------
# 提取soybean组的数据
x <- sort(as.vector(chickwts$weight[chickwts$feed == "soybean"]))
# 提取linseed组的数据
y <- sort(as.vector(chickwts$weight[chickwts$feed == "linseed"]))

# 查看两个样本的大小
n <- length(x)  # soybean组的样本量
m <- length(y)  # linseed组的样本量

## ----eval=TRUE----------------------------------------------------------------
cvM_stat <- function(x, y) {
  # 计算样本大小
  n <- length(x)
  m <- length(y)
  N <- n + m  # 总样本量
  
  # 合并并排序两个样本
  z <- c(x, y)
  z_sorted <- sort(z)
  
  # 计算经验分布函数
  # ecdf函数返回一个函数，可以计算指定值的经验分布值
  Fn <- ecdf(x)  # soybean组的经验分布函数
  Gm <- ecdf(y)  # linseed组的经验分布函数
  
  # 在合并排序的数据点上计算经验分布值
  F_values <- Fn(z_sorted)
  G_values <- Gm(z_sorted)
  
  # 计算经验分布函数差值的平方和
  diff_sq <- (F_values - G_values)^2
  sum_diff_sq <- sum(diff_sq)
  
  # 计算Cramér-von Mises统计量
  omega2 <- (n * m) / N^2 * sum_diff_sq
  
  return(omega2)
}

## ----eval=TRUE----------------------------------------------------------------
omega2_obs <- cvM_stat(x, y)
print(paste("观测统计量 omega^2 =", omega2_obs))

## ----eval=TRUE----------------------------------------------------------------
# 创建数据框用于绘图
data_ecdf <- data.frame(
  weight = c(x, y),
  group = factor(rep(c("Soybean", "Linseed"), times = c(n, m)))
)

# 绘制ECDF曲线
p1 <- ggplot(data_ecdf, aes(x = weight, color = group)) +
  stat_ecdf(linewidth = 1) +
  labs(title = "Soybean和Linseed组的经验分布函数",
       x = "体重（克）",
       y = "经验分布函数") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

print(p1)

## ----eval=TRUE----------------------------------------------------------------
# 计算在合并排序数据点上的ECDF值
z_combined <- sort(c(x, y))
Fn_values <- ecdf(x)(z_combined)
Gm_values <- ecdf(y)(z_combined)
diff_values <- Fn_values - Gm_values

# 创建数据框用于绘图
data_diff <- data.frame(
  weight = z_combined,
  diff = diff_values
)

# 绘制ECDF差值图
p2 <- ggplot(data_diff, aes(x = weight, y = diff)) +
  geom_step(direction = "hv") +
  labs(title = "经验分布函数差值（Fn - Gm）",
       x = "体重（克）",
       y = "差值") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

print(p2)

## ----eval=TRUE----------------------------------------------------------------
set.seed(123)  # 设置随机种子以保证结果可重复
R <- 999       # 置换次数
N <- n + m     # 总样本量
z <- c(x, y)   # 合并样本

omega2_perm <- numeric(R)  # 创建向量存储置换统计量

for (i in 1:R) {
  # 随机置换合并样本的顺序
  permuted_indices <- sample(1:N, N, replace = FALSE)
  # 根据置换后的索引分配样本
  x_perm <- z[permuted_indices[1:n]]
  y_perm <- z[permuted_indices[(n+1):N]]
  
  # 计算置换样本的Cramér-von Mises统计量
  omega2_perm[i] <- cvM_stat(x_perm, y_perm)
}

## ----eval=TRUE----------------------------------------------------------------
p_value <- mean(c(omega2_obs, omega2_perm) >= omega2_obs)
print(paste("置换检验的p值 =", p_value))

## ----eval=TRUE----------------------------------------------------------------
# 创建数据框用于绘图
data_perm <- data.frame(
  omega2 = omega2_perm
)

p3 <- ggplot(data_perm, aes(x = omega2)) +
  geom_histogram(binwidth = 0.01, fill = "lightblue", color = "white", boundary = 0) +
  geom_vline(xintercept = omega2_obs, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Cramér-von Mises统计量的置换分布",
       x = expression(omega^2),
       y = "频数") +
  annotate("text", x = omega2_obs, y = Inf, label = "观测统计量", color = "red", vjust = -1, hjust = -0.1) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

print(p3)

## ----eval=TRUE----------------------------------------------------------------
print(p1)  # 显示ECDF曲线图

## ----eval=TRUE----------------------------------------------------------------
print(p2)  # 显示ECDF差值图

## ----eval=TRUE----------------------------------------------------------------
print(p3)  # 显示置换统计量直方图

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
# 如果需要绘制更高级的图形，可以加载ggplot2包
# install.packages("ggplot2")  # 如未安装ggplot2，可先安装
library(ggplot2)

# 设置随机数种子，确保结果可重复
set.seed(124)

# 设置样本量
n <- 100  # 样本量为100

# 生成自变量x，服从标准正态分布
x <- rnorm(n)

# 生成因变量y，使其与x有线性关系，并加入随机噪声
# y = 5 * x + 随机噪声
y <- 5 * x + rnorm(n)

## ----eval=TRUE----------------------------------------------------------------
# 计算观察到的Spearman秩相关系数
rho_obs <- cor(x, y, method = "spearman")

# 输出观察到的Spearman秩相关系数
cat("观察到的Spearman秩相关系数 rho_obs =", rho_obs, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 使用cor.test函数进行Spearman秩相关性检验
spearman_test <- cor.test(x, y, method = "spearman")

# 提取p值
p_value_cor_test <- spearman_test$p.value

# 输出cor.test函数计算的p值
cat("使用cor.test函数计算的p值 =", p_value_cor_test, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 设置置换次数
B <- 10000  # 置换次数为10000次

# 初始化向量，用于存储每次置换的Spearman秩相关系数
rho_perm <- numeric(B)

# 进行置换检验
for (i in 1:B) {
  # 随机置换y的值，生成置换样本
  y_perm <- sample(y)
  
  # 计算置换样本的Spearman秩相关系数
  rho_perm[i] <- cor(x, y_perm, method = "spearman")
}

# 计算置换检验的p值
p_value_perm <- mean(abs(rho_perm) >= abs(rho_obs))

# 输出置换检验的p值
cat("置换检验计算的p值 =", p_value_perm, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 绘制x和y的散点图，直观展示二者的关系
plot(x, y, main = "变量x与变量y的散点图",
     xlab = "变量x", ylab = "变量y",
     pch = 19, col = "blue")

## ----eval=TRUE----------------------------------------------------------------
# 创建数据框，包含rho_perm数据
df_perm <- data.frame(rho_perm = rho_perm)

# 使用ggplot2绘制置换分布的密度图
ggplot(df_perm, aes(x = rho_perm)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "lightblue", color = "white") +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(xintercept = rho_obs, color = "red", linewidth = 1) +
  labs(title = "置换检验的Spearman秩相关系数密度图",
       x = "Spearman秩相关系数", y = "密度") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

## ----eval=TRUE----------------------------------------------------------------
# 将结果整理为数据框，便于查看
result_df <- data.frame(
  方法 = c("cor.test检验", "置换检验"),
  p值 = c(p_value_cor_test, p_value_perm)
)

# 输出结果数据框
print(result_df)

## ----eval=TRUE----------------------------------------------------------------
cat("观察到的Spearman秩相关系数 rho_obs =", rho_obs, "\n")

## ----eval=TRUE----------------------------------------------------------------
cat("使用cor.test函数计算的p值 =", p_value_cor_test, "\n")

## ----eval=TRUE----------------------------------------------------------------
cat("置换检验计算的p值 =", p_value_perm, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 绘制x和y的散点图
plot(x, y, main = "变量x与变量y的散点图",
     xlab = "变量x", ylab = "变量y",
     pch = 19, col = "blue")

## ----eval=TRUE----------------------------------------------------------------
# 使用ggplot2绘制置换分布的密度图
ggplot(df_perm, aes(x = rho_perm)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "lightblue", color = "white") +
  geom_density(color = "blue", linewidth = 1) +
  geom_vline(xintercept = rho_obs, color = "red", linewidth = 1) +
  labs(title = "置换检验的Spearman秩相关系数密度图",
       x = "Spearman秩相关系数", y = "密度") +
  theme_minimal()

## ----eval=TRUE----------------------------------------------------------------
# 将结果整理为数据框，便于查看
result_df <- data.frame(
  方法 = c("cor.test检验", "置换检验"),
  p值 = c(p_value_cor_test, p_value_perm)
)
print(result_df)

## ----eval=TRUE----------------------------------------------------------------
# 清空内存
rm(list = ls())

# 加载必要的库
library(ggplot2)  # 用于数据可视化
library(coda)     # 用于Gelman-Rubin诊断
library(gridExtra)  # 用于组合多个图形
library(kableExtra) # 用于表格绘制

## ----eval=TRUE----------------------------------------------------------------
# Metropolis-Hastings采样函数
metropolis_hastings <- function(iter, initial, proposal_sd) {
  samples <- numeric(iter)  # 初始化样本向量
  samples[1] <- initial  # 设置初始值
  
  for (i in 2:iter) {
    # 从提议分布生成候选样本
    candidate <- rnorm(1, mean = samples[i - 1], sd = proposal_sd)
    
    # 计算接受率
    acceptance_ratio <- dcauchy(candidate) / dcauchy(samples[i - 1])
    if (runif(1) < acceptance_ratio) {
      samples[i] <- candidate  # 接受候选样本
    } else {
      samples[i] <- samples[i - 1]  # 保留当前样本
    }
  }
  
  return(samples)  # 返回样本
}

## ----eval=TRUE----------------------------------------------------------------
# 运行Metropolis-Hastings采样
set.seed(123)  # 设置随机数种子
n_iterations <- 10000  # 总迭代次数
initial_value <- 0  # 初始值
proposal_sd <- 1  # 提议分布标准差

# 生成样本
samples <- metropolis_hastings(n_iterations, initial_value, proposal_sd)

# 丢弃前1000个样本
samples <- samples[-(1:1000)]

## ----eval=TRUE----------------------------------------------------------------
# 加载必要的包
library(knitr)       # 用于美化表格
library(magrittr)    # 提供管道操作符

# 计算生成样本的十分位数
deciles_samples <- quantile(samples, probs = seq(0.1, 0.9, by = 0.1))

# 计算理论标准Cauchy分布的十分位数
deciles_theoretical <- qcauchy(seq(0.1, 0.9, by = 0.1))

# 创建对比表格
decile_comparison <- data.frame(
  Decile = seq(0.1, 0.9, by = 0.1),
  Sample_Quantiles = deciles_samples,
  Theoretical_Quantiles = deciles_theoretical
)

# 美化表格并输出
kable(decile_comparison, format = "html", caption = "十分位数对比表格") %>%
  kable_styling(full_width = F, position = "center")


## ----eval=TRUE----------------------------------------------------------------
library(ggplot2)

# 将数据转换为长格式以便绘图
decile_long <- tidyr::pivot_longer(
  decile_comparison,
  cols = c(Sample_Quantiles, Theoretical_Quantiles),
  names_to = "Type",
  values_to = "Quantile"
)

# 创建绘图
p <-ggplot(decile_long, aes(x = Decile, y = Quantile, color = Type)) +
  geom_point(size = 3) +  # 添加点
  geom_line() +  # 添加连线
  labs(title = "样本十分位数与理论十分位数对比",
       x = "十分位数",
       y = "Quantile") +
  theme_minimal() +  # 使用简洁主题
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )+
  scale_color_manual(values = c("Sample_Quantiles" = "blue", "Theoretical_Quantiles" = "red")) +  # 自定义颜色
  theme(legend.title = element_blank())  # 隐藏图例标题
print(p)

## ----eval=TRUE----------------------------------------------------------------
# Gelman-Rubin收敛监测
n_chains <- 5  # 设置链的数量
chains <- replicate(n_chains, metropolis_hastings(n_iterations, initial_value, proposal_sd))
chains <- chains[-(1:1000), ]  # 丢弃前1000个样本

# 计算各链的均值和方差
chain_means <- apply(chains, 2, mean)
chain_vars <- apply(chains, 2, var)

# 计算总均值和总方差
overall_mean <- mean(chain_means)
overall_var <- mean(chain_vars)

# Gelman-Rubin统计量
B <- sum((chain_means - overall_mean)^2) * n_iterations / (n_chains - 1)
W <- mean(chain_vars)
R_hat <- sqrt((W + B / n_iterations) / W)

# 输出R_hat值
print("Gelman-Rubin统计量 (R_hat):")
print(R_hat)

## ----eval=TRUE----------------------------------------------------------------
# 选择每隔10个迭代的样本
sample_indices <- seq(1, nrow(chains), by = 10)
matplot(t(chains[sample_indices, ]), type = 'l', col = 1:n_chains, lty = 1,
        main = '多条链样本轨迹图', xlab = '迭代次数', ylab = '样本值')
legend("topright", legend = paste("链", 1:n_chains), col = 1:n_chains, lty = 1)

## ----eval=TRUE----------------------------------------------------------------
# 绘制样本的直方图及密度图
ggplot(data.frame(samples), aes(x = samples)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = 'blue', alpha = 0.7) +
  stat_function(fun = dcauchy, color = 'red', linewidth = 1) +
  ggtitle('样本的直方图与密度图') +
  xlab('样本值') + ylab('密度')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 可选：调整字体样式
  )

## ----eval=TRUE----------------------------------------------------------------
# 绘制样本密度与理论值对比图
plot(density(samples), main = '样本密度图', xlab = '样本值', ylab = '密度', col = 'blue')
lines(density(samples), col = 'blue', lwd = 2)
curve(dcauchy, add = TRUE, col = 'red', lwd = 2)  # 加入理论密度曲线
legend("topright", legend = c("样本密度", "理论Cauchy密度"), col = c("blue", "red"), lty = 1)

## ----eval=TRUE----------------------------------------------------------------
print(R_hat)

## ----eval=TRUE----------------------------------------------------------------
rm(list = ls())
set.seed(103)

# 参数设置
n <- 10          # 二项分布参数n
a <- 2           # Beta分布参数a
b <- 3           # Beta分布参数b
niter <- 5000    # 迭代次数
nchains <- 3     # 链的数量
burnin <- 1000   # 预热期长度

## ----eval=TRUE----------------------------------------------------------------
gibbs_sampler <- function(n, a, b, niter, init_y) {
  # 初始化存储空间
  x <- numeric(niter)
  y <- numeric(niter)
  y[1] <- init_y
  
  # 存储每次迭代的接受率
  acceptance <- numeric(niter - 1)
  
  # Gibbs采样迭代
  for (i in 1:(niter - 1)) {
    # 从条件分布p(x|y)中抽样
    x[i + 1] <- rbinom(1, n, y[i])
    
    # 从条件分布p(y|x)中抽样
    y[i + 1] <- rbeta(1, x[i + 1] + a, n - x[i + 1] + b)
    
    # 计算接受率（这里总是1，因为Gibbs采样总是接受新值）
    acceptance[i] <- 1
  }
  
  return(list(
    x = x, 
    y = y,
    acceptance_rate = mean(acceptance)
  ))
}

## ----eval=TRUE----------------------------------------------------------------
chains <- list()
init_values <- c(0.2, 0.5, 0.8)  # 使用不同的初始值

for (i in 1:nchains) {
  chains[[i]] <- gibbs_sampler(n, a, b, niter, init_values[i])
}


## ----eval=TRUE----------------------------------------------------------------
gelman_rubin <- function(chains, burnin) {
  nchains <- length(chains)
  niter <- length(chains[[1]]$y) - burnin
  
  # 提取燃烧期后的数据
  chain_data <- lapply(chains, function(chain) {
    chain$y[(burnin + 1):length(chain$y)]
  })
  
  # 计算链内均值
  chain_means <- sapply(chain_data, mean)
  
  # 计算链间方差B
  B <- niter * var(chain_means)
  
  # 计算链内方差W
  W <- mean(sapply(chain_data, var))
  
  # 计算方差估计和R_hat
  var_est <- (1 - 1/niter) * W + (1/niter) * B
  R_hat <- sqrt(var_est / W)
  
  # 返回诊断信息
  return(list(
    R_hat = R_hat,
    W = W,
    B = B,
    var_est = var_est
  ))
}

## ----eval=TRUE----------------------------------------------------------------
autocorr_analysis <- function(chain, lags = 0:20) {
  acf_values <- acf(chain$y, lag.max = max(lags), plot = FALSE)
  return(acf_values$acf[lags + 1])
}

effective_size <- function(chain) {
  acf_vals <- autocorr_analysis(chain)
  tau <- 1 + 2 * sum(acf_vals[-1])  # 初始正序列估计
  n_eff <- length(chain$y) / tau
  return(n_eff)
}

## ----eval=TRUE----------------------------------------------------------------
diag_results <- gelman_rubin(chains, burnin)
print("Gelman-Rubin诊断结果：")
print(paste("R_hat =", round(diag_results$R_hat, 4)))
print(paste("链内方差 (W) =", round(diag_results$W, 4)))
print(paste("链间方差 (B) =", round(diag_results$B, 4)))

## ----eval=TRUE----------------------------------------------------------------
eff_sizes <- sapply(chains, effective_size)
print("各链的有效样本大小为：")
print(round(eff_sizes))

## ----eval=TRUE----------------------------------------------------------------
library(ggplot2)
library(gridExtra)

# 准备绘图数据
plot_data <- data.frame(
  iteration = rep(1:niter, nchains),
  chain = factor(rep(1:nchains, each = niter)),
  y = unlist(lapply(chains, function(x) x$y))
)

# 跟踪图
p1 <- ggplot(plot_data, aes(x = iteration, y = y, color = chain)) +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = burnin, linetype = "dashed", color = "red") +
  labs(title = "参数y的跟踪图", subtitle = paste("R_hat =", round(diag_results$R_hat, 4)),
       x = "迭代次数", y = "参数值") +
  theme_minimal() + theme(text = element_text(family = "sans"))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 调整字体样式
  )

# 打印显示跟踪图
print(p1)

# 密度图
p2 <- ggplot(plot_data[plot_data$iteration > burnin,], aes(x = y, fill = chain)) +
  geom_density(alpha = 0.3) +
  labs(title = "参数y的后验密度", x = "参数值", y = "密度") +
  theme_minimal() + theme(text = element_text(family = "sans"))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 调整字体样式
  )
# 打印显示密度图
print(p2)

# 自相关函数图
acf_data <- lapply(chains, function(chain) {
  acf_vals <- autocorr_analysis(chain)
  data.frame(lag = 0:20, acf = acf_vals)
})
acf_plot_data <- do.call(rbind, Map(function(d, i) { d$chain <- factor(i); return(d) }, acf_data, 1:length(chains)))

p3 <- ggplot(acf_plot_data, aes(x = lag, y = acf, color = chain)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "自相关函数图", x = "滞后", y = "自相关") +
  theme_minimal() + theme(text = element_text(family = "sans"))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 调整字体样式
  )
# 打印显示自相关函数图
print(p3)


## ----eval=TRUE----------------------------------------------------------------
summary_stats <- data.frame(
  Chain = 1:nchains,
  Mean = sapply(chains, function(x) mean(x$y[(burnin + 1):niter])),
  SD = sapply(chains, function(x) sd(x$y[(burnin + 1):niter])),
  Q25 = sapply(chains, function(x) quantile(x$y[(burnin + 1):niter], 0.25)),
  Median = sapply(chains, function(x) median(x$y[(burnin + 1):niter])),
  Q75 = sapply(chains, function(x) quantile(x$y[(burnin + 1):niter], 0.75)),
  ESS = eff_sizes
)
print("\n各链的汇总统计量：")
print(round(summary_stats, 4))

# 计算指定滞后下的自相关值
compute_acf <- function(chain, lags = c(1, 5, 10)) {
  acf_values <- acf(chain$y, lag.max = max(lags), plot = FALSE)$acf
  return(acf_values[lags + 1])  # lags + 1是因为acf从滞后0开始
}

# 对每条链计算Lag1, Lag5和Lag10的自相关
acf_results <- data.frame(
  Chain = 1:nchains,
  Lag1 = sapply(chains, function(chain) compute_acf(chain, 1)),
  Lag5 = sapply(chains, function(chain) compute_acf(chain, 5)),
  Lag10 = sapply(chains, function(chain) compute_acf(chain, 10))
)

# 显示结果
print("自相关值 (Lag1, Lag5, Lag10):")
print(round(acf_results, 4))


## ----eval=TRUE----------------------------------------------------------------
# 设置随机数种子,便于复现结果
set.seed(105)   

# 目标分布的概率密度函数f(x)
# 此处目标分布为双峰分布，由两个正态分布的加权和构成
f <- function(x) {
  0.3 * dnorm(x, mean = -2, sd = 1) + 0.7 * dnorm(x, mean = 2, sd = 2)
}

# 提议分布g(r|s)，选择标准正态分布，均值为当前状态s
# 用于从当前状态转移到候选状态
g <- function(r, s) {
  dnorm(r, mean = s)  
}

## ----eval=TRUE----------------------------------------------------------------
# 接受概率函数 alpha(r, s)
# 计算在从当前状态s转移到候选状态r时的接受概率
alpha <- function(r, s) {
  min(f(r) * g(s, r) / (f(s) * g(r, s)), 1) 
}

## ----eval=TRUE----------------------------------------------------------------
# Metropolis-Hastings采样函数
# 参数 iter_num：采样迭代次数
MH_sampler <- function(iter_num) {
  # 随机选取初始状态
  state <- rnorm(1) 
  
  # 初始化一个向量，用于储存采样结果
  samples <- numeric(iter_num) 
  
  # 开始采样迭代
  for (i in 1:iter_num) {
    # 从提议分布中抽取候选状态
    candidate <- rnorm(1, mean = state)
    
    # 计算从当前状态到候选状态的接受概率
    accept_prob <- alpha(candidate, state)
    
    # 根据接受概率决定是否接受候选状态
    # 若接受则将当前状态更新为候选状态；否则保持原状态
    if (runif(1) < accept_prob) {
      state <- candidate  
    }
    
    # 记录当前状态（无论是否接受候选状态）
    samples[i] <- state 
  }
  
  # 返回采样结果
  samples 
}

## ----eval=TRUE----------------------------------------------------------------
# 从目标分布中抽取10000个样本，用于对比Metropolis-Hastings采样结果
samples_target <- numeric(10000)

for (i in 1:10000) {
  # 生成一个[0,1]的随机数temp，用于决定从哪个正态分布采样
  temp <- runif(1)
  
  # 若temp < 0.3，则从N(-2, 1)采样；否则从N(2, 2)采样
  if (temp < 0.3) {
    samples_target[i] <- rnorm(1, -2, 1) 
  } else {
    samples_target[i] <- rnorm(1, 2, 2)
  }
}

## ----eval=TRUE----------------------------------------------------------------
# 使用Metropolis-Hastings算法抽取10000个样本
samples_MH <- MH_sampler(10000) 

## ----eval=TRUE----------------------------------------------------------------
# 加载绘图所需的库
library(ggplot2)

# 将目标分布和Metropolis-Hastings采样分布的数据整合为一个数据框
df <- data.frame(
  x = c(samples_target, samples_MH),
  group = rep(c("Target", "MH"), each = 10000)
) 

# 绘制密度函数图，显示目标分布和采样分布的重合情况
ggplot(df, aes(x, fill = group)) +
  geom_density(alpha = 0.4) +  # 设置透明度
  theme_minimal() +  # 使用简洁主题
  labs(title = "目标分布 vs Metropolis-Hastings采样分布")  # 添加标题

## ----eval=TRUE----------------------------------------------------------------
# 清空工作空间，释放内存
rm(list = ls())

## ----eval=TRUE----------------------------------------------------------------
# 计算第 k 项的函数
compute_term <- function(k, a_norm, d) {
  # 符号部分：(-1)^k
  sign_part <- (-1)^k
  
  # 对数运算初始化
  ln_Tk <- 0
  
  # 部分1：ln(1 / (k! * 2^k))
  ln_part1 <- -lgamma(k + 1) - k * log(2)
  
  # 部分2：ln(||a||^(2k+2) / ((2k+1)(2k+2)))
  ln_part2 <- (2 * k + 2) * log(a_norm) - log(2 * k + 1) - log(2 * k + 2)
  
  # 部分3：ln(Γ((d+1)/2))
  ln_part3 <- lgamma((d + 1) / 2)
  
  # 部分4：ln(Γ(k + 3/2))
  ln_part4 <- lgamma(k + 1.5)
  
  # 部分5：-ln(Γ(k + d/2 + 1))
  ln_part5 <- -lgamma(k + d / 2 + 1)
  
  # 总的对数值
  ln_Tk <- ln_part1 + ln_part2 + ln_part3 + ln_part4 + ln_part5
  
  # 计算第 k 项的值
  Tk <- sign_part * exp(ln_Tk)
  
  return(Tk)
}

## ----eval=TRUE----------------------------------------------------------------
library(knitr)
library(kableExtra)

# 计算级数和的函数，修改后能展示表格结果
compute_series_sum <- function(a, d, tol = 1e-10, max_iter = 1000) { 
  # 计算向量 a 的欧几里得范数
  a_norm <- sqrt(sum(a^2)) 
   
  # 初始化参数
  sum_series <- 0       # 累加的级数和
  k <- 0                # 迭代计数器
  term_values <- c()    # 存储每一项的值
  sum_values <- c()     # 存储级数和的变化
  k_values <- c()       # 存储项数 k
 
  repeat { 
    # 计算第 k 项
    Tk <- compute_term(k, a_norm, d)
     
    # 存储当前项的值
    term_values <- c(term_values, Tk)
    sum_series <- sum_series + Tk
    sum_values <- c(sum_values, sum_series)
    k_values <- c(k_values, k)
     
    # 判断收敛条件
    if (abs(Tk) < tol) { 
      break 
    } 
     
    # 更新迭代计数器
    k <- k + 1 
     
    # 防止超过最大迭代次数
    if (k > max_iter) { 
      warning("达到最大迭代次数，级数可能未完全收敛") 
      break 
    } 
  }
   
  # 创建一个包含计算过程的表格数据框
  result_table <- data.frame(
    k = k_values,
    Term = term_values,
    CumulativeSum = sum_values,
    AbsTerm = abs(term_values)
  )
   
  # 返回结果
  return(list(
    sum = sum_series, 
    iterations = k, 
    terms_table = result_table
  )) 
}

# 定义向量 a 和维度 d
a_vector <- c(1, 2)
d_value <- length(a_vector)

# 调用函数计算级数和
result <- compute_series_sum(a_vector, d_value)

# 显示结果表格
kable(result$terms_table, caption = "级数项的计算过程")



## ----eval=TRUE----------------------------------------------------------------
# 定义向量 a 和维度 d
a_vector <- c(1, 2)
d_value <- length(a_vector)

# 调用函数计算级数和
result <- compute_series_sum(a_vector, d_value)

# 输出结果
cat("当 a = (1, 2)^T 时，级数的和为：", result$sum, "\n")
cat("计算迭代次数为：", result$iterations, "\n")

## ----eval=TRUE----------------------------------------------------------------
# 导入 ggplot2
library(ggplot2)

# 准备绘图数据
term_df <- result$terms_table

# 绘制级数项的绝对值
plot_terms <- ggplot(term_df, aes(x = k, y = AbsTerm)) +
  geom_line(color = "#1f77b4", linewidth = 1.2, linetype = "solid") +  # 使用柔和的蓝色和较粗的线条
  geom_point(color = "#ff7f0e", size = 3, alpha = 0.8) +  # 使用橙色点并调整透明度
  scale_y_log10(labels = scales::label_math(format = scales::scientific_format(digits = 2))) +  # 对数刻度优化显示
  labs(
    title = "级数项的收敛情况（对数刻度）",
    subtitle = "展示各项绝对值的收敛趋势",
    x = "项数 k",
    y = expression(paste("绝对值 ", "|T[k]|"))
  ) +
  theme_minimal(base_size = 15) +  # 使用更大的基础字体大小
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),  # 标题加粗并居中
    plot.subtitle = element_text(hjust = 0.5),  # 副标题居中
    axis.text = element_text(size = 12),  # 坐标轴文本大小调整
    axis.title = element_text(face = "bold"),  # 坐标轴标题加粗
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  # 增加主要网格线
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted")   # 增加次要网格线
  ) +
  annotate("text", x = max(term_df$k) * 0.8, y = min(term_df$AbsTerm) * 10,
           label = "收敛趋势明显", color = "darkred", size = 5, fontface = "italic")  # 添加文本标注

# 显示图形
print(plot_terms)


## ----eval=TRUE----------------------------------------------------------------
library(ggplot2)

# 检查 result$terms_table 是否为空
if (!is.null(result$terms_table) && nrow(result$terms_table) > 0) {
  # 准备绘图数据
  sum_df <- data.frame(
    k = result$terms_table$k,
    Sum = result$terms_table$CumulativeSum
  )

  # 使用 ggplot2 绘制级数和的变化，并进行美化
  plot_sums <- ggplot(sum_df, aes(x = k, y = Sum)) +
    geom_line(color = "#2ca02c", linewidth = 1.2, linetype = "solid") +  # 使用柔和的绿色，线条较粗
    geom_point(color = "#d62728", size = 3, alpha = 0.8) +  # 使用红色点并调整透明度
    labs(
      title = "级数和的收敛情况",
      subtitle = "展示各项累积和的收敛趋势",
      x = "项数 k",
      y = expression(paste("级数和 ", S[k]))
    ) +
    theme_minimal(base_size = 15) +  # 使用较大的基础字体大小
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),  # 标题加粗并居中
      plot.subtitle = element_text(hjust = 0.5),  # 副标题居中
      axis.text = element_text(size = 12),  # 坐标轴文本大小调整
      axis.title = element_text(face = "bold"),  # 坐标轴标题加粗
      panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  # 增加主要网格线
      panel.grid.minor = element_line(color = "grey90", linetype = "dotted")   # 增加次要网格线
    ) +
    annotate("text", x = max(sum_df$k) * 0.8, y = max(sum_df$Sum) * 0.9,
             label = "收敛趋势明显", color = "blue", size = 5, fontface = "italic")  # 添加文本标注

  # 显示图形
  print(plot_sums)
} else {
  print("result$terms_table 为空，无法绘制图形")
}



## ----eval=TRUE----------------------------------------------------------------
# 显示结果表格（交互式）
# 忽略DT包版本警告
suppressWarnings(library(DT))
library(DT)
datatable(result$terms_table, options = list(pageLength = 10), caption = "级数项的计算过程")


## ----eval=TRUE----------------------------------------------------------------
# 显示图形
print(plot_terms)

## ----eval=TRUE----------------------------------------------------------------
# 显示图形
print(plot_sums)

## ----eval=TRUE----------------------------------------------------------------
# 清空工作空间，释放内存
rm(list = ls())

# 加载必要的库
library(ggplot2)

## ----eval=TRUE----------------------------------------------------------------
# 计算 c_{k-1}(a)
calc_c_k_minus1 <- function(a, k) {
  numerator <- a^2 * (k - 1)
  denominator <- k - a^2
  if (denominator <= 0) {
    return(NA)  # 避免负数开方
  }
  c_k_minus1 <- sqrt(numerator / denominator)
  return(c_k_minus1)
}

# 计算 c_k(a)
calc_c_k <- function(a, k) {
  numerator <- a^2 * k
  denominator <- k + 1 - a^2
  if (denominator <= 0) {
    return(NA)
  }
  c_k <- sqrt(numerator / denominator)
  return(c_k)
}

## ----eval=TRUE----------------------------------------------------------------
# 计算对数生存函数，避免数值下溢
log_survival_t <- function(x, df) {
  if (is.na(x)) {
    return(NA)
  }
  # 使用 pt() 函数的 log.p = TRUE 参数计算对数生存函数
  log_S <- pt(q = x, df = df, lower.tail = FALSE, log.p = TRUE)
  return(log_S)
}

## ----eval=TRUE----------------------------------------------------------------
# 定义目标函数 f(a)
f_a <- function(a, k) {
  # 检查 a 的取值范围
  if (a <= 0 || a >= sqrt(k)) {
    return(NA)
  }
  
  # 计算 c_{k-1} 和 c_k
  c_k_minus1 <- calc_c_k_minus1(a, k)
  c_k <- calc_c_k(a, k)
  
  # 如果计算失败，返回 NA
  if (is.na(c_k_minus1) || is.na(c_k)) {
    return(NA)
  }
  
  # 计算对数生存函数值
  log_S_k_minus1 <- log_survival_t(c_k_minus1, df = k - 1)
  log_S_k <- log_survival_t(c_k, df = k)
  
  # 检查生存函数值是否为 NA
  if (is.na(log_S_k_minus1) || is.na(log_S_k)) {
    return(NA)
  }
  
  # 返回对数概率差
  f_value <- log_S_k_minus1 - log_S_k
  return(f_value)
}

## ----eval=TRUE----------------------------------------------------------------
# 定义求解 a 的函数
solve_a <- function(k, lower = 0.01, upper = NULL, tol = 1e-8) {
  # 设置 upper 为 sqrt(k) 的略小值，避免分母为零
  if (is.null(upper)) {
    upper <- sqrt(k) - 0.01
  }
  
  # 检查函数在区间端点的值，以确保可以使用 uniroot
  f_lower <- f_a(lower, k)
  f_upper <- f_a(upper, k)
  
  # 如果函数值为 NA 或者同号，调整区间
  if (is.na(f_lower) || is.na(f_upper) || f_lower * f_upper > 0) {
    warning("无法在指定区间内找到零点，调整区间")
    # 尝试扩大区间
    lower_new <- 0.0001
    upper_new <- sqrt(k) - 0.0001
    f_lower_new <- f_a(lower_new, k)
    f_upper_new <- f_a(upper_new, k)
    if (!is.na(f_lower_new) && !is.na(f_upper_new) && f_lower_new * f_upper_new < 0) {
      lower <- lower_new
      upper <- upper_new
    } else {
      return(NA)
    }
  }
  
  # 使用 uniroot() 求解方程 f(a) = 0
  result <- tryCatch({
    uniroot(f = f_a, interval = c(lower, upper), k = k, tol = tol)$root
  }, error = function(e) {
    NA  # 如果无解，返回 NA
  })
  
  # 返回求解得到的 a
  return(result)
}

## ----eval=TRUE----------------------------------------------------------------
# 定义要计算的 k 值
k_values <- c(4:25, 100, 500, 1000)

# 初始化结果向量
a_values <- numeric(length(k_values))

# 循环计算每个 k 对应的 a
for (i in seq_along(k_values)) {
  k <- k_values[i]
  # 清理不必要的变量
  gc()
  # 求解 a
  a_values[i] <- solve_a(k)
  cat("对于 k =", k, "，求得的 a =", a_values[i], "\n")
}

# 将结果整理为数据框
results <- data.frame(k = k_values, a = a_values)

## ----eval=TRUE----------------------------------------------------------------
# 绘制 a 与 k 的关系图
p1<-ggplot(results, aes(x = k, y = a)) +
  geom_line(color = "blue", linewidth = 0.8) +  # 调节线条宽度
  geom_point(color = "red", size = 2, shape = 21, fill = "white", stroke = 1.2) +  # 增加点的大小，使用白色填充
  labs(
    title = "不同自由度下的 a 值",
    x = "自由度 k",
    y = "参数 a"
  ) +
  theme_minimal(base_size = 14) +  # 设置整体字体大小
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # 标题居中，加粗
    axis.title = element_text(face = "bold", size = 14),  # 坐标轴标题加粗
    axis.text = element_text(color = "black", size = 12),  # 坐标轴刻度文字颜色和大小
    panel.grid.major = element_line(color = "gray80"),  # 设置网格线颜色
    panel.grid.minor = element_blank()  # 隐藏次要网格线
  )

# 打印图像
print(p1)


## ----eval=TRUE----------------------------------------------------------------
# 选择一个 k 值，例如 k = 10
k_example <- 10

# 定义 a 的取值范围
a_seq <- seq(0.01, sqrt(k_example) - 0.01, length.out = 1000)

# 计算对应的 f(a) 值
f_values <- sapply(a_seq, f_a, k = k_example)

# 创建数据框
df_fa <- data.frame(a = a_seq, f_a = f_values)

# 绘制 f(a) 曲线
p2<-ggplot(df_fa, aes(x = a, y = f_a)) +
  geom_line(color = "purple", linewidth = 0.5) +  # 调节线条粗细并调整颜色
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 0.4) +  # 修改水平线样式
  labs(
    title = paste0("目标函数 f(a) 随 a 变化的曲线 (k = ", k_example, ")"),
    x = "a 值", 
    y = expression(f(a))
  ) +
  theme_minimal(base_size = 14) +  # 设置基本字体大小
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # 设置标题大小、粗体、居中
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # 设置x轴标题大小并增加顶部边距
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # 设置y轴标题大小并增加右侧边距
    axis.text = element_text(size = 12),  # 设置轴刻度标签字体大小
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),  # 使用浅灰色网格线
    panel.grid.minor = element_blank()  # 移除次要网格线
  )

# 打印图像
print(p2)


## ----eval=TRUE----------------------------------------------------------------
# 打印关系图
print(p1)

## ----eval=TRUE----------------------------------------------------------------
# 打印K=10时的曲线
print(p2)

## ----eval=TRUE----------------------------------------------------------------
# 定义 a 的取值范围
a_seq <- seq(0.01, sqrt(10) - 0.01, length.out = 1000)

# 计算对应的 c_{k-1} 和 c_k
c_k_minus1_vals <- sqrt(a_seq^2 * (10 - 1) / (10 - a_seq^2))
c_k_vals <- sqrt(a_seq^2 * 10 / (10 + 1 - a_seq^2))

# 计算生存函数值
S_k_minus1 <- pt(c_k_minus1_vals, df = 9, lower.tail = FALSE)
S_k <- pt(c_k_vals, df = 10, lower.tail = FALSE)

# 创建数据框
df_compare <- data.frame(a = a_seq, S_k_minus1 = S_k_minus1, S_k = S_k)

# 绘制曲线

p3<-ggplot(df_compare, aes(x = a)) +
  geom_line(aes(y = S_k_minus1), color = "blue", linewidth = 1, linetype = "dashed") + # 调整线宽和颜色
  geom_line(aes(y = S_k), color = "red", linewidth = 0.5) + # 调整线宽和颜色
  labs(
    title = expression(Scriptstyle("S"["k-1"](a)~和~S["k"](a)~曲线~(k==10))),
    x = "a 值", 
    y = "生存函数值"
  ) +
  theme_minimal(base_size = 14) +  # 设置基本字体大小
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # 居中加粗标题
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # 设置x轴标题大小并增加顶部边距
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # 设置y轴标题大小并增加右侧边距
    axis.text = element_text(size = 12),  # 设置轴刻度标签字体大小
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),  # 使用浅灰色网格线
    panel.grid.minor = element_blank()  # 移除次要网格线
  ) +
  # 添加注释点和文字
  annotate("point", x = 1.625383, y = S_k[which.min(abs(a_seq - 1.625383))],
           color = "green", size = 2) +
  annotate("text", x = 1.65, y = S_k[which.min(abs(a_seq - 1.625383))],
           label = "交点 A(k=10)", color = "green", hjust = 0, size = 3)

# 打印图像
print(p3)

## ----eval=TRUE----------------------------------------------------------------
rm(list = ls())  # 清空环境变量
set.seed(100)    # 设置随机种子，保证结果可重复

## ----eval=TRUE----------------------------------------------------------------
# 观测数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
n <- length(Y)   # 样本容量
tau <- 1         # 删失阈值

# 指示函数：未删失（delta = 1），删失（delta = 0）
delta <- ifelse(Y < tau, 1, 0)

# 分别提取未删失和删失的数据
Y_uncensored <- Y[delta == 1]  # 未删失数据
Y_censored <- Y[delta == 0]    # 删失数据

# 统计量
n_uncensored <- sum(delta)     # 未删失数据数量
n_censored <- n - n_uncensored # 删失数据数量
sum_uncensored <- sum(Y_uncensored)  # 未删失数据总和

## ----eval=TRUE----------------------------------------------------------------
EM_algorithm <- function(Y, delta, tau, tol = 1e-6, max_iter = 1000) {
  # 输入参数：
  # Y：观测数据
  # delta：删失指示变量
  # tau：删失阈值
  # tol：收敛阈值
  # max_iter：最大迭代次数
  
  n <- length(Y)
  n_uncensored <- sum(delta)
  n_censored <- n - n_uncensored
  Y_uncensored <- Y[delta == 1]
  sum_uncensored <- sum(Y_uncensored)
  
  # 初始化lambda，初始值为未删失数据的均值
  lambda_old <- mean(Y_uncensored)
  
  # 保存每次迭代的lambda值
  lambda_values <- numeric()
  lambda_values[1] <- lambda_old
  
  # 迭代过程
  for (iter in 1:max_iter) {
    # E步：计算删失数据的条件期望
    E_T_censored <- tau + lambda_old  # E[T_i | T_i > tau, lambda]
    
    # M步：更新lambda
    lambda_new <- (sum_uncensored + n_censored * E_T_censored) / n
    
    # 保存当前的lambda值
    lambda_values[iter + 1] <- lambda_new
    
    # 检查收敛条件
    if (abs(lambda_new - lambda_old) < tol) {
      cat("算法在第", iter, "次迭代后收敛。\n")
      break
    }
    
    # 更新lambda_old
    lambda_old <- lambda_new
  }
  
  # 如果达到最大迭代次数仍未收敛，给出提示
  if (iter == max_iter) {
    warning("达到最大迭代次数，算法未收敛。")
  }
  
  # 返回结果
  return(list(lambda_est = lambda_new, iterations = iter, lambda_values = lambda_values))
}

## ----eval=TRUE----------------------------------------------------------------
em_result <- EM_algorithm(Y, delta, tau)
print(em_result$lambda_est)

## ----eval=TRUE----------------------------------------------------------------
# MLE估计的lambda值
lambda_MLE <- (sum(delta * Y) + sum((1 - delta) * tau)) / sum(delta)
print(lambda_MLE)

## ----eval=TRUE----------------------------------------------------------------
# 输出EM算法和MLE的估计结果
cat("EM算法估计的lambda：", em_result$lambda_est, "\n")
cat("MLE估计的lambda：", lambda_MLE, "\n")

# 设置颜色和主题风格
library(ggplot2)

# 绘制lambda的收敛曲线，增加ylim范围确保MLE线条可见
plot(0:(em_result$iterations), em_result$lambda_values, type = "o", 
     col = "dodgerblue", pch = 16, lwd = 2,
     xlab = "迭代次数", ylab = expression(lambda), 
     main = "EM算法中lambda的收敛过程",
     ylim = c(min(em_result$lambda_values) - 0.1, max(em_result$lambda_values) + 0.1), 
     cex.lab = 1.2, cex.main = 1.4)

# 添加水平线表示MLE估计值，并设置为明显颜色
abline(h = lambda_MLE, col = "darkred", lty = 2, lwd = 3)

# 添加文本标签指示MLE估计值
text(x = em_result$iterations / 2, y = lambda_MLE, labels = paste("MLE:", round(lambda_MLE, 3)), 
     col = "darkred", pos = 4, cex = 1.1)

# 增加图例到中央
legend("center", inset = -0.1, xpd = TRUE,  # 设置图例在中央
       legend = c("EM算法估计值", "MLE估计值"),
       col = c("dodgerblue", "darkred"), pch = c(16, NA), lty = c(1, 2), lwd = 2,
       bty = "n", cex = 1.1)


# 绘制观测数据的直方图与拟合的指数分布密度函数
hist(Y, breaks = 5, probability = TRUE, xlim = c(0, tau + 2),
     main = "观测数据直方图与拟合的指数分布",
     xlab = "Y", ylab = "概率密度", border = "white", col = "skyblue",
     cex.lab = 1.2, cex.main = 1.4)

# 添加EM算法拟合的指数分布曲线
curve(dexp(x, rate = 1 / em_result$lambda_est), from = 0, to = tau + 2,
      col = "dodgerblue", lwd = 2, add = TRUE)

# 添加MLE拟合的指数分布曲线
curve(dexp(x, rate = 1 / lambda_MLE), from = 0, to = tau + 2,
      col = "tomato", lwd = 2, lty = 2, add = TRUE)

# 添加图例到右上角
legend("topright", legend = c("EM算法拟合", "MLE拟合"),
       col = c("dodgerblue", "tomato"), lty = c(1, 2), lwd = 2,
       bty = "n", cex = 1.1)


## ----eval=TRUE----------------------------------------------------------------
# 清空工作空间，释放内存
rm(list = ls())

# 加载所需的包，忽略包版本警告和加载提示
suppressPackageStartupMessages(suppressWarnings(library(lpSolve))) # 用于线性规划求解
suppressPackageStartupMessages(suppressWarnings(library(plotly)))  # 用于三维绘图

# 检查是否存在命名冲突
conflicts <- conflicts(detail = TRUE)
if (length(conflicts) > 0) {
  print("命名冲突的函数：")
  print(conflicts)
} else {
  print("没有命名冲突。")
}

## ----eval=TRUE----------------------------------------------------------------
# 定义目标函数的系数
objective <- c(4, 2, 9)  # 目标函数：4x + 2y + 9z

# 定义不等式约束的系数矩阵
constraints <- matrix(c(2, 1, 1,   # 2x + y + z
                        1, -1, 3), # x - y + 3z
                      nrow = 2, byrow = TRUE)

# 定义不等式的方向
directions <- c("<=", "<=")

# 定义不等式约束的右端常数
rhs <- c(2, 3)

## ----eval=TRUE----------------------------------------------------------------
# 求解线性规划问题
lp_solution <- lp(direction = "min",          # 求最小值
                  objective.in = objective,   # 目标函数系数
                  const.mat = constraints,    # 约束条件系数矩阵
                  const.dir = directions,     # 约束条件方向
                  const.rhs = rhs,            # 约束条件右端常数
                  all.int = FALSE,            # 决策变量为连续变量
                  all.bin = FALSE)            # 决策变量不是二进制

# 输出结果
cat("最优目标函数值：", lp_solution$objval, "\n")
cat("最优解：\n")
variables <- c("x", "y", "z")
for (i in 1:length(variables)) {
  cat(variables[i], "=", lp_solution$solution[i], "\n")
}

## ----eval=TRUE----------------------------------------------------------------
# 生成数据用于绘制可行域
x_vals <- seq(0, 2, length.out = 50)
y_vals <- seq(0, 2, length.out = 50)
z_vals <- seq(0, 2, length.out = 50)

# 创建可行域的数据框
feasible_points <- expand.grid(x = x_vals, y = y_vals, z = z_vals)
feasible_points <- subset(feasible_points,
                          2 * x + y + z <= 2 &  # 满足第一个约束
                          x - y + 3 * z <= 3 &  # 满足第二个约束
                          x >= 0 & y >= 0 & z >= 0) # 非负约束

## ----eval=TRUE----------------------------------------------------------------
# 绘制三维散点图
fig <- plot_ly(data = feasible_points, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers",
               marker = list(size = 1, color = 'blue')) %>%
  add_markers(x = lp_solution$solution[1], y = lp_solution$solution[2], z = lp_solution$solution[3],
              marker = list(size = 5, color = 'red')) %>%
  layout(title = list(text = "线性规划可行域与最优解", y = 0.9), # 将标题位置下移
         scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'z')))
# 显示图形
fig

## ----eval=TRUE----------------------------------------------------------------
# 显示图形
fig

## ----eval=TRUE----------------------------------------------------------------
# 清理工作空间
rm(list = ls())

# 加载必要的数据集和包
data(mtcars)
library(ggplot2)
library(gridExtra)
library(grid) # 加载 grid 包以使用 textGrob

# 定义公式列表
formulas <- list(
  mpg ~ disp,                        # 公式1：mpg 与 disp 的线性关系
  mpg ~ I(1 / disp),                 # 公式2：mpg 与 disp 的倒数的线性关系
  mpg ~ disp + wt,                   # 公式3：mpg 与 disp 和 wt 的线性关系
  mpg ~ I(1 / disp) + wt             # 公式4：mpg 与 disp 的倒数和 wt 的线性关系
)

# 方法一：使用 for 循环
# 初始化一个空列表用于存储模型
models_for <- list()

# 遍历公式列表，拟合模型
for (i in seq_along(formulas)) {
  # 提取当前公式
  formula <- formulas[[i]]
  
  # 拟合线性模型
  model <- lm(formula, data = mtcars)
  
  # 将模型存入列表
  models_for[[i]] <- model
}

# 方法二：使用 lapply() 函数
# 定义模型拟合函数
fit_lm <- function(formula) {
  lm(formula, data = mtcars)
}

# 使用 lapply() 对公式列表进行模型拟合
models_lapply <- lapply(formulas, fit_lm)

# 比较两种方法的结果是否一致
all.equal(models_for, models_lapply)  # 返回 TRUE，表示一致

# 创建绘图列表
plot_list <- list()

for (i in seq_along(models_lapply)) {
  # 提取模型和公式
  model <- models_lapply[[i]]
  formula <- formulas[[i]]
  
  # 提取响应变量和预测值
  actual <- mtcars$mpg
  predicted <- model$fitted.values
  r_squared <- summary(model)$r.squared
  
  # 创建数据框
  plot_data <- data.frame(Actual = actual, Predicted = predicted)
  
  # 创建 ggplot 图
  p <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
    geom_point(color = "dodgerblue", size = 1.2, alpha = 0.5) +  # 点大小和透明度设置
    geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed", linewidth = 0.8) +  # y=x 参考线
    theme_minimal(base_size = 12) +  # 基础字体
    labs(
      title = paste("模型", i, "：实际值 vs 预测值"),
      subtitle = paste("R² =", round(r_squared, 3)),
      x = "实际值 (mpg)",
      y = "预测值 (mpg)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90"),  # 增强网格对比
      panel.grid.minor = element_blank()
    ) +
    coord_equal()  # 保持比例一致
  
  # 将图存入列表
  plot_list[[i]] <- p
}

# 绘制所有图，主标题放大字体
grid.arrange(
  grobs = plot_list,
  ncol = 2,
  top = textGrob("模型比较：实际值与预测值", gp = gpar(fontsize = 14, fontface = "bold"))
)


## ----eval=TRUE----------------------------------------------------------------
# 清理工作空间，释放内存
rm(list = ls())

# 加载必要的数据集
data(mtcars)

# 生成自举样本列表
set.seed(123)  # 设置随机种子，保证结果可重复
bootstraps <- lapply(1:10, function(i) {
  # 随机有放回地抽取行索引
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  # 根据抽取的行索引创建自举样本
  mtcars[rows, ]
})

# 定义模型拟合函数（不使用匿名函数）
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 方法一：使用 for 循环
# 初始化一个空列表用于存储模型
models_for <- list()

# 遍历自举样本列表，拟合模型
for (i in seq_along(bootstraps)) {
  # 提取当前自举样本
  bootstrap_sample <- bootstraps[[i]]
  
  # 拟合线性模型
  model <- fit_model(bootstrap_sample)
  
  # 将模型存入列表
  models_for[[i]] <- model
}

# 方法二：使用 lapply()，不使用匿名函数
models_lapply <- lapply(bootstraps, fit_model)

# 比较两种方法的结果是否一致
all.equal(models_for, models_lapply)  # 返回 TRUE，表示一致

# 可视化：绘制模型系数的分布
coefficients <- sapply(models_for, function(model) coef(model)[2])  # 提取 disp 的系数

# 绘制系数的箱线图
boxplot(coefficients,
        main = "自举样本中 disp 系数的分布",
        ylab = "系数值")

## ----eval=TRUE----------------------------------------------------------------
# 清理工作空间，释放内存
rm(list = ls())

# 加载必要的数据集
data(mtcars)

# 定义提取 R^2 值的函数
rsq <- function(mod) summary(mod)$r.squared

# 练习3：提取模型的 R^2 值
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用 lapply() 拟合模型
fit_lm <- function(formula) {
  lm(formula, data = mtcars)
}
models_3 <- lapply(formulas, fit_lm)

# 提取 R^2 值
rsq_values_3 <- sapply(models_3, rsq)

# 创建结果数据框
results_3 <- data.frame(
  Model = paste("模型", 1:4),
  Formula = sapply(formulas, function(f) deparse(f)),
  R_squared = rsq_values_3
)

# 打印结果
print("练习3的模型 R^2 值：")
print(results_3)

# 练习4：提取自举样本模型的 R^2 值
# 生成自举样本列表
set.seed(123)
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  mtcars[rows, ]
})

# 定义模型拟合函数
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 拟合模型
models_4 <- lapply(bootstraps, fit_model)

# 提取 R^2 值
rsq_values_4 <- sapply(models_4, rsq)

# 创建结果数据框
results_4 <- data.frame(
  Bootstrap_Sample = paste("样本", 1:10),
  R_squared = rsq_values_4
)

# 打印结果
print("练习4的自举样本模型 R^2 值：")
print(results_4)

# 可视化：绘制 R^2 值的柱状图
par(mfrow = c(1, 2))  # 将图形设备分为 1x2 网格

# 练习3的 R^2 值柱状图
barplot(rsq_values_3,
        names.arg = paste("模型", 1:4),
        main = "练习3模型的 R^2 值",
        ylab = "R^2 值",
        col = "skyblue")

# 练习4的 R^2 值柱状图
barplot(rsq_values_4,
        names.arg = paste("样本", 1:10),
        main = "练习4自举样本模型的 R^2 值",
        ylab = "R^2 值",
        col = "lightgreen",
        las = 2)  # 使 x 轴标签垂直显示

# 重置图形参数
par(mfrow = c(1, 1))

## ----eval=TRUE----------------------------------------------------------------
# 清理工作空间，释放内存
rm(list = ls())

# 设置随机数种子，确保结果可重复
set.seed(125)

# 模拟 t 检验的结果
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 方法一：使用 sapply() 和匿名函数提取 p 值
p_values_anonymous <- sapply(trials, function(test) {
  # 从 t 检验结果中提取 p 值
  test$p.value
})

# 方法二：直接使用 [[ 提取 p 值，消除匿名函数
p_values_direct <- sapply(trials, `[[`, "p.value")

# 验证两种方法的结果是否一致
all.equal(p_values_anonymous, p_values_direct)  # 返回 TRUE，表示一致

# 可视化结果：绘制 p 值的直方图
hist(p_values_direct,
     breaks = 20,
     main = "t 检验的 p 值分布",
     xlab = "p 值",
     col = "lightblue",
     border = "white")

# 添加显著性水平的参考线（例如 0.05）
abline(v = 0.05, col = "red", lwd = 2, lty = 2)

## ----eval=TRUE----------------------------------------------------------------
# 定义新的函数 my_lapply_parallel
my_lapply_parallel <- function(FUN, ..., FUN.VALUE) {
  # 将所有输入列表组合成一个列表
  inputs <- list(...)
  
  # 检查所有输入列表的长度是否一致
  lengths <- sapply(inputs, length)
  if (!all(lengths == lengths[1])) {
    stop("所有输入列表的长度必须一致！")
  }
  
  # 使用 Map() 并行遍历所有输入列表，应用函数 FUN
  results_list <- Map(FUN, ...)
  
  # 使用 vapply() 将结果转换为指定类型的向量或矩阵
  results <- vapply(results_list, identity, FUN.VALUE = FUN.VALUE)
  
  return(results)
}

# 示例：并行遍历两个向量，计算它们的乘积，结果为数值向量
vec1 <- 1:5
vec2 <- 6:10

# 定义要应用的函数
multiply <- function(x, y) {
  x * y
}

# 指定输出的类型和长度
fun_value <- numeric(1)

# 使用自定义的 my_lapply_parallel 函数
result <- my_lapply_parallel(multiply, vec1, vec2, FUN.VALUE = fun_value)

# 打印结果
print("两个向量对应元素的乘积：")
print(result)

# 可视化结果：绘制结果的柱状图
barplot(result,
        names.arg = paste(vec1, "*", vec2),
        main = "两个向量元素乘积的柱状图",
        xlab = "元素对",
        ylab = "乘积",
        col = "orange")

## ----eval=TRUE----------------------------------------------------------------
# 可视化结果：绘制 p 值的直方图
hist(p_values_direct,
     breaks = 20,
     main = "t 检验的 p 值分布",
     xlab = "p 值",
     col = "lightblue",
     border = "white")

# 添加显著性水平的参考线（例如 0.05）
abline(v = 0.05, col = "red", lwd = 2, lty = 2)

## ----eval=TRUE----------------------------------------------------------------
# 可视化结果：绘制结果的柱状图
barplot(result,
        names.arg = paste(vec1, "*", vec2),
        main = "两个向量元素乘积的柱状图",
        xlab = "元素对",
        ylab = "乘积",
        col = "orange")

## ----eval=TRUE----------------------------------------------------------------
# 清理工作空间，释放内存
rm(list = ls())

# 定义优化的卡方检验函数
fast_chisq_test <- function(x, y) {
  # 将数值向量转换为因子
  x_factor <- as.factor(x)
  y_factor <- as.factor(y)
  
  # 构建观察频数矩阵
  observed <- table(x_factor, y_factor)
  
  # 计算行和、列和、总和
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  total <- sum(observed)
  
  # 计算期望频数矩阵
  expected <- outer(row_totals, col_totals) / total
  
  # 检查期望频数中是否存在零，避免除零错误
  if (any(expected == 0)) {
    stop("期望频数中存在零，无法计算卡方统计量。")
  }
  
  # 计算卡方统计量
  chi_square <- sum((observed - expected)^2 / expected)
  
  # 计算自由度
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  
  # 计算 p 值
  p_value <- pchisq(chi_square, df, lower.tail = FALSE)
  
  # 返回结果
  result <- list(
    statistic = chi_square,
    parameter = df,
    p.value = p_value,
    observed = observed,
    expected = expected,
    method = "Pearson's Chi-squared test (快速版)",
    data.name = paste(deparse(substitute(x)), "和", deparse(substitute(y)))
  )
  
  class(result) <- "htest"
  return(result)
}

# 示例数据
set.seed(130)
x <- sample(1:5, 1000, replace = TRUE)
y <- sample(1:4, 1000, replace = TRUE)

# 使用优化的卡方检验函数
result_fast <- fast_chisq_test(x, y)

# 使用内置的 chisq.test() 函数进行比较
result_builtin <- chisq.test(x, y, correct = FALSE)

# 将结果整理为数据框
comparison <- data.frame(
  方法 = c("自定义函数", "内置函数"),
  卡方统计量 = c(result_fast$statistic, result_builtin$statistic),
  自由度 = c(result_fast$parameter, result_builtin$parameter),
  p值 = c(result_fast$p.value, result_builtin$p.value)
)

# 输出结果表格
print("卡方检验结果比较：")
print(comparison)

# 可视化：绘制观察频数矩阵的热图
library(ggplot2)
library(reshape2)

# 转换观察频数矩阵为数据框
observed_df <- as.data.frame(as.table(result_fast$observed))
colnames(observed_df) <- c("X", "Y", "Frequency")

# 绘制热图
heatmap<-ggplot(observed_df, aes(x = X, y = Y, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "观察频数矩阵热图", x = "X 分类", y = "Y 分类") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # 调整字体样式
  )

# 显示图像
print(heatmap)

## ----eval=TRUE----------------------------------------------------------------
# 定义优化的 table 函数
fast_table <- function(x, y) {
  # 检查输入是否为整数向量
  if (!is.integer(x) || !is.integer(y)) {
    stop("输入的 x 和 y 必须是整数向量。")
  }

  # 获取 x 和 y 的取值范围
  x_levels <- sort(unique(x))
  y_levels <- sort(unique(y))

  # 将 x_levels 和 y_levels 转换为字符类型
  x_levels_char <- as.character(x_levels)
  y_levels_char <- as.character(y_levels)

  # 创建频数矩阵，初始化为零，并设置维度名称
  freq_matrix <- matrix(0L, nrow = length(x_levels), ncol = length(y_levels),
                        dimnames = list(x_levels_char, y_levels_char))

  # 为 dimnames 设置名称，与 table() 函数一致
  names(dimnames(freq_matrix)) <- c(deparse(substitute(x)), deparse(substitute(y)))

  # 创建值到索引的映射
  x_index <- match(x, x_levels)
  y_index <- match(y, y_levels)

  # 计算频数
  for (i in seq_along(x)) {
    freq_matrix[x_index[i], y_index[i]] <- freq_matrix[x_index[i], y_index[i]] + 1L
  }

  # 将矩阵转换为 table 对象，保持与 table() 函数输出一致
  freq_table <- as.table(freq_matrix)

  return(freq_table)
}

# 示例数据
set.seed(130)
x <- sample(1L:5L, 1000, replace = TRUE)
y <- sample(1L:4L, 1000, replace = TRUE)

# 使用优化的 table 函数
freq_table_fast <- fast_table(x, y)

# 使用内置的 table 函数进行比较
freq_table_builtin <- table(x, y)

# 检查结果是否一致
comparison_result <- all.equal(freq_table_fast, freq_table_builtin)
print(comparison_result)  # 应该返回 TRUE

# 利用优化的 table 函数加速卡方检验
fast_chisq_test_v2 <- function(x, y) {
  # 检查输入是否为整数向量
  if (!is.integer(x) || !is.integer(y)) {
    stop("输入的 x 和 y 必须是整数向量。")
  }

  # 使用优化的 table 函数构建观察频数矩阵
  observed <- fast_table(x, y)

  # 计算行和、列和、总和
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  total <- sum(observed)

  # 计算期望频数矩阵
  expected <- outer(row_totals, col_totals) / total

  # 检查期望频数中是否存在零，避免除零错误
  if (any(expected == 0)) {
    stop("期望频数中存在零，无法计算卡方统计量。")
  }

  # 计算卡方统计量
  chi_square <- sum((observed - expected)^2 / expected)

  # 计算自由度
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)

  # 计算 p 值
  p_value <- pchisq(chi_square, df, lower.tail = FALSE)

  # 返回结果
  result <- list(
    statistic = chi_square,
    parameter = df,
    p.value = p_value,
    observed = observed,
    expected = expected,
    method = "Pearson's Chi-squared test with fast_table",
    data.name = paste(deparse(substitute(x)), "和", deparse(substitute(y)))
  )

  class(result) <- "htest"
  return(result)
}

# 使用优化的卡方检验函数
result_fast_v2 <- fast_chisq_test_v2(x, y)

# 使用内置的 chisq.test() 函数进行比较
result_builtin <- chisq.test(x, y, correct = FALSE)

# 将结果整理为数据框
comparison_v2 <- data.frame(
  方法 = c("自定义函数（fast_table）", "内置函数"),
  卡方统计量 = c(unname(result_fast_v2$statistic), unname(result_builtin$statistic)),
  自由度 = c(result_fast_v2$parameter, result_builtin$parameter),
  p值 = c(result_fast_v2$p.value, result_builtin$p.value)
)

# 输出结果表格
print("利用 fast_table 的卡方检验结果比较：")
print(comparison_v2)



## ----eval=TRUE----------------------------------------------------------------
print(heatmap)

## ----eval=TRUE----------------------------------------------------------------
# 清空工作空间，释放内存
rm(list = ls())
# 抑制版本不兼容造成的警告并加载 Rcpp 和 microbenchmark 包
suppressWarnings({
  library(Rcpp)
  library(microbenchmark)
})

## ----eval=TRUE----------------------------------------------------------------
# 纯 R 实现的 Gibbs 采样器函数
gibbs_sampler_R <- function(N, n, a, b) {
  # 初始化存储向量
  x <- integer(N)  # 用于存储 x 的值
  y <- numeric(N)  # 用于存储 y 的值
  
  # 初始值
  x[1] <- floor(n / 2)  # 将 x[1] 初始化为 n 的一半
  y[1] <- 0.5           # 将 y[1] 初始化为 0.5
  
  # Gibbs 采样迭代
  for (i in 2:N) {
    # 给定 y[i - 1]，从 Binomial(n, y[i - 1]) 中抽样得到 x[i]
    x[i] <- rbinom(1, size = n, prob = y[i - 1])
    
    # 给定 x[i]，从 Beta(x[i] + a, n - x[i] + b) 中抽样得到 y[i]
    y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
  }
  
  # 返回结果列表
  return(list(x = x, y = y))
}

## ----eval=TRUE----------------------------------------------------------------
# 使用 Rcpp 编写 Gibbs 采样器函数
cppFunction('
List gibbs_sampler_Rcpp(int N, int n, double a, double b) {
  // 引入必要的头文件
  #include <Rcpp.h>
  using namespace Rcpp;
  
  // 初始化存储向量
  IntegerVector x(N);  // 用于存储 x 的值
  NumericVector y(N);  // 用于存储 y 的值
  
  // 初始值
  x[0] = n / 2;  // 将 x[0] 初始化为 n 的一半
  y[0] = 0.5;    // 将 y[0] 初始化为 0.5
  
  // Gibbs 采样迭代
  for (int i = 1; i < N; ++i) {
    // 给定 y[i - 1]，从 Binomial(n, y[i - 1]) 中抽样得到 x[i]
    x[i] = R::rbinom(n, y[i - 1]);
    
    // 给定 x[i]，从 Beta(x[i] + a, n - x[i] + b) 中抽样得到 y[i]
    y[i] = R::rbeta(x[i] + a, n - x[i] + b);
  }
  
  // 返回结果列表
  return List::create(Named("x") = x, Named("y") = y);
}
')

## ----eval=TRUE----------------------------------------------------------------
# 设置参数
N <- 10000  # 采样次数
n <- 20     # Binomial 分布的参数
a <- 2      # Beta 分布的参数
b <- 2      # Beta 分布的参数

# 设置随机种子以确保可重复性
set.seed(100)

# 运行纯 R 实现的 Gibbs 采样器
result_R <- gibbs_sampler_R(N, n, a, b)

# 运行 Rcpp 实现的 Gibbs 采样器
result_Rcpp <- gibbs_sampler_Rcpp(N, n, a, b)

## ----eval=TRUE----------------------------------------------------------------
# 比较 x 的分布
qqplot(result_R$x, result_Rcpp$x,
       main = "QQ 图比较 x (R 实现 vs Rcpp 实现)",
       xlab = "R 实现的 x",
       ylab = "Rcpp 实现的 x",
       pch = 19,          # 使用实心圆点
       col = rgb(0, 0, 1, 0.5),  # 半透明蓝色
       cex = 0.6          # 调整点的大小
)
abline(0, 1, col = "red", lwd = 2)  # 绘制 y = x 的参考线，增加线宽
grid()  # 添加网格线

# 比较 y 的分布
qqplot(result_R$y, result_Rcpp$y,
       main = "QQ 图比较 y (R 实现 vs Rcpp 实现)",
       xlab = "R 实现的 y",
       ylab = "Rcpp 实现的 y",
       pch = 19,
       col = rgb(1, 0, 0, 0.5),  # 半透明红色
       cex = 0.6
)
abline(0, 1, col = "blue", lwd = 2)
grid()

## ----eval=TRUE----------------------------------------------------------------
# 定义测试次数
benchmark_times <- 100

# 比较计算时间
benchmark_result <- microbenchmark(
  gibbs_R = gibbs_sampler_R(N, n, a, b),
  gibbs_Rcpp = gibbs_sampler_Rcpp(N, n, a, b),
  times = benchmark_times
)

# 打印 benchmark 结果
print(benchmark_result)

# 绘制计算时间的箱线图
boxplot(benchmark_result, main = "Gibbs 采样器计算时间比较", names = c("R 实现", "Rcpp 实现"), ylab = "时间（毫秒）")

## ----eval=TRUE----------------------------------------------------------------
# 设置图形布局为 2x2
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # 设置边距

# 绘制 R 实现的 x 分布
hist(result_R$x, 
     breaks = n + 1, 
     main = "R 实现的 x 分布", 
     xlab = "x", 
     col = rgb(0.2, 0.4, 0.6, 0.7),  # 半透明蓝色
     border = "black", 
     probability = TRUE)
box()  # 添加边框

# 绘制 Rcpp 实现的 x 分布
hist(result_Rcpp$x, 
     breaks = n + 1, 
     main = "Rcpp 实现的 x 分布", 
     xlab = "x", 
     col = rgb(0.8, 0.2, 0.2, 0.7),  # 半透明红色
     border = "black", 
     probability = TRUE)
box()

# 绘制 R 实现的 y 分布
hist(result_R$y, 
     breaks = 50, 
     main = "R 实现的 y 分布", 
     xlab = "y", 
     col = rgb(0.2, 0.6, 0.4, 0.7),  # 半透明绿色
     border = "black", 
     probability = TRUE)
box()

# 绘制 Rcpp 实现的 y 分布
hist(result_Rcpp$y, 
     breaks = 50, 
     main = "Rcpp 实现的 y 分布", 
     xlab = "y", 
     col = rgb(0.6, 0.3, 0.8, 0.7),  # 半透明紫色
     border = "black", 
     probability = TRUE)
box()

## ----eval=TRUE----------------------------------------------------------------
print(benchmark_result)

## ----eval=TRUE----------------------------------------------------------------
# 计算 x 的自相关函数
acf(result_R$x, main = "x 的自相关函数（R 实现）")
acf(result_Rcpp$x, main = "x 的自相关函数（Rcpp 实现）")

# 计算 y 的自相关函数
acf(result_R$y, main = "y 的自相关函数（R 实现）")
acf(result_Rcpp$y, main = "y 的自相关函数（Rcpp 实现）")

