// spectralClustering.cpp
#include <Rcpp.h>
using namespace Rcpp;

//' @title 谱聚类算法（Rcpp 版本）
//' @description 使用 Rcpp 实现的谱聚类算法，将数据映射到低维空间后进行聚类。
//' @param X 数据矩阵，每行是一个样本。
//' @param k 聚类的类别数。
//' @return 一个整数向量，表示每个样本的聚类标签。
//' @export
// [[Rcpp::export]]
 IntegerVector spectralClusteringRcpp(NumericMatrix X, int k) {
   
   int n = X.nrow(); // 样本数量
   int d = X.ncol(); // 特征数量
   
   // 1. 数据归一化处理，将每个特征缩放到 [0, 1]
   NumericMatrix data_norm(n, d);
   
   for(int j = 0; j < d; j++) {
     double min_val = X(0, j);
     double max_val = X(0, j);
     // 找到第 j 列的最小值和最大值
     for(int i = 1; i < n; i++) {
       if(X(i, j) < min_val) min_val = X(i, j);
       if(X(i, j) > max_val) max_val = X(i, j);
     }
     double range = max_val - min_val;
     // 缩放到 [0, 1]
     for(int i = 0; i < n; i++) {
       if(range != 0) {
         data_norm(i, j) = (X(i, j) - min_val) / range;
       } else {
         data_norm(i, j) = 0.0; // 如果范围为0，设为0
       }
     }
   }
   
   // 2. 计算相似度矩阵 W，W[i,j] = exp(-||x_i -x_j||^2)
   NumericMatrix W(n, n);
   
   for(int i = 0; i < n; i++) {
     W(i, i) = 1.0; // 自己与自己的相似度设为1
     for(int j = i + 1; j < n; j++) {
       double dist_sq = 0.0;
       for(int l = 0; l < d; l++) {
         double diff = data_norm(i, l) - data_norm(j, l);
         dist_sq += diff * diff;
       }
       double sim = exp(-dist_sq);
       W(i, j) = sim;
       W(j, i) = sim; // 对称矩阵
     }
   }
   
   // 3. 构建拉普拉斯矩阵 L = D - W
   NumericVector degrees(n);
   for(int i = 0; i < n; i++) {
     for(int j = 0; j < n; j++) {
       degrees[i] += W(i, j);
     }
   }
   
   // 构建拉普拉斯矩阵 L
   NumericMatrix L(n, n);
   for(int i = 0; i < n; i++) {
     L(i, i) = degrees[i] - W(i, i);
     for(int j = 0; j < n; j++) {
       if(i != j) {
         L(i, j) = -W(i, j);
       }
     }
   }
   
   // 4. 计算特征值和特征向量
   // 使用 R 的 eigen 函数
   Function eigen_func("eigen");
   List eigen_result = eigen_func(L, Named("symmetric") = true);
   NumericVector eigen_values = eigen_result["values"];
   NumericMatrix eigen_vectors = eigen_result["vectors"];
   
   // 5. 取最小的 k 个特征值对应的特征向量
   // 因为 eigen 函数返回特征值按降序排列，需要找到最小的 k 个
   NumericVector sorted_eigen_values = clone(eigen_values);
   
   // 定义 R 的 order 函数
   Function order_func("order");
   
   // 调用 R 的 order 函数，获取升序排序的索引
   IntegerVector order_idx = order_func(sorted_eigen_values, Named("decreasing") = false);
   
   // 选择前 k 个最小特征值对应的特征向量
   NumericMatrix U(n, k);
   for(int i = 0; i < k; i++) {
     int idx = order_idx[i] - 1; // R 是1-based
     for(int j = 0; j < n; j++) {
       U(j, i) = eigen_vectors(j, idx);
     }
   }
   
   // 6. 定义聚类中心为前 k 个特征向量的行（每个聚类中心是一个 k 维向量）
   // 由于 U 是 n x k 矩阵，每行是一个样本的 k 维特征向量
   // 但在 R 代码中，聚类中心是前 k 个特征向量的行，这在 Rcpp 中需要重新组织
   // 这里，我们假设聚类中心为特征矩阵 U 的前 k 行
   NumericMatrix centroids(k, k);
   for(int i = 0; i < k; i++) {
     for(int j = 0; j < k; j++) {
       centroids(i, j) = U(i, j);
     }
   }
   
   // 7. 对每个样本分配标签，基于与每个中心的距离
   IntegerVector labels(n);
   
   for(int i = 0; i < n; i++) {
     double min_dist = R_PosInf;
     int min_cluster = 1;
     for(int j = 0; j < k; j++) {
       double dist_sq = 0.0;
       for(int l = 0; l < k; l++) {
         double diff = centroids(j, l) - U(i, l);
         dist_sq += diff * diff;
       }
       if(dist_sq < min_dist) {
         min_dist = dist_sq;
         min_cluster = j + 1; // 1-based cluster labels
       }
     }
     labels[i] = min_cluster;
   }
   
   return labels;
 }

