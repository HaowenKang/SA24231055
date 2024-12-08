#include <Rcpp.h>
using namespace Rcpp;

//' @title K-Means++ 聚类算法（Rcpp 版本）
//' @description 使用 Rcpp 实现的 K-Means++ 聚类算法
//' @param X 数据矩阵，每行是一个样本
//' @param k 聚类的类别数
//' @param max_iter 最大迭代次数，默认为 100
//' @return 一个列表，包含聚类结果和聚类中心
//' @examples
//' \dontrun{
//' data(ori_data_x)
//' result <- kmeansppCpp(as.matrix(ori_data_x), k = 3)
//' }
//' @export
// [[Rcpp::export]]
List kmeansppCpp(NumericMatrix X, int k, int max_iter = 100) {
  int n_samples = X.nrow();
  int n_features = X.ncol();

  if (k <= 0 || k > n_samples) {
    stop("聚类数 k 必须大于 0 且小于等于样本数");
  }

  // 初始化聚类中心
  NumericMatrix centers(k, n_features);
  IntegerVector idx = seq(0, n_samples - 1);
  
  // 第一个中心随机选择
  centers(0, _) = X(Rcpp::sample(idx, 1, false)[0], _);

  // K-Means++ 初始化剩余的中心
  for (int i = 1; i < k; i++) {
    NumericVector distances(n_samples);
    for (int j = 0; j < n_samples; j++) {
      double min_dist = R_PosInf;
      for (int c = 0; c < i; c++) {
        double dist = 0.0;
        for (int l = 0; l < n_features; l++) {
          double diff = X(j, l) - centers(c, l);
          dist += diff * diff;
        }
        if (dist < min_dist) {
          min_dist = dist;
        }
      }
      distances[j] = min_dist;
    }
    // 计算选择下一个中心的概率分布
    NumericVector probs = distances / sum(distances);
    centers(i, _) = X(Rcpp::sample(idx, 1, false, probs)[0], _);
  }

  // K-Means 聚类过程
  IntegerVector cluster_assignments(n_samples, -1);
  IntegerVector new_assignments(n_samples);
  NumericMatrix distance_matrix(n_samples, k);

  for (int iter = 0; iter < max_iter; iter++) {
    // 计算每个样本到各个中心的距离
    for (int i = 0; i < n_samples; i++) {
      for (int j = 0; j < k; j++) {
        double dist = 0.0;
        for (int l = 0; l < n_features; l++) {
          double diff = X(i, l) - centers(j, l);
          dist += diff * diff;
        }
        distance_matrix(i, j) = sqrt(dist);
      }
    }

    // 分配样本到最近的中心
    for (int i = 0; i < n_samples; i++) {
      NumericVector dists = distance_matrix(i, _);
      new_assignments[i] = which_min(dists);
    }

    // 检查是否收敛
    if (iter > 0 && is_true(all(cluster_assignments == new_assignments))) {
      break;
    }
    cluster_assignments = clone(new_assignments);

    // 更新聚类中心
    for (int j = 0; j < k; j++) {
      NumericVector sum(n_features);
      int count = 0;
      for (int i = 0; i < n_samples; i++) {
        if (cluster_assignments[i] == j) {
          sum += X(i, _);
          count++;
        }
      }
      if (count > 0) {
        centers(j, _) = sum / count;
      } else {
        // 如果某个聚类没有样本，随机重新初始化中心
        centers(j, _) = X(Rcpp::sample(idx, 1, false)[0], _);
      }
    }
  }

  // 返回结果，注意 R 的索引从 1 开始
  return List::create(
    Named("clusters") = cluster_assignments + 1,
    Named("centers") = centers
  );
}
