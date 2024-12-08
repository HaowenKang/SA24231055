#include <Rcpp.h>
using namespace Rcpp;

//' @title K-Means 聚类算法（Rcpp 版本）
//' @description 使用 Rcpp 实现的 K-Means 聚类算法
//' @param X 数据矩阵，每行是一个样本
//' @param k 聚类的类别数
//' @param max_iter 最大迭代次数，默认为 100
//' @return 一个列表，包含聚类结果和聚类中心
//' @examples
//' \dontrun{
//' data(ori_data_x)
//' result <- kmeansCpp(as.matrix(ori_data_x), k = 3)
//' }
//' @export
// [[Rcpp::export]]
List kmeansCpp(NumericMatrix X, int k, int max_iter = 100) {
  int n_samples = X.nrow();
  int n_features = X.ncol();

  if (k <= 0 || k > n_samples) {
    stop("聚类数 k 必须大于 0 且小于等于样本数");
  }

  // 初始化聚类中心
  NumericMatrix centers(k, n_features);
  IntegerVector indices = sample(n_samples, k) - 1; // R 索引从 1 开始，C++ 从 0 开始
  for (int i = 0; i < k; i++) {
    for (int j = 0; j < n_features; j++) {
      centers(i, j) = X(indices[i], j);
    }
  }

  IntegerVector cluster_assignments(n_samples);
  IntegerVector new_assignments(n_samples);
  NumericMatrix distances(n_samples, k);

  for (int iter = 0; iter < max_iter; iter++) {
    // 计算距离
    for (int i = 0; i < n_samples; i++) {
      for (int j = 0; j < k; j++) {
        double dist = 0.0;
        for (int l = 0; l < n_features; l++) {
          dist += pow(X(i, l) - centers(j, l), 2);
        }
        distances(i, j) = sqrt(dist);
      }
    }

    // 分配样本到最近的中心
    for (int i = 0; i < n_samples; i++) {
      NumericVector dists = distances(i, _);
      new_assignments[i] = which_min(dists);
    }

    // 检查是否收敛
    if (is_true(all(cluster_assignments == new_assignments))) {
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
      }
    }
  }

  return List::create(Named("clusters") = cluster_assignments + 1, // 调整为 R 的索引
                      Named("centers") = centers);
}
