#include <Rcpp.h>
using namespace Rcpp;

//' @title 朴素贝叶斯分类算法（Rcpp 版本）
//' @description 使用 Rcpp 实现的朴素贝叶斯分类算法
//' @param data 训练数据集，包括特征和标签
//' @return 训练好的朴素贝叶斯模型
//' @examples
//' \dontrun{
//' data(filmtrain)
//' model <- naiveBayesCpp(as.matrix(filmtrain))
//' }
//' @export
// [[Rcpp::export]]
List naiveBayesCpp(NumericMatrix data) {
  int n_samples = data.nrow();
  int n_features = data.ncol() - 1;
  NumericVector y = data(_, n_features);
  NumericVector labels = unique(y);
  
  // 计算先验概率
  std::map<double, double> prior;
  for (double label : labels) {
    int count = 0;
    for (int i = 0; i < n_samples; i++) {
      if (y[i] == label) {
        count++;
      }
    }
    prior[label] = (double)count / n_samples;
  }
  
  // 计算条件概率
  List likelihood;
  for (double label : labels) {
    std::vector<int> indices;
    for (int i = 0; i < n_samples; i++) {
      if (y[i] == label) {
        indices.push_back(i);
      }
    }
    List feature_likelihood;
    for (int j = 0; j < n_features; j++) {
      std::map<double, double> counts;
      for (int idx : indices) {
        double value = data(idx, j);
        counts[value] += 1.0;
      }
      // 归一化
      for (auto& kv : counts) {
        kv.second /= indices.size();
      }
      feature_likelihood.push_back(counts);
    }
    likelihood.push_back(feature_likelihood);
  }
  
  return List::create(Named("prior") = prior, Named("likelihood") = likelihood, Named("labels") = labels);
}
