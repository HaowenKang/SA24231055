#include <Rcpp.h>
using namespace Rcpp;

//' @title 信息增益计算（Rcpp 版本）
//' @description 使用 Rcpp 实现的信息增益计算函数
//' @param data 数据框，包含特征和标签
//' @param feature 要计算的信息增益的特征名称
//' @return 信息增益值
//' @examples
//' \dontrun{
//' data(filmtrain)
//' gain <- informationGainCpp(filmtrain, "V1")
//' }
//' @export
// [[Rcpp::export]]
double informationGainCpp(DataFrame data, String feature) {
  // 获取标签列，假设标签在数据框的最后一列
  CharacterVector labels = data[data.size() - 1];

  // 获取指定的特征列
  IntegerVector feature_values = data[feature];

  int n = labels.size();  // 样本总数

  // 计算总熵
  std::map<std::string, int> label_counts;
  for (int i = 0; i < n; i++) {
    // 将 labels[i] 显式转换为 std::string
    std::string label = Rcpp::as<std::string>(labels[i]);
    label_counts[label] += 1;
  }

  double total_entropy = 0.0;
  for (auto it = label_counts.begin(); it != label_counts.end(); ++it) {
    double p = static_cast<double>(it->second) / n;
    total_entropy -= p * std::log2(p);
  }

  // 计算条件熵
  std::map<int, std::map<std::string, int>> feature_label_counts;
  std::map<int, int> feature_counts;
  for (int i = 0; i < n; i++) {
    int feature_value = feature_values[i];
    std::string label = Rcpp::as<std::string>(labels[i]);
    feature_label_counts[feature_value][label] += 1;
    feature_counts[feature_value] += 1;
  }

  double feature_entropy = 0.0;
  for (auto it = feature_label_counts.begin(); it != feature_label_counts.end(); ++it) {
    int feature_value_count = feature_counts[it->first];
    double sub_entropy = 0.0;
    for (auto jt = it->second.begin(); jt != it->second.end(); ++jt) {
      double p = static_cast<double>(jt->second) / feature_value_count;
      sub_entropy -= p * std::log2(p);
    }
    feature_entropy += (static_cast<double>(feature_value_count) / n) * sub_entropy;
  }

  double gain = total_entropy - feature_entropy;
  return gain;
}
