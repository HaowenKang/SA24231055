#include <Rcpp.h>
using namespace Rcpp;

//' @title Softmax 多分类算法（Rcpp 版本）
//' @description 使用 Rcpp 实现的 Softmax 多分类算法，并计算测试集的准确率
//' @param X 训练数据的特征矩阵（不包括标签列）
//' @param y 训练数据的标签向量
//' @param X_test 测试数据的特征矩阵（不包括标签列）
//' @param y_test 测试数据的标签向量
//' @param learning_rate 学习率
//' @param epochs 迭代次数
//' @return 一个列表，包含训练好的权重矩阵、每次迭代的损失值和测试集的准确率
//' @examples
//' \dontrun{
//' data(filmtrain)
//' data(filmtest)
//' X_train <- as.matrix(filmtrain[, 1:(ncol(filmtrain)-1)])
//' y_train <- filmtrain[, ncol(filmtrain)]
//' X_test <- as.matrix(filmtest[, 1:(ncol(filmtest)-1)])
//' y_test <- filmtest[, ncol(filmtest)]
//' model <- softmaxCpp(X_train, y_train, X_test, y_test, learning_rate = 0.01, epochs = 100)
//' }
//' @export
// [[Rcpp::export]]
 List softmaxCpp(NumericMatrix X, IntegerVector y, NumericMatrix X_test, IntegerVector y_test, double learning_rate = 0.01, int epochs = 100) {
   int num_samples = X.nrow();
   int num_features = X.ncol();
   IntegerVector classes = unique(y).sort();
   int num_classes = classes.size();
   
   NumericMatrix W(num_features, num_classes);
   NumericVector loss_history(epochs);
   
   // 将标签转换为 one-hot 编码
   IntegerMatrix y_one_hot(num_samples, num_classes);
   for (int i = 0; i < num_samples; i++) {
     int class_index = std::distance(classes.begin(), std::find(classes.begin(), classes.end(), y[i]));
     y_one_hot(i, class_index) = 1;
   }
   
   for (int epoch = 0; epoch < epochs; epoch++) {
     // 前向传播
     NumericMatrix scores(num_samples, num_classes);
     for (int i = 0; i < num_samples; i++) {
       for (int j = 0; j < num_classes; j++) {
         double score = 0.0;
         for (int k = 0; k < num_features; k++) {
           score += X(i, k) * W(k, j);
         }
         scores(i, j) = score;
       }
     }
     
     // 计算概率
     NumericMatrix exp_scores(num_samples, num_classes);
     NumericVector sum_exp_scores(num_samples);
     for (int i = 0; i < num_samples; i++) {
       double sum_exp = 0.0;
       for (int j = 0; j < num_classes; j++) {
         exp_scores(i, j) = std::exp(scores(i, j));
         sum_exp += exp_scores(i, j);
       }
       sum_exp_scores[i] = sum_exp;
     }
     
     NumericMatrix probs(num_samples, num_classes);
     for (int i = 0; i < num_samples; i++) {
       for (int j = 0; j < num_classes; j++) {
         probs(i, j) = exp_scores(i, j) / sum_exp_scores[i];
       }
     }
     
     // 计算损失
     double loss = 0.0;
     for (int i = 0; i < num_samples; i++) {
       for (int j = 0; j < num_classes; j++) {
         if (y_one_hot(i, j) == 1) {
           loss -= std::log(probs(i, j));
         }
       }
     }
     loss /= num_samples;
     loss_history[epoch] = loss;
     
     // 反向传播
     NumericMatrix dscores = clone(probs);
     for (int i = 0; i < num_samples; i++) {
       for (int j = 0; j < num_classes; j++) {
         dscores(i, j) -= y_one_hot(i, j);
       }
     }
     for (int i = 0; i < num_samples; i++) {
       for (int j = 0; j < num_classes; j++) {
         dscores(i, j) /= num_samples;
       }
     }
     
     NumericMatrix dW(num_features, num_classes);
     for (int k = 0; k < num_features; k++) {
       for (int j = 0; j < num_classes; j++) {
         double grad = 0.0;
         for (int i = 0; i < num_samples; i++) {
           grad += X(i, k) * dscores(i, j);
         }
         dW(k, j) = grad;
       }
     }
     
     // 参数更新
     for (int k = 0; k < num_features; k++) {
       for (int j = 0; j < num_classes; j++) {
         W(k, j) -= learning_rate * dW(k, j);
       }
     }
   }
   
   // 计算测试集准确率
   int num_test_samples = X_test.nrow();
   NumericMatrix scores_test(num_test_samples, num_classes);
   for (int i = 0; i < num_test_samples; i++) {
     for (int j = 0; j < num_classes; j++) {
       double score = 0.0;
       for (int k = 0; k < num_features; k++) {
         score += X_test(i, k) * W(k, j);
       }
       scores_test(i, j) = score;
     }
   }
   
   // 计算概率
   NumericMatrix exp_scores_test(num_test_samples, num_classes);
   NumericVector sum_exp_scores_test(num_test_samples);
   for (int i = 0; i < num_test_samples; i++) {
     double sum_exp = 0.0;
     for (int j = 0; j < num_classes; j++) {
       exp_scores_test(i, j) = std::exp(scores_test(i, j));
       sum_exp += exp_scores_test(i, j);
     }
     sum_exp_scores_test[i] = sum_exp;
   }
   
   NumericMatrix probs_test(num_test_samples, num_classes);
   for (int i = 0; i < num_test_samples; i++) {
     for (int j = 0; j < num_classes; j++) {
       probs_test(i, j) = exp_scores_test(i, j) / sum_exp_scores_test[i];
     }
   }
   
   // 预测测试集类别
   IntegerVector y_pred_test(num_test_samples);
   for (int i = 0; i < num_test_samples; i++) {
     double max_prob = probs_test(i, 0);
     int pred = 0;
     for (int j = 1; j < num_classes; j++) {
       if (probs_test(i, j) > max_prob) {
         max_prob = probs_test(i, j);
         pred = j;
       }
     }
     y_pred_test[i] = classes[pred];
   }
   
   // 计算准确率
   int correct = 0;
   for (int i = 0; i < num_test_samples; i++) {
     if (y_pred_test[i] == y_test[i]) {
       correct++;
     }
   }
   double test_accuracy = static_cast<double>(correct) / num_test_samples;
   
   return List::create(Named("W") = W,
                       Named("loss_history") = loss_history,
                       Named("test_accuracy") = test_accuracy);
 }
