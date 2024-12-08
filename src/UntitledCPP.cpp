#include <Rcpp.h>
using namespace Rcpp;

//' @title Conformal Prediction for Regression
//' @name conformal_predict
//' @description 本函数基于简单的 conformal prediction 方法为每个测试数据点生成预测区间。
//' @param train_x 一个数值型向量，表示训练数据的特征（自变量）
//' @param train_y 一个数值型向量，表示训练数据的标签（目标值）
//' @param test_x 一个数值型向量，表示测试数据的特征（自变量）
//' @param alpha 一个介于 0 和 1 之间的数值，定义预测区间的置信水平
//' @return 一个包含每个测试数据点预测区间的列表，包含下界和上界。
//' @examples
//' train_x <- c(1, 2, 3, 4, 5)
//' train_y <- c(1.1, 2.0, 3.1, 4.2, 5.1)
//' test_x <- c(2.5, 3.5)
//' alpha <- 0.05
//' res <- conformal_predict(train_x, train_y, test_x, alpha)
//' print(res)
//' @export
 // [[Rcpp::export]]
 List conformal_predict(NumericVector train_x, NumericVector train_y, 
                        NumericVector test_x, double alpha) {
   
   int n_train = train_x.size(); // 训练集样本数
   int n_test = test_x.size();   // 测试集样本数
   
   // 计算训练集的非一致性得分
   NumericVector nonconformity_scores(n_train);
   for (int i = 0; i < n_train; ++i) {
     // 这里使用训练集的简单绝对误差作为非一致性得分
     nonconformity_scores[i] = std::abs(train_y[i] - train_x[i]);
   }
   
   // 将非一致性得分排序
   std::sort(nonconformity_scores.begin(), nonconformity_scores.end());
   
   // 找到与给定 alpha 水平对应的阈值
   int threshold_index = static_cast<int>((1 - alpha) * n_train); 
   double threshold = nonconformity_scores[threshold_index];
   
   // 为每个测试数据点预测区间
   NumericVector lower_bound(n_test); // 预测下界
   NumericVector upper_bound(n_test); // 预测上界
   for (int i = 0; i < n_test; ++i) {
     // 简单的预测模型（此处直接使用 test_x 作为预测值，实际应用中可根据需要进行修改）
     double prediction = test_x[i];  
     
     // 预测区间基于非一致性得分的阈值
     lower_bound[i] = prediction - threshold;
     upper_bound[i] = prediction + threshold;
   }
   
   // 返回包含预测区间的列表
   return List::create(Named("lower") = lower_bound,
                       Named("upper") = upper_bound);
 }
