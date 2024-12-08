## ----eval=FALSE---------------------------------------------------------------
# conformal_p_value <- function(model, train_data, test_data) {
#   # 计算非一致性得分
#   nonconformity_scores <- apply(test_data, 1, function(x) abs(predict(model, newdata = matrix(x, nrow = 1)) - mean(predict(model, newdata = train_data))))
# 
#   # 基于非一致性得分计算 p-value
#   p_values <- sapply(nonconformity_scores, function(score) mean(score >= nonconformity_scores))
# 
#   return(p_values)
# }
# 该函数使用 apply() 函数计算每个测试数据点的非一致性得分。
# 然后，通过将每个点的得分与训练集中所有其他点的得分进行比较，计算对应的 p-value。

## -----------------------------------------------------------------------------
conformal_prediction_set <- function(model, train_data, test_data, p_threshold) {
  p_values <- conformal_p_value(model, train_data, test_data)
  
  # 根据 p-value 阈值筛选预测值
  prediction_set <- list()
  for (i in 1:length(p_values)) {
    if (p_values[i] >= p_threshold) {
      prediction_set[[i]] <- predict(model, newdata = matrix(test_data[i,], nrow = 1))
    } else {
      prediction_set[[i]] <- NA  # 如果 p-value 太低，则不进行预测
    }
  }
  
  return(prediction_set)
}


