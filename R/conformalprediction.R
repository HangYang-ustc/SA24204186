#' @title Conformal Prediction R and Rcpp functions.
#' @name Conformal Prediction p-value Calculation
#' @description 本函数根据提供的模型、训练数据和测试数据计算 conformal prediction 的 p-value。p-value 衡量新数据与模型的符合程度。
#' @param model 训练好的预测模型（例如，回归或分类模型）。
#' @param train_data 一个数据框或矩阵，包含用于模型拟合的训练数据。
#' @param test_data 一个数据框或矩阵，包含用于计算 p-value 的测试数据。
#'
#' @return 一个包含每个测试数据点对应 p-value 的向量。
#' @export
conformal_p_value <- function(model, train_data, test_data) {
  # 计算非一致性得分
  nonconformity_scores <- apply(test_data, 1, function(x) abs(predict(model, newdata = matrix(x, nrow = 1)) - mean(predict(model, newdata = train_data))))
  
  # 基于非一致性得分计算 p-value
  p_values <- sapply(nonconformity_scores, function(score) mean(score >= nonconformity_scores))
  
  return(p_values)
}


#' @title Conformal Prediction Set Generation
#' @name Conformal Prediction Set Generation
#' @description 本函数根据给定的 p-value 阈值生成 conformal prediction 集合，模型用于预测新测试实例的潜在类别或范围。
#' @param model 训练好的预测模型（例如，回归或分类模型）。
#' @param train_data 一个数据框或矩阵，包含用于模型拟合的训练数据。
#' @param test_data 一个数据框或矩阵，包含用于生成预测集合的测试数据。
#' @param p_threshold 一个介于 0 和 1 之间的数值。用于确定哪些预测应包含在预测集合中的 p-value 阈值。
#'
#' @return 一个列表，包含每个测试实例的预测集合。
#' @export
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

#' @importFrom Rcpp evalCpp
#' @importFrom stats predict
#' @useDynLib SA24204186
NULL
