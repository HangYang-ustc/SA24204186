## -----------------------------------------------------------------------------
# 定义参数
N <- 1000
null_hypotheses <- 950
alt_hypotheses <- 50
alpha <- 0.1
num_simulations <- 10000

# 定义函数来生成 p 值
generate_p_values <- function() {
  p_values_null <- runif(null_hypotheses)  # 零假设的 p 值均匀分布
  p_values_alt <- rbeta(alt_hypotheses, 0.1, 1)  # 备择假设的 p 值为 beta 分布
  return(c(p_values_null, p_values_alt))  # 合并p值
}

# Bonferroni 校正
bonferroni_correction <- function(p_values, alpha) {
  return(p.adjust(p_values, method = "bonferroni") <= alpha)
}

# Benjamini-Hochberg 校正
bh_correction <- function(p_values, alpha) {
  return(p.adjust(p_values, method = "BH") <= alpha)
}

# 模拟计算 FWER, FDR, TPR
calculate_metrics <- function(method) {
  FWER <- 0
  FDR <- 0
  TPR <- 0
  
  for (i in 1:num_simulations) {
    p_values <- generate_p_values()
    rejected <- method(p_values, alpha)
    
    # 计算 FWER
    FWER <- FWER + (sum(rejected[1:null_hypotheses]) > 0)
    
    # 计算 FDR 和 TPR
    false_discoveries <- sum(rejected[1:null_hypotheses])
    true_discoveries <- sum(rejected[(null_hypotheses+1):N])
    total_discoveries <- sum(rejected)
    
    if (total_discoveries > 0) {
      FDR <- FDR + false_discoveries / total_discoveries
    }
    
    TPR <- TPR + true_discoveries / alt_hypotheses
  }
  
  FWER <- FWER / num_simulations
  FDR <- FDR / num_simulations
  TPR <- TPR / num_simulations
  
  return(c(FWER, FDR, TPR))
}

# 运行模拟
bonferroni_results <- calculate_metrics(bonferroni_correction)
bh_results <- calculate_metrics(bh_correction)

# 输出结果
results <- data.frame(
  Bonferroni_Correction = bonferroni_results,
  BH_Correction = bh_results
)
rownames(results) <- c("FWER", "FDR", "TPR")

print(results)


## -----------------------------------------------------------------------------
library(boot)

# 生成样本
set.seed(123)
data <- rnorm(100, mean = 50, sd = 10)  # 正态分布，样本均值为50，标准差为10

# 定义统计量函数，计算样本均值
mean_stat <- function(data, indices) {
  return(mean(data[indices]))  # 返回重抽样样本的均值
}

# 使用 bootstrap 方法，进行1000次重抽样，计算样本均值的置信区间
bootstrap_results <- boot(data, statistic = mean_stat, R = 1000)

# 计算 95% bootstrap 百分位置信区间
ci_percentile <- boot.ci(bootstrap_results, type = "perc")
print(ci_percentile)

# 计算 95% 的标准正态置信区间
ci_normal <- boot.ci(bootstrap_results, type = "norm")
print(ci_normal)


## -----------------------------------------------------------------------------
# 使用 BCa 方法计算 95% bootstrap 置信区间
ci_bca <- boot.ci(bootstrap_results, type = "bca")
print(ci_bca)


