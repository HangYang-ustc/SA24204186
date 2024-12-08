## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
if (!require(microbenchmark)) {
  install.packages("microbenchmark", repos = "https://cloud.r-project.org/")
  library(microbenchmark)
} else {
  library(microbenchmark)
}



## -----------------------------------------------------------------------------
# Monte Carlo积分的练习，使用对偶变量减少方差。
# 设定种子以确保可重复性
set.seed(123)

# 对偶变量方法计算exp(-x^2)的积分
n <- 10000
u <- runif(n)
x <- exp(-u^2)
y <- exp(-(1-u)^2)
I_antithetic <- mean((x + y) / 2)

# 直接Monte Carlo方法
I_standard <- mean(x)

# 打印结果
cat("对偶变量方法的结果:", I_antithetic, "\n")
cat("标准Monte Carlo方法的结果:", I_standard, "\n")


## -----------------------------------------------------------------------------
# 使用控制变量减少方差的方法。
# 使用控制变量计算e^(-x)的积分
n <- 10000
u <- runif(n)
h <- exp(-u)
g <- u  # 控制变量 g(u) = u

# 计算
cov_hg <- cov(h, g)
var_g <- var(g)
alpha <- -cov_hg / var_g
I_control <- mean(h + alpha * (g - 0.5))

cat("使用控制变量的方法得到的结果:", I_control, "\n")


## -----------------------------------------------------------------------------
# 重要性抽样的例子，计算exp(-x^2/2)的积分。
# 使用重要性抽样
n <- 10000
u <- runif(n)
g <- 1 / (1 + u^2)  # 权重函数
h <- exp(-u^2 / 2) / g
I_importance <- mean(h * g)

cat("使用重要性抽样的方法得到的结果:", I_importance, "\n")


## -----------------------------------------------------------------------------
library(microbenchmark)
set.seed(123)

# 定义排序函数和模拟实验
n_values <- c(1e4, 2e4, 4e4, 6e4, 8e4)
num_simulations <- 100
avg_times <- numeric(length(n_values))
logn_values <- n_values * log(n_values)

for (i in seq_along(n_values)) {
  times <- microbenchmark(
    sort(sample(1:n_values[i])),
    times = num_simulations
  )$time
  avg_times[i] <- mean(times) / 1e6  # 转换为毫秒
}

# 回归分析
lm_fit <- lm(avg_times ~ logn_values)

# 绘制结果
plot(logn_values, avg_times, main = "计算时间 vs n log(n)",
     xlab = "n log(n)", ylab = "平均计算时间 (ms)")
abline(lm_fit, col = "red")
summary(lm_fit)


