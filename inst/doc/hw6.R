## -----------------------------------------------------------------------------
# 加载必要的包
library(coda)

# 设置参数
a <- 2       # Beta 分布的参数 a
b <- 2       # Beta 分布的参数 b
n <- 10      # 最大可能的 x 值
n_samples <- 10000  # 每条链的样本数

# Gibbs 采样函数
gibbs_sampler <- function(n_samples, n, a, b) {
  x_samples <- numeric(n_samples)
  y_samples <- numeric(n_samples)
  
  # 初始化值
  x_samples[1] <- rbinom(1, n, 0.5)
  y_samples[1] <- rbeta(1, x_samples[1] + a, n - x_samples[1] + b)
  
  for (i in 2:n_samples) {
    # 使用条件分布从 x | y 采样
    x_samples[i] <- rbinom(1, n, y_samples[i - 1])
    # 使用条件分布从 y | x 采样
    y_samples[i] <- rbeta(1, x_samples[i] + a, n - x_samples[i] + b)
  }
  
  return(list(x = x_samples, y = y_samples))
}

# 生成两条独立的链
chain1 <- gibbs_sampler(n_samples, n, a, b)
chain2 <- gibbs_sampler(n_samples, n, a, b)

# 将链转化为 mcmc 对象
x_chain1 <- mcmc(chain1$x)
y_chain1 <- mcmc(chain1$y)
x_chain2 <- mcmc(chain2$x)
y_chain2 <- mcmc(chain2$y)

# 结合链并执行 Gelman-Rubin 收敛监测
chains_x <- mcmc.list(x_chain1, x_chain2)
chains_y <- mcmc.list(y_chain1, y_chain2)
gelman_diag_x <- gelman.diag(chains_x)
gelman_diag_y <- gelman.diag(chains_y)

# 输出 Gelman-Rubin 诊断结果
print("x 链的 Gelman-Rubin 诊断结果：")
print(gelman_diag_x)
print("y 链的 Gelman-Rubin 诊断结果：")
print(gelman_diag_y)

# 检查收敛性
if (all(gelman_diag_x$psrf < 1.2) && all(gelman_diag_y$psrf < 1.2)) {
  cat("链已收敛\n")
} else {
  cat("链未完全收敛，请增加样本数\n")
}

# 可视化生成的样本
par(mfrow = c(1, 2))
plot(chain1$x, chain1$y, main = "Gibbs Sampler Chain 1", xlab = "x", ylab = "y", pch = 20, col = rgb(0, 0, 1, 0.5))
plot(chain2$x, chain2$y, main = "Gibbs Sampler Chain 2", xlab = "x", ylab = "y", pch = 20, col = rgb(1, 0, 0, 0.5))



## -----------------------------------------------------------------------------
# 定义参数
a <- 2
b <- 2
n <- 10
n_samples <- 10000

# Gibbs 采样函数
gibbs_sampler <- function(n_samples, n, a, b) {
  x_samples <- numeric(n_samples)
  y_samples <- numeric(n_samples)
  
  # 初始化值
  x_samples[1] <- rbinom(1, n, 0.5)
  y_samples[1] <- rbeta(1, x_samples[1] + a, n - x_samples[1] + b)
  
  for (i in 2:n_samples) {
    x_samples[i] <- rbinom(1, n, y_samples[i - 1])
    y_samples[i] <- rbeta(1, x_samples[i] + a, n - x_samples[i] + b)
  }
  
  return(list(x = x_samples, y = y_samples))
}

# 生成样本
samples <- gibbs_sampler(n_samples, n, a, b)

# Gelman-Rubin 收敛监控
x_chain <- mcmc(samples$x)
y_chain <- mcmc(samples$y)
chains <- mcmc.list(x_chain, y_chain)
gelman_diag <- gelman.diag(chains)
print(gelman_diag)

# 绘制样本
plot(samples$x, samples$y, main = "Gibbs Sampler for Bivariate Density", xlab = "x", ylab = "y")


