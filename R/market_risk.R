setwd(dir = "/home/hamaad/Projects/market_risk_gan_tensorflow/R/")

options(scipen = 999)

library(package = ggplot2)
library(package = quantmod)
library(package = MASS)
library(package = scales)
library(package = reshape2)

# Let's take a year's worth of data.
start <- as.Date(x = "2016-01-01")
end <- as.Date(x = "2016-12-31")

# Let's get Apple, Google, Microsoft, Intel and Box data from Yahoo.
getSymbols(
  Symbols = "AAPL;GOOG;MSFT;INTC;BOX",
  src = "yahoo",
  from = start,
  to = end
)

# Create daily returns series based on close prices.
data <- data.frame(
  "Apple" = as.numeric(x = AAPL[, "AAPL.Close"]),
  "Google" = as.numeric(x = GOOG[, "GOOG.Close"]),
  "Microsoft" = as.numeric(x = MSFT[, "MSFT.Close"]),
  "Intel" = as.numeric(x = INTC[, "INTC.Close"]),
  "Box" = as.numeric(x = BOX[, "BOX.Close"])
)
ret.data <- diff(x = as.matrix(x = log(x = data)), lag = 1)
colnames(x = ret.data) <-
  c("Apple", "Google", "Microsoft", "Intel", "Box")

# Have a look at the returns data.
head(x = ret.data, n = 5)

# Let's assume we have an equal weight for all assets in our 5 assets portfolio.
# Based on those weightings calculate the portfolio returns on a daily basis.
port.data <- data.frame(apply(
  X = ret.data,
  MARGIN = 1,
  FUN = function(x)
    weighted.mean(x = x,
                  w = rep(x = 1 / ncol(x = ret.data), ncol(x = ret.data)))
))
colnames(port.data) <- "Portfolio.ret"

# Plot the daily portfolio returns data.
png(filename = "portfolio_returns.png",
    width = 600,
    height = 400)
ggplot(data = port.data,
       aes(x = Portfolio.ret)) +
  geom_density(fill = "skyblue", alpha = 0.25) +
  ggtitle(label = "KDE plot of portfolio returns") +
  xlab(label = "Portfolio returns") +
  ylab(label = "Density") +
  scale_x_continuous(labels = percent)
dev.off()

# Get mean returns vector.
est.ret.vector <- colMeans(x = ret.data)

# Get volatilities vector.
est.sd <- as.matrix(x = apply(X = ret.data,
                              MARGIN = 2,
                              FUN = sd))

# Get correlation matrix.
est.cor.matrix <- cor(x = ret.data)

# Use volatilies vector and correlation matrix to estimate variance-covariance matrix.
est.cov.matrix <- est.cor.matrix * (est.sd %*% t(x = est.sd))

# Given the earlier portfolio weights estimate portfolio volatility.
est.sd.port <-
  sqrt(x = t(x = as.matrix(x = rep(
    x = 1 / ncol(x = ret.data), ncol(x = ret.data)
  ))) %*%
    est.cov.matrix %*%
    as.matrix(x = rep(x = 1 / ncol(x = ret.data), ncol(x = ret.data))))

# Estimate VaR using variance-covariance method.
VaR.var.covar <- (est.sd.port * qnorm(p = 0.01,
                                      mean = 0,
                                      sd = 1)) + mean(x = port.data$Portfolio.ret)

# Estimate VaR using historical simulation method.
VaR.hist.sim <- quantile(
  x = sample(
    x = port.data$Portfolio.ret,
    size = 1000,
    replace = TRUE
  ),
  probs = 0.01
)

# Estimate VaR using Monte Carlo simulation method.
# Simulate returns data from multidimensional Gaussian distribution.
sim.ret.data <- mvrnorm(n = 1000,
                        mu = est.ret.vector,
                        Sigma = est.cov.matrix)

# Get portfolio returns.
sim.port.data <- data.frame(apply(
  X = sim.ret.data,
  MARGIN = 1,
  FUN = function(x)
    weighted.mean(x = x,
                  w = rep(
                    x = 1 / ncol(x = sim.ret.data), ncol(x = sim.ret.data)
                  ))
))
colnames(x = sim.port.data) <- "Portfolio.ret"

VaR.MC <- quantile(x = sim.port.data$Portfolio.ret,
                   probs = 0.01)

VaR <- data.frame("Var.Covar" = VaR.var.covar,
                  "Hist.Sim" = VaR.hist.sim,
                  "MC" = VaR.MC)

write.csv(x = ret.data,
          file = "ret_data.csv",
          row.names = FALSE)