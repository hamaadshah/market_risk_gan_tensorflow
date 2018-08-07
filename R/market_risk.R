# Set scientific notation penalty to high for better printing.
options(scipen = 999)

# Load required libraries.
library(ggplot2)
library(quantmod)
library(MASS)
library(scales)
library(reshape2)

# Let's take a year's worth of data.
start <- as.Date("2016-01-01")
end <- as.Date("2016-12-31")

# Let's get Apple, Google, Microsoft, Intel and Box data from Yahoo.
getSymbols(Symbols = "AAPL;GOOG;MSFT;INTC;BOX", 
           src = "yahoo", 
           from = start, 
           to = end)

# Create daily returns series based on close prices.
data <- data.frame("Apple" = as.numeric(AAPL[, "AAPL.Close"]),
                   "Google" = as.numeric(GOOG[, "GOOG.Close"]),
                   "Microsoft" = as.numeric(MSFT[, "MSFT.Close"]),
                   "Intel" = as.numeric(INTC[, "INTC.Close"]),
                   "Box" = as.numeric(BOX[, "BOX.Close"]))
ret.data <- diff(x = as.matrix(log(x = data)), lag = 1)
colnames(ret.data) <- c("Apple", "Google", "Microsoft", "Intel", "Box")

# Have a look at the returns data.
head(x = ret.data, n = 5)

# Let's assume we have an equal weight for all assets in our 5 assets portfolio.
# Based on those weightings calculate the portfolio returns on a daily basis.
port.data <- data.frame(apply(X = ret.data, 
                              MARGIN = 1,
                              FUN = function(x) weighted.mean(x = x, 
                                                              w = rep(x = 1 / ncol(ret.data), ncol(ret.data)))))
colnames(port.data) <- "Portfolio.ret"

# Plot the daily portfolio returns data.
png(filename = "portfolio_returns.png", width = 600, height = 400)
ggplot(data = port.data, 
       aes(x = Portfolio.ret)) +
geom_density(fill = "skyblue", alpha = 0.25) +
ggtitle("KDE plot of portfolio returns") +
xlab("Portfolio returns") +
ylab("Density") +
scale_x_continuous(labels = percent)
dev.off()

# Get mean returns vector.
est.ret.vector <- colMeans(x = ret.data)

# Get volatilities vector.
est.sd <- as.matrix(apply(X = ret.data, 
                          MARGIN = 2, 
                          FUN = sd))

# Get correlation matrix.
est.cor.matrix <- cor(x = ret.data)

# Use volatilies vector and correlation matrix to estimate variance-covariance matrix.
est.cov.matrix <- est.cor.matrix * (est.sd %*% t(est.sd))

# Given the earlier portfolio weights estimate portfolio volatility.
est.sd.port <- sqrt(t(as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data)))) %*% 
                      est.cov.matrix %*% 
                      as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data))))

# Estimate VaR using variance-covariance method.
VaR.var.covar <- (est.sd.port * qnorm(p = 0.01, 
                                      mean = 0, 
                                      sd = 1)) + mean(port.data$Portfolio.ret)

# Estimate VaR using historical simulation method.
VaR.hist.sim <- quantile(x = sample(x = port.data$Portfolio.ret, 
                                    size = 1000, 
                                    replace = TRUE), 
                         probs = 0.01)

# Estimate VaR using Monte Carlo simulation method.
# Simulate returns data from multidimensional Gaussian distribution.
sim.ret.data <- mvrnorm(n = 1000, 
                        mu = est.ret.vector, 
                        Sigma = est.cov.matrix)

# Get portfolio returns.
sim.port.data <- data.frame(apply(X = sim.ret.data, 
                                  MARGIN = 1,
                                  FUN = function(x) weighted.mean(x = x, 
                                                                  w = rep(x = 1 / ncol(sim.ret.data), ncol(sim.ret.data)))))
colnames(sim.port.data) <- "Portfolio.ret"

VaR.MC <- quantile(x = sim.port.data$Portfolio.ret, 
                   probs = 0.01)

VaR <- data.frame("Var.Covar" = VaR.var.covar,
                  "Hist.Sim" = VaR.hist.sim,
                  "MC" = VaR.MC)