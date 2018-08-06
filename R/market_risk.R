library(ggplot2)
library(quantmod)

start <- as.Date("2016-01-01")
end <- as.Date("2016-12-31")

getSymbols(Symbols = "AAPL;GOOG;MSFT;INTC;BOX", 
           src = "yahoo", 
           from = start, 
           to = end)

data <- data.frame("Apple" = as.numeric(AAPL$AAPL.Close),
                   "Google" = as.numeric(GOOG$GOOG.Close),
                   "Microsoft" = as.numeric(MSFT$MSFT.Close),
                   "Intel" = as.numeric(INTC$INTC.Close),
                   "Box" = as.numeric(BOX$BOX.Close))

log.data <- log(x = data)

ret.data <- data.frame(diff(x = as.matrix(log.data), lag = 1))

colnames(ret.data) <- c("Apple", "Google", "Microsoft", "Intel", "Box")

port.data <- data.frame(apply(X = ret.data, 
                              MARGIN = 1,
                              FUN = function(x) weighted.mean(x = x, w = rep(x = 1 / ncol(ret.data), ncol(ret.data)))))

colnames(port.data) <- "Portfolio.ret"

ggplot(data = port.data, 
       aes(x = Portfolio.ret)) +
geom_density()

tail.risks <- quantile(x = port.data$Portfolio.ret, 
                       probs = c(0.01, 0.99))

est.sd <- as.matrix(apply(X = ret.data, MARGIN = 2, FUN = sd))

est.cor.matrix <- cor(x = ret.data)

est.cov.matrix <- est.cor.matrix * (est.sd %*% t(est.sd))
s
sqrt(t(as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data)))) %*% 
       est.cov.matrix %*% 
       as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data)))) *
qnorm(p = 0.01, 
      mean = 0, 
      sd = 1)

qnorm(p = 0.01, 
      mean = mean(port.data$Portfolio.ret), 
      sd = sqrt(t(as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data)))) %*% 
                  est.cov.matrix %*% 
                  as.matrix(rep(x = 1 / ncol(ret.data), ncol(ret.data)))))