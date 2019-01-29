#lecture 4
# GARCH - Generalized autoregressive conditional heteroscetasticity model
dataset <- read.csv("stockdata.csv", sep = ",", header = TRUE)
names(dataset) <- c("Date", "Last", "High", "Low", "Turnover")
dataset[, 1] <- as.Date(dataset[, 1], format = "%d.%m.%y")

head(dataset)
summary(dataset)

dataset <- dataset[order(dataset$Date),]
dataset <- dataset[,c("Date", "Last")]

dataset$r <- c(NA, diff(log(dataset$Last)))

plot(dataset$Date, dataset$r, type = "l", xlab = "", ylab = "r")


r <-  na.omit(dataset$r)

garch.var <- function(r, omega, alpha, beta){
  sigma2 <-  r[1]^2
  for(i in 2:length(r)){
    sigma2 <- c(sigma2, omega + alpha*r[i]^2 + beta*sigma2[i-1])
  }
  return(sigma2)
}

garch.ll.fun <- function(par,r){
  omega <- par[1]
  alpha <- par[2]
  beta <- par[3]
  sigma2 <- garch.var(r, omega, alpha, beta)
  r <- r[-1]
  sigma2 <- sigma2[-length(sigma2)]
  ll <- sum(-log(sigma2) - r^2/sigma2)
  return(-ll)
}

res <-  nlminb(c(0.001,0.3,0.3), garch.ll.fun, lower = 1e-06, upper = 1 - 1e-06,r = r)
res

omega <- res$par[1]
alpha <- res$par[2]
beta <- res$par[3]
gamma <- 1 - alpha - beta
VL <- omega/gamma
VL

#volatility forecast
m <- 249
garch.sigma2 <- garch.var(r,omega, alpha, beta)
garch.sigma2.t <- garch.sigma2[length(garch.sigma2)]
garch.sigma2.vec <- VL + (alpha+beta)^(0:m) * (garch.sigma2.t-VL)

garch.sigma.vec <- sqrt(garch.sigma2.vec)
plot(garch.sigma.vec * sqrt(250), xlab = "Days", ylab = "GARCH Volatility",
     type = "l")
sqrt(sum(garch.sigma2.vec))



#option implied volatility

C.market <- mean(c(7.5, 8.75))

S0 <- 766.12 # OBX index
K <- 770 # Strike price
T <- as.numeric(as.Date("2018-02-16") - as.Date("2018-01-24"))/365 # Maturity
rf <- 0.008 # Risk-free interest rate

call <- function(S0, sigma, K, rf, T) {
d1 <- (log(S0/K) + (rf + sigma^2/2) * T)/sigma/sqrt(T)
d2 <- d1 - sigma * sqrt(T)
C <- S0 * pnorm(d1) - exp(-rf * T) * K * pnorm(d2)
return(C)
}

obj.fun <- function(sigma, C.market, S0, K, rf, T) {
C.model <- call(S0, sigma, K, rf, T)
eps <- (C.model - C.market)^2
return(eps)
}

res <- nlm(obj.fun, p = 0.2, C.market = C.market, S0 = S0, K = K,
           rf = rf, T = T)
res
