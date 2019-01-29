dataset <- read.csv("stockdata.csv", sep = ",", header = TRUE)
names(dataset) <- c("Date", "Last", "High", "Low", "Turnover")
dataset[, 1] <- as.Date(dataset[, 1], format = "%d.%m.%y")

head(dataset)
summary(dataset)

dataset <- dataset[order(dataset$Date),]
dataset <- dataset[,c("Date", "Last")]

dataset$r <- c(NA, diff(log(dataset$Last)))

plot(dataset$Date, dataset$r, type = "l", xlab = "", ylab = "r")

#exponentially weighted moving average model
r <-  na.omit(dataset$r)
n <-  length(r)
lambda <-  0.94
lambda.vec <- lambda^(0:(n-1))
r.vec <-  r[length(r):1]
sigma2 <- (1-lambda) * sum(lambda.vec * r.vec^2)
sigma <- sqrt(sigma2)
sigma * sqrt(250)


#or like this
sd(r.vec) * sqrt(250)
sigma2 <-  sum((1-lambda) * lambda^(0:(length(r)-1)) * rev(r)^2)
sigma <-  sqrt(sigma2)
sigma * sqrt(250)

#or like this
sigma2 <- c()
for (i in 1:length(r)){
  sigma2 <- c(sigma2, sum((1-lambda)*lambda^(0:(i-1))*rev(r[1:i])^2))
}
sigma <-  sqrt(sigma2)

plot(sigma * sqrt(250), xlab="",ylab = "EWMA", type = "l")

#we add the maximmum likelihood approach for the lambda

#function that computes EWMA variances
ewma.var <- function(r,lambda){
  sigma2 <- sum((1-lambda)*lambda^(0:(length(r)-1))*rev(r)^2)
  return(sigma2)
}

#function that computes historical EWMA variances
hist.ewma.var <- function(r,lambda){
  sigma2 <- c()
  for(i in 1:length(r)){
    sigma2 <- c(sigma2, ewma.var(r[1:i],lambda))
  }
  return(sigma2)
}

#log likelihood function for EWMA volatility model
ewma.ll.fun <- function(lambda,r){
  sigma2 <- hist.ewma.var(r, lambda)
  sigma2 <-  sigma2[-length(sigma2)]
  r <- r[-1]
  log.ll <-  sum(-log(sigma2) - r^2/sigma2)
  return(-log.ll)
}

res <- nlminb(0.5, ewma.ll.fun, lower = 1e-06, upper= 1 - 1e-06,r = r)
res 


