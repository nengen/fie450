dataset <- read.csv("stockdata.csv", sep = ",", header = TRUE)
names(dataset) <- c("Date", "Last", "High", "Low", "Turnover")
dataset[, 1] <- as.Date(dataset[, 1], format = "%d.%m.%y")

head(dataset)
summary(dataset)

dataset <- dataset[order(dataset$Date),]
dataset <- dataset[,c("Date", "Last")]

plot(dataset$Date, dataset$Last, type = "l", xlab = "", ylab = "OBX")

#returns
dataset$r <- c(NA, diff(log(dataset$Last)))

mean(dataset$r, na.rm = TRUE)
sd(dataset$r, na.rm = TRUE)

mu <- mean(dataset$r, na.rm = TRUE) * 250
sigma <- sd(dataset$r, na.rm = TRUE) * sqrt(250)
sharpe <- mu/sigma

r <-  na.omit(dataset$r)
n <- length(r)
SE <- sd(r) * sqrt(250)/sqrt(n)


p <- 0.99
z <- qnorm((1-p)/2)
ci <- c(mu-z*SE,mu+z*SE)

#Exercise2
ibmdata <- read.csv("IBM.csv", header = TRUE, sep = ",")
ibmdata <- ibmdata[order(ibmdata$Date),]
ibmdata <- ibmdata[,c("Date", "Close")]

plot(ibmdata$Date, ibmdata$Close, type = "l", xlab = "", ylab = "IBM")

#returns
ibmdata$normal <- c(NA, diff(ibmdata$Close))
ibmdata$r <- c(NA, diff(log(ibmdata$Close)))

mean(ibmdata$r, na.rm = TRUE)
sd(ibmdata$r, na.rm = TRUE)

mu <- mean(ibmdata$r, na.rm = TRUE) * 12
sigma <- sd(ibmdata$r, na.rm = TRUE) * sqrt(12)
sharpe <- mu/sigma

r <-  na.omit(ibmdata$r)
n <- length(r)
SE <- sd(r) * sqrt(12)/sqrt(n)


p <- 0.99
z <- qnorm((1-p)/2)
ci <- c(mu-z*SE,mu+z*SE)
