library(tidyverse)

#response: Life_expectancy
#chosen predictor variables: GDP, infant_deaths, Measles

#dataset name: Life_Expectancy_Data

#use tidyverse to remove missing values from data
lifeExpectancy <- Life_Expectancy_Data %>%
  select(c(Life_expectancy, GDP, infant_deaths, Measles)) %>%
  drop_na()

####design matrix####

n <- nrow(lifeExpectancy)
Y <- matrix(lifeExpectancy$Life_expectancy, nrow=n, ncol=1)
X <- matrix(c(rep(1,n), lifeExpectancy$GDP, lifeExpectancy$infant_deaths, lifeExpectancy$Measles),
            nrow=n, ncol=4, byrow=FALSE)

#X1:GDP X2:Infant Deaths X3:Measles
#Y:Life Expectancy
colnames(X) <- c("Intercept", "X1", "X2", "X3")
colnames(Y) <- "Y"

####correlation matrix + scatter plot####

fullMatrix <- matrix(c(lifeExpectancy$Life_expectancy, lifeExpectancy$GDP, lifeExpectancy$infant_deaths, lifeExpectancy$Measles),
               nrow=n, ncol=4, byrow=FALSE)
colnames(fullMatrix) <- c("Y", "X1", "X2", "X3")
cor(fullMatrix)

pairs(fullMatrix)

####point estimates####

XtX <- t(X)%*%X
XtY <- t(X)%*%Y
b <- solve(XtX)%*%XtY
b

####residuals and fitted values####

#residuals
inv.XX <- solve(t(X)%*%X)
H <- X%*%inv.XX%*%t(X)
I <- diag(1,nrow=n,ncol=n)
res <- (I-H)%*%Y 

#fitted values
Y.hat <- X%*%b 

####MSE####

MSE <- sum((Y-Y.hat)^2)/2481
#MSE <- t(Y)%*%(I-H)%*%Y/(n-2)
MSE

####R2 and adjusted R2####

Y.bar <- mean(Y)
SSE <- sum((Y-Y.hat)^2)
SSR <- sum((Y.bar-Y.hat)^2)
SSTO <- sum((Y-Y.bar)^2)

R2 <- SSR/SSTO
adjR2 <- 1-(2484/2481)*(SSE/SSTO)

R2
adjR2

####F-test####

MSR <- SSR/3
F.test <- MSR/MSE
Fdist <- qf(0.95, 3, 2481)
F.test>Fdist
#reject H0

##NOT WORKING
pval <- pf(0.025, 3, 2481)
pval

####covariance matrix b.hat####

#C = (X^t X)^-1

####hypothesis tests####

XtX <- t(X)%*%X
var.b <- MSE*solve(XtX)
s.b <- sqrt(diag(var.b))
t.values <- b/s.b
t.values

#need p-vals

####Xhs matrix####

####hypothesis tests with lm####

tmp <- as.data.frame(fullMatrix)
regMat <- lm(Y ~ X1+X2+X3, tmp)