library(tidyverse)
library(ggpubr)

#Response variable: Life_expectancy
#Numerical predictor variable: Adult_Mortality

####removing missing data####
lifeExpectancy <- Life_Expectancy_Data %>%
  select(c(Life_expectancy, Adult_Mortality)) %>%
  drop_na()

####Scatter plot####
ggplot(lifeExpectancy, aes(x=Adult_Mortality, y=Life_expectancy)) +
  geom_point() +
  labs(title = "Adult Mortality versus Life Expectancy", x = "Adult Mortality", y = "Life Expectancy")

####Histograms####
adultMortalityHist <- ggplot(lifeExpectancy, aes(x=Adult_Mortality)) +
  geom_histogram() +
  labs(title = "Adult Mortality", x= "Adult Deaths per 1000 people", y="")

lifeExpectancyHist <- ggplot(lifeExpectancy, aes(x=Life_expectancy)) +
  geom_histogram() +
  labs(title = "Life Expectancy", x="Life Expectancy (age)", y="")

ggarrange(adultMortalityHist, lifeExpectancyHist)

####Calculating variance of response####
responseVar <- round(var(lifeExpectancy$Life_expectancy),digits=3)
responseVar

####sample correlation####
cor(lifeExpectancy$Life_expectancy, lifeExpectancy$Adult_Mortality)

####point estimates of B1 and B0####
regLine <- lm(Life_expectancy ~ Adult_Mortality, data = lifeExpectancy)
summary(regLine)
regLine$coefficients
#b1 = -0.0533358
#b0 = 78.018216

####Fitted value calculation####
regLine$fitted.values
summary(regLine$fitted.values)

####R2 and MSE####
#R2 = SSR/SST
anova(regLine)
SSR <- 128741
SSE <- 136750
SST <- SSR + SSE
R2 <- SSR/SST
R2

#MSE = SSE/(n-2)
n <- nrow(lifeExpectancy)
MSE <- SSE/(n-2)
MSE

####Standard Errors B1 and B0####
#need to find these equations

####Confidence intervals b1 and b0####

summary(regLine)
b0 <- 78.018216
b1 <- -0.053358
tdist <- qt(0.975, n-2)
sb0 <- sqrt(0.209837)
sb1 <- sqrt(0.001017)

b0.lower <- b0 - tdist*sb0
b0.upper <- b0 + tdist*sb0

b1.lower <- b1 - tdist*sb1
b1.upper <- b1 + tdist*sb1

#77.12002 < b0 < 78.91641
#-0.115888 < b1 < 0.009171975

####Confidence predictor X0####

x <- 200
x.bar <- mean(lifeExpectancy$Adult_Mortality)
yh.hat <- b0 + b1*x
sYh <- MSE * ((1/n) + (x-x.bar)^2/Sxx)
sEpred <- sqrt(sYh^2 + MSE)

expY.lower <- yh.hat - tdist * sYh
expY.upper <- yh.hat + tdist * sYh

Ynew.lower <- yh.hat - tdist * sEpred
Ynew.upper <- yh.hat + tdist * sEpred

#Confidence interval: 67.31281 < E[Yh] < 67.38043
#Prediction interval: 53.94196 < Ynew < 80.75127

####Plotting confidence bands####

plot(lifeExpectancy$Adult_Mortality, lifeExpectancy$Life_expectancy, xlab = "Adult Mortality", 
     ylab = "Life Expectancy", pch=19, col="gray")
abline(regLine,lty=1,col="black")

sort.X <- sort(lifeExpectancy$Adult_Mortality)
sort.fitted.Y <- regLine$fitted.values[order(lifeExpectancy$Adult_Mortality)]

W2.val <- 2*qf(0.95,2,n-2)
W.val <- sqrt(W2.val)

Sxx <- sum((sort.X-mean(sort.X))^2)
se.Y.hat <- sqrt(MSE * ((1/n) + (sort.X-mean(lifeExpectancy$Adult_Mortality))^2/Sxx))

upper.band <- sort.fitted.Y + W.val*se.Y.hat
lower.band <- sort.fitted.Y - W.val*se.Y.hat

lines(sort.X,upper.band,col="blue",lty=2,lwd=2); lines(sort.X,lower.band,col="blue",lty=2,lwd=2)

new.X <- seq(0,750, length.out=100)
new.Y <- b0 + b1*new.X

S2.val <- 2*qf(0.95,2,n-2)
S.val <- sqrt(S2.val)

se.Y.pred.hat <- sqrt(MSE * (1 + (1/n) + (new.X-mean(lifeExpectancy$Adult_Mortality))^2/Sxx))

upper.pred.band <- new.Y + S.val*se.Y.pred.hat
lower.pred.band <- new.Y - S.val*se.Y.pred.hat

lines(new.X,upper.pred.band,col="red",lty=2,lwd=2); lines(new.X,lower.pred.band,col="red",lty=2,lwd=2)
