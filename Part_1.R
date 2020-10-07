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
  geom_point()

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
