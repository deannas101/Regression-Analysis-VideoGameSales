library(tidyverse)

#Response variable: Life expectancy
#Numerical predictor variable: Adult_Mortality

#removing missing data
lifeExpectancy <- Life_Expectancy_Data %>%
  select(c(Life_expectancy, Adult_Mortality)) %>%
  drop_na()

#Scatter plot
ggplot(lifeExpectancy, aes(x=Adult_Mortality, y=Life_expectancy)) +
  geom_point()

#Histograms
ggplot(lifeExpectancy, aes(x=Adult_Mortality)) +
  geom_histogram()

ggplot(lifeExpectancy, aes(x=Life_expectancy)) +
  geom_histogram()

#Calculating variance of response
responseVar <- round(var(lifeExpectancy$Life_expectancy),digits=3)
responseVar

#sample correlation
cor(lifeExpectancy$Life_expectancy, lifeExpectancy$Adult_Mortality)

