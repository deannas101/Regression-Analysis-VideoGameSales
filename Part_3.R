library(car)
library(lmtest)
library(olsrr)
library(tidyverse)

# predictors: measles, alcohol, hepatitis b, adult mortality

# reduced data set with observations we're interested in
red_data <- Life_Expectancy_Data %>%
  select(Life_expectancy, Adult_Mortality, Alcohol, Hepatitis_B, Measles) %>%
  drop_na()

#### 1####
X <- as.matrix(red_data[, -1])
X <- cbind(1, X)
inv.XX <- solve(t(X) %*% X)
H <- X %*% inv.XX %*% t(X)
lev <- diag(H)

#### 2####
pairs(red_data)

#### 3####
lmfit <- lm(Life_expectancy ~ ., data = red_data)
res <- rstudent(lmfit)
fit.Y <- fitted(lmfit)

plot(res ~ fit.Y, xlab = "Fitted values", ylab = "Residual", main = "Residual Plot against Fitted values")
abline(h = 0, lwd = 2)
plot(res ~ red_data$Adult_Mortality, xlab = "X", ylab = "Residual", main = "Residual Plot against Adult Mortality")
abline(h = 0, lwd = 2)
plot(res ~ red_data$Alcohol, xlab = "X", ylab = "Residual", main = "Residual Plot against Alcohol")
abline(h = 0, lwd = 2)
plot(res ~ red_data$Hepatitis_B, xlab = "X", ylab = "Residual", main = "Residual Plot against Hepatitis B")
abline(h = 0, lwd = 2)
plot(res ~ red_data$Measles, xlab = "X", ylab = "Residual", main = "Residual Plot against Measles")
abline(h = 0, lwd = 2)

#### 4####
sqrt.lmfit <- lm(Life_expectancy ~ sqrt(X[, -1]), data = red_data)
res.sqrt.lmfit <- resid(sqrt.lmfit)
qqnorm(res.sqrt.lmfit)
qqline(res.sqrt.lmfit)

#### 5####
p <- ncol(X)
n <- nrow(X)
which(lev > 2 * p / n)

leveragePlots(lmfit)

hist(lev, main = "Histogram of leverage points", xlab = "leverage")
abline(v = 2 * p / n, lty = 2, lwd = 2, col = "red")

#### 6####
ols_plot_cooksd_chart(lmfit)

#### 7####
ols_plot_dffits(lmfit)

#### 8####
ols_plot_dfbetas(lmfit)

#### 9####
# lack-of-fit: don't think this is necessary

#### 10####
# BP and BF test
bptest(lmfit)

lm.res <- resid(lmfit)

# BF Test - Adult_Mortality
mean.life <- mean(red_data$Adult_Mortality)

sort.life <- sort(red_data$Adult_Mortality)
order.life <- order(red_data$Adult_Mortality)

sort.res <- lm.res[order.life]

group1.index <- which((sort.life < mean.life) | (sort.life == mean.life))
group1 <- sort.life[group1.index]

group2.index <- which((sort.life > mean.life))
group2 <- sort.life[group2.index]

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

t.test(d1, d2)

# BF Test - Alcohol
mean.life <- mean(red_data$Alcohol)

sort.life <- sort(red_data$Alcohol)
order.life <- order(red_data$Alcohol)

sort.res <- lm.res[order.life]

group1.index <- which((sort.life < mean.life) | (sort.life == mean.life))
group1 <- sort.life[group1.index]

group2.index <- which((sort.life > mean.life))
group2 <- sort.life[group2.index]

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

t.test(d1, d2)

# BF Test - Hepatitis_B
mean.life <- mean(red_data$Hepatitis_B)

sort.life <- sort(red_data$Hepatitis_B)
order.life <- order(red_data$Hepatitis_B)

sort.res <- lm.res[order.life]

group1.index <- which((sort.life < mean.life) | (sort.life == mean.life))
group1 <- sort.life[group1.index]

group2.index <- which((sort.life > mean.life))
group2 <- sort.life[group2.index]

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

t.test(d1, d2)

# BF Test - Measles
mean.life <- mean(red_data$Measles)

sort.life <- sort(red_data$Measles)
order.life <- order(red_data$Measles)

sort.res <- lm.res[order.life]

group1.index <- which((sort.life < mean.life) | (sort.life == mean.life))
group1 <- sort.life[group1.index]

group2.index <- which((sort.life > mean.life))
group2 <- sort.life[group2.index]

group1.res <- sort.res[group1.index]
group2.res <- sort.res[group2.index]

d1 <- abs(group1.res - median(group1.res))
d2 <- abs(group2.res - median(group2.res))

t.test(d1, d2)

#### 11####
shapiro.test(lm.res)
