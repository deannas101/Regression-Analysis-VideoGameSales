library(car)
library(caret)
library(lmtest)
library(olsrr)
library(tidyverse)

# Re-creating data set with interaction terms ----------------------------------

Life_Expectancy_Data <- as.data.frame(Life_Expectancy_Data)

exp_data <- Life_Expectancy_Data[, 4:6]
exp_data$V1 <- Life_Expectancy_Data$Hepatitis_B
exp_data$V2 <- Life_Expectancy_Data$Measles
exp_data$V3 <- Life_Expectancy_Data$BMI
exp_data$V4 <- Life_Expectancy_Data$Diphtheria
colnames(exp_data) <- c(
  "Y", "X1", "X2", "X3", "X4", "X5", "X6"
)

# imputing missing values
process <- preProcess(exp_data, method = "knnImpute")
exp_data <- predict(process, exp_data)

X1X2 <- exp_data$X1 * exp_data$X2
X1X3 <- exp_data$X1 * exp_data$X3
X1X4 <- exp_data$X1 * exp_data$X4
X1X5 <- exp_data$X1 * exp_data$X5
X1X6 <- exp_data$X1 * exp_data$X6

X2X3 <- exp_data$X2 * exp_data$X3
X2X4 <- exp_data$X2 * exp_data$X4
X2X5 <- exp_data$X2 * exp_data$X5
X2X6 <- exp_data$X2 * exp_data$X6

X3X4 <- exp_data$X3 * exp_data$X4
X3X5 <- exp_data$X3 * exp_data$X5
X3X6 <- exp_data$X3 * exp_data$X6

X4X5 <- exp_data$X4 * exp_data$X5
X4X6 <- exp_data$X4 * exp_data$X6

X5X6 <- exp_data$X5 * exp_data$X6

exp_data <- cbind(
  exp_data, X1X2, X1X3, X1X4, X1X5, X1X6, X2X3, X2X4, X2X5, X2X6, X3X4,
  X3X5, X3X6, X4X5, X4X6, X5X6
)

cor(exp_data)

# reducing multicollinearity by centering and scaling
process <- preProcess(exp_data, method = c("center", "scale"))
exp_data <- predict(process, exp_data)

cor(exp_data)

# assessing if any interaction terms should be included ------------------------

first_model <- lm(Y ~ ., exp_data)
summary(first_model)

# remove X2X6
second_model <- lm(
  Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X1X2 + X1X3 + X1X4 + X1X5
    + X1X6 + X2X3 + X2X4 + X2X5 + X3X4 + X3X5 + X3X6 + X4X5 + X4X6 + X5X6,
  exp_data
)
summary(second_model)

# remove X2
third_model <- lm(Y ~ X1 + X3 + X4 + X5 + X6 + X1X2 + X1X3 + X1X4 + X1X5 + X1X6
  + X2X3 + X2X4 + X2X5 + X3X4 + X3X5 + X3X6 + X4X5 + X4X6 + X5X6, exp_data)
summary(third_model)

# remove X3
fourth_model <- lm(Y ~ X1 + X4 + X5 + X6 + X1X2 + X1X3 + X1X4 + X1X5 + X1X6
  + X2X3 + X2X4 + X2X5 + X3X4 + X3X5 + X3X6 + X4X5 + X4X6 + X5X6, exp_data)
summary(fourth_model)

# remove X4X5
fifth_model <- lm(Y ~ X1 + X4 + X5 + X6 + X1X2 + X1X3 + X1X4 + X1X5 + X1X6
  + X2X3 + X2X4 + X2X5 + X3X4 + X3X5 + X3X6 + X4X6 + X5X6, exp_data)
summary(fifth_model)

# remove X1X2
sixth_model <- lm(Y ~ X1 + X4 + X5 + X6 + X1X3 + X1X4 + X1X5 + X1X6 + X2X3
  + X2X4 + X2X5 + X3X4 + X3X5 + X3X6 + X4X6 + X5X6, exp_data)
summary(sixth_model)

# remove X1X5 - final model
seventh_model <- lm(Y ~ X1 + X4 + X5 + X6 + X1X3 + X1X4 + X1X6 + X2X3 + X2X4
  + X2X5 + X3X4 + X3X5 + X3X6 + X4X6 + X5X6, exp_data)
summary(seventh_model)

# viewing interaction effects --------------------------------------------------

# assumption checking: round 1 -------------------------------------------------

# data preparation
X <- exp_data %>%
  select(
    X1, X4, X5, X6, X1X3, X1X4, X1X6, X2X3, X2X4, X2X5, X3X4, X3X5, X3X6,
    X4X6, X5X6
  ) %>%
  as.matrix()
X <- cbind(1, X)

resid_fit <- resid(seventh_model)

# linearity - q-q plot
sqrt_fit <- lm(Y ~ sqrt(abs(X[, -1])), data = exp_data)
sqrt_resid_fit <- resid(sqrt_fit)
qqnorm(sqrt_resid_fit)
qqline(sqrt_resid_fit)

# normality - shapiro-wilk
shapiro.test(resid_fit) # not normally distributed

# homoscedasticity - BP and BF
bptest(seventh_model) #error terms don't have constant variance

# influential observations - leverage, cook's, DFFITS, DFBETAS

# multicollinearity - VIFs
vif(seventh_model) #max VIF < 10, no serious multicollinearity issue

# box-cox transformation and assumption checking: round 2 ----------------------

#transformations are warranted due to a lack of normality across the data
process <- preProcess(exp_data, method = "BoxCox")
exp_data <- predict(process, exp_data)

# data preparation
X <- exp_data %>%
    select(
        X1, X4, X5, X6, X1X3, X1X4, X1X6, X2X3, X2X4, X2X5, X3X4, X3X5, X3X6,
        X4X6, X5X6
    ) %>%
    as.matrix()
X <- cbind(1, X)

final_model <- lm(Y ~ X1 + X4 + X5 + X6 + X1X3 + X1X4 + X1X6 + X2X3 + X2X4
                    + X2X5 + X3X4 + X3X5 + X3X6 + X4X6 + X5X6, exp_data)
resid_fit <- resid(final_model)

# linearity - q-q plot
sqrt_fit <- lm(Y ~ sqrt(abs(X[, -1])), data = exp_data)
sqrt_resid_fit <- resid(sqrt_fit)
qqnorm(sqrt_resid_fit)
qqline(sqrt_resid_fit)

# normality - shapiro-wilk
shapiro.test(resid_fit) # not normally distributed

# homoscedasticity - BP and BF
bptest(final_model) #error terms don't have constant variance

# influential observations - leverage, cook's, DFFITS, DFBETAS
inv.XX <- solve(t(X) %*% X)
H <- X %*% inv.XX %*% t(X)
lev <- diag(H)
p <- ncol(X)
n <- nrow(X)
which(lev > 2 * p / n)

leveragePlots(final_model)

hist(lev, main = "Histogram of leverage points", xlab = "leverage")
abline(v = 2 * p / n, lty = 2, lwd = 2, col = "red")

ols_plot_cooksd_chart(final_model)

ols_plot_dffits(final_model)

ols_plot_dfbetas(final_model)

# there's too many outliers to force a linear model to work on this data

# multicollinearity - VIFs
vif(final_model) #max VIF < 10, no serious multicollinearity issue
