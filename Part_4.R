library(caret)

# Re-creating data set with interaction terms ---------------------------------

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
