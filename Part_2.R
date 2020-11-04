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

pval <- pf(F.test, 3, 2481, lower.tail = FALSE)
pval

####covariance matrix b.hat####

bCov <- cov(fullMatrix)

s.b <- sqrt(diag(bCov))
t.stat <- b/s.b
stnd.error <- b/t.stat

####hypothesis tests of coefficients####

XtX <- t(X)%*%X
var.b <- MSE*solve(XtX)
s.b <- sqrt(diag(var.b))
t.values <- b/s.b
t.values

p.values <- pt(t.values, 3)
p.values

####Xhs matrix####

Xh <- matrix(X[5,], ncol=1)
Y.hat <- t(Xh)%*%b
s.Y.hat <- sqrt(MSE*(t(Xh)%*%solve(XtX)%*%Xh))
lower.CI.mean <- Y.hat - qt(0.975, 76)*s.Y.hat 
upper.CI.mean <- Y.hat + qt(0.975, 76)*s.Y.hat

Xh.mat <- matrix(t(X[c(5,500, 1000, 1020, 2000),]), ncol=5)
Y.hat <- t(Xh.mat)%*%b

#Working-Hotelling

n <- nrow(lifeExpectancy)
p <- nrow(Xh.mat)
s.Y.hat <- sqrt(MSE*diag(t(Xh.mat)%*%solve(XtX)%*%Xh.mat))

W2.val <- p*qf(0.95,p,n-p) 
W.val <- sqrt(W2.val)

lower.CB <- Y.hat - W.val*s.Y.hat
upper.CB <- Y.hat + W.val*s.Y.hat

#Bonferroni

g <- ncol(Xh.mat)
conf.level <- 1-(0.05/(2*g))
B.val <- qt(conf.level,n-p) 

lower.CB <- Y.hat - B.val*s.Y.hat
upper.CB <- Y.hat + B.val*s.Y.hat

####hypothesis tests with lm####

dfFull <- as.data.frame(fullMatrix)
lmFit <- lm(Y ~ X1+X2+X3, dfFull)
lmFit_y1 <- lm(Y ~ X1, dfFull)
lmFit_y2 <- lm(Y ~ X2, dfFull)
lmFit_y3 <- lm(Y ~ X3, dfFull)

#1st bullet

anova(lmFit) #SSE = 177141 df = 2481
anova(lmFit_y1) #SSE = 181906 df = 2483
anova(lmFit_y2) #SSE = 223285 df = 2483
anova(lmFit_y3) #SSE = 226119 df = 2483

#p-1 = 3
#n-p = 2481

#X1=X3=0
F.test <- ((181906-177141)/(2483-2481))/(177141/2481)
F.test>Fdist
#reject H0
pval <- pf(F.test, 3, 2481, lower.tail = FALSE)
pval

#X2=X3=0
F.test <- ((223285-177141)/(2483-2481))/(177141/2481)
F.test>Fdist
#reject H0
pval <- pf(F.test, 3, 2481, lower.tail = FALSE)
pval

#X1=X2=0
F.test <- ((226119-177141)/(2483-2481))/(177141/2481)
F.test>Fdist
#reject H0
pval <- pf(F.test, 3, 2481, lower.tail = FALSE)
pval

#2nd bullet

#b1 is estimated to be closer to 0 than 1, but I wanted to see if
#it could potentially be close to 1

#H0: X1 = 1, Ha: X1 != 1, alpha = 0.05
t.star <- (3.009e-04 - 1)/sqrt(1.194e-05)
t.dist <- qt(0.975, n-2)
t.star > t.dist
#fail to reject H0
pval <- pt(t.star, n-2)
pval

#3rd bullet: slide 36 class 20, lm(y~ Xc+x3) where Xc = x1+x2
lmFit_Xc <- lm(Y ~ I(X1+X2)+X3, dfFull)
summary(lmFit_Xc)
