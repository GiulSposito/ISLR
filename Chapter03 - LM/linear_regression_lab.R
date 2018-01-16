library(MASS)
library(ISLR)
library(ggplot2)
str(Boston)

plot(medv~lstat, Boston)
fit1 <- lm(medv~lstat, Boston)
summary(fit1)
abline(fit1, col="Red")
names(fit1)
confint(fit1)

## cool
predict(fit1, newdata = data.frame(lstat=c(5,10,15)), interval = "confidence")

fit2 <- lm(medv~lstat+age, Boston)
summary(fit2)

fit3 <- lm(medv~.,Boston)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)


fit4 <- lm(medv~lstat*age,Boston)
summary(fit4)
  

fit5 <- lm(medv~lstat+I(lstat^2), Boston)
summary(fit5)
plot(fit5)

par(mfrow=c(1,1))


x <- as.factor(c(0,1,1,1,2,2,1,2,3,3,2,2,1,1,0,0,0,NA,1,2,1,1,1,NA,3))

x  <- contrasts(x)
?contrasts

d <- data_frame(
  name=c("Giul","Ju","Carol","Sil"),
  status=as.factor(c("N","S","N","C"))
)

d

as_data_frame(contrasts(d$status))
library(caret)
install.packages("caret")


str(caret::dummyVars(name~.,d))
f(d)
