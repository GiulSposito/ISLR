library(ISLR)

# S&P Stock dataset
str(Smarket)

# view all features
pairs(Smarket, col=Smarket$Direction)


# Logistic Regression
model <- glm( Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family = binomial )

summary(model)

probs <- predict(model, type="response")
hist(probs)

pred = ifelse(probs>0.5, "Up","Down")
table(pred, Smarket$Direction)
mean(pred==Smarket$Direction)

train <- Smarket$Year < 2005
model <- glm( Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family = binomial, subset=train)
probs <- predict(model, newdata = Smarket[!train,], type="response")
hist(probs)
pred <- ifelse(probs > 0.5, "Up", "Down")
direction.2005 <- Smarket$Direction[!train]
table(pred,direction.2005)
mean(pred==direction.2005)


model <- glm( Direction~Lag1+Lag2,
              data=Smarket, family = binomial, subset=train)
probs <- predict(model, newdata = Smarket[!train,], type="response")
hist(probs)
pred <- ifelse(probs > 0.5, "Up", "Down")
direction.2005 <- Smarket$Direction[!train]
table(pred,direction.2005)
mean(pred==direction.2005)

summary(model)

