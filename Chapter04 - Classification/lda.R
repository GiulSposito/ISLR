library(ISLR)
library(MASS)


model <- lda(Direction~Lag1+Lag2, data=Smarket, subset = Year<2005)
model

plot(model)

Smarket.2005 <- subset(Smarket, Year==2005)
pred <- predict(model, Smarket.2005)
head(as.data.frame(pred))
