library(forecast) 
library(leaps)
library(ggplot2)
getwd()
setwd("/Users/madhurip/Desktop/BUAN 6356")
regairbnb.df <- read.csv("seattle.csv")
reg_airbnb.df <- na.omit(regairbnb.df)
View(reg_airbnb.df)


set.seed(1)
numberOfRows <- nrow(reg_airbnb.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)

train.df <- reg_airbnb.df[train.index,]
View(train.df)
validation.df <- reg_airbnb.df[-train.index,]
View(validation.df)

reg_airbnb1.lm <- lm(formula = price ~ room_type, data = train.df)
reg_airbnb2.lm <- lm(formula = price ~ address, data = train.df)
reg_airbnb3.lm <- lm(formula = price ~ reviews, data = train.df)
reg_airbnb4.lm <- lm(formula = price ~ overall_satisfaction, data = train.df)
reg_airbnb5.lm <- lm(formula = price ~ accommodates, data = train.df)
reg_airbnb6.lm <- lm(formula = price ~ bedrooms, data = train.df)
reg_airbnb7.lm <- lm(formula = price ~ bathrooms, data = train.df)
reg_airbnb8.lm <- lm(formula = price ~ bedrooms+bathrooms, data = train.df)
reg_airbnb9.lm <- lm(formula = price ~ bedrooms+bathrooms+accommodates, data = train.df)
reg_airbnb10.lm <- lm(formula = price ~ bedrooms+bathrooms+accommodates+reviews, data = train.df)
reg_airbnb11.lm <- lm(formula = price ~ bedrooms+bathrooms+accommodates+reviews+overall_satisfaction, data = train.df)
#cooks.distance(reg_airbnb11.lm)

plot(reg_airbnb10.lm$residuals,pch = 16, col= "blue")

options(scipen = TRUE)
summary(reg_airbnb11.lm)
summary(reg_airbnb10.lm)
summary(reg_airbnb1.lm)
summary(reg_airbnb2.lm)
summary(reg_airbnb3.lm)
summary(reg_airbnb4.lm)
summary(reg_airbnb5.lm)
summary(reg_airbnb6.lm)
summary(reg_airbnb7.lm)
summary(reg_airbnb8.lm)
summary(reg_airbnb9.lm)

#library(forecast)
reg_airbnb1.pred <- predict(reg_airbnb11.lm, validation.df)
summary(reg_airbnb1.pred)
accuracy(reg_airbnb1.pred, validation.df$price)

#reg_airbnb2.pred <- predict(reg_airbnb2.lm, validation.df)
#accuracy(reg_airbnb2.pred, validation.df$price)

reg_airbnb3.pred <- predict(reg_airbnb3.lm, validation.df)
accuracy(reg_airbnb3.pred, validation.df$price)

reg_airbnb4.pred <- predict(reg_airbnb4.lm, validation.df)
accuracy(reg_airbnb4.pred, validation.df$price)

reg_airbnb5.pred <- predict(reg_airbnb5.lm, validation.df)
accuracy(reg_airbnb5.pred, validation.df$price)

reg_airbnb6.pred <- predict(reg_airbnb6.lm, validation.df)
accuracy(reg_airbnb6.pred, validation.df$price)

reg_airbnb7.pred <- predict(reg_airbnb7.lm, validation.df)
accuracy(reg_airbnb7.pred, validation.df$price)

reg_airbnb8.pred <- predict(reg_airbnb8.lm, validation.df)
accuracy(reg_airbnb8.pred, validation.df$price)

reg_airbnb.lm <- lm(formula = price ~ ., data = train.df[,c(5:10)])
summary(reg_airbnb.lm)
housePriceAll.pred <- predict(reg_airbnb.lm, validation.df)
accuracy(housePriceAll.pred, validation.df$price)

# stepwise regression

reg_airbnb.lm.step <- step(reg_airbnb.lm, direction = "forward")
summary(reg_airbnb.lm.step)
reg_airbnb.lm.step.pred <- predict(reg_airbnb.lm.step, validation.df)
accuracy(reg_airbnb.lm.step.pred, validation.df$price)

reg_airbnb.lm.step <- step(reg_airbnb.lm, direction = "backward")
summary(reg_airbnb.lm.step)
reg_airbnb.lm.step.pred <- predict(reg_airbnb.lm.step, validation.df)
accuracy(reg_airbnb.lm.step.pred, validation.df$price)

all.residuals <- validation.df$price - reg_airbnb.lm.step.pred
all.residuals.df <- data.frame(all.residuals)
ggplot(all.residuals.df) + geom_histogram(aes(x = all.residuals), binwidth = 1)

