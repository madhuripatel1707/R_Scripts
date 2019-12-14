setwd("/Users/madhurip/Desktop/BUAN 6356/")
airbnb_full.df <- read.csv("seattle.csv")
View(airbnb_full.df)
actual_pred_airbnb.df <- na.omit(airbnb_full.df)
View(actual_pred_airbnb.df)

selected.var <- c(5:10,12,13)
View(selected.var)
# partition the data 
set.seed(123)  # set seed for reproducing the partition
numberOfRows <- nrow(actual_pred_airbnb.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- actual_pred_airbnb.df[train.index, selected.var]
valid.df <- actual_pred_airbnb.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 10 predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
airbnb_full.lm <- lm(price ~ bedrooms + bathrooms + accommodates + overall_satisfaction + reviews , data = train.df)

#  use options(scipen = TRUE) to display in scientific notation
# use options(scipen = 999) to not use scientific notation.
#options(scipen = 999)
summary(airbnb_full.lm)


# use predict() to make predictions on a new set. 
#options(scipen=999, digits = 0)
airbnb_full.lm.pred <- predict(airbnb_full.lm, valid.df)
summary(airbnb_full.lm.pred)

#calculate rmse by hand to show how it works.  this will typically be done using accuracy()
residuals <- valid.df$price - airbnb_full.lm.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = airbnb_full.lm.pred, "Actual" = valid.df$price,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
View(df)


library(forecast)
library(leaps)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error
#options(scipen=999, digits = 3)

accuracy(airbnb_full.lm.pred, valid.df$price)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

# use regsubsets() in package leaps to run an exhaustive search. 
# regsubsets() will calculate adjusted r2 for every possible combination of predictors
# unlike with lm(), categorical predictors must be turned into dummies manually.


search <- regsubsets(price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2

#par(mfrow=c(1,1))
plot(search, scale="r2")

#best 4
airbnb.best.lm <- lm(price ~ reviews + bedrooms + bathrooms + accommodates, data = train.df)
summary(airbnb.best.lm)
airbnb.best.lm.pred <- predict(airbnb.best.lm, valid.df)
accuracy(airbnb.best.lm.pred, valid.df$price)

#library(gains)
library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1
write.csv(df, file = "df.csv")




