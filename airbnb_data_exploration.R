getwd()
setwd("/Users/madhurip/Desktop/BUAN 6356/")
airbnb.df <- read.csv("seattle.csv")
View(airbnb.df)

#HISTOGRAMS

xmin= min(airbnb.df$reviews)
xmax=max(airbnb.df$reviews)

hist(airbnb.df$reviews, xlab = "reviews", xlim = c(xmin,xmax))

hist(airbnb.df$overall_satisfaction, xlab = "overall_satisfaction")

hist(airbnb.df$bedrooms, xlab = "bedrooms")

hist(airbnb.df$bathrooms, xlab = "bathrooms")

hist(airbnb.df$accommodates, xlab = "accommodates")

print(max(airbnb.df$price))
hist(airbnb.df$price, xlab = "price", xlim = c(min(airbnb.df$price),max(airbnb.df$price)))

# BOXPLOTS

boxplot(airbnb.df$price ~ airbnb.df$room_type, xlab = "Room_Type", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$address, xlab = "Address", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$rate_type, xlab = "Rate_Type", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$currency, xlab = "Currency", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$accommodates, xlab = "Accommodates", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$bedrooms, xlab = "Bedrooms", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$bathrooms, xlab = "Bathrooms", ylab = "Price")

boxplot(airbnb.df$price ~ airbnb.df$overall_satisfaction, xlab = "overall_satisfaction", ylab = "Price")

airbnb_num.df <- airbnb.df[,c(5:10)]
View(airbnb_num.df)
airbnb_num.df <- na.omit(airbnb_num.df)

# DESCRIPTIVE STATISTICS

summary(airbnb_num.df)
help(sapply)
airbnb_stats.df <- data.frame(  mean=sapply(airbnb_num.df, mean), 
                            median=sapply(airbnb_num.df, median), 
                            sd=sapply(airbnb_num.df, sd), 
                            variance=sapply(airbnb_num.df, var),
                            min=sapply(airbnb_num.df, min), 
                            max=sapply(airbnb_num.df, max), 
                            count=sapply(airbnb_num.df, length),
                            miss.val=sapply(airbnb_num.df, function(x) 
                              sum(length(which(is.na(x))))))
print(airbnb_stats.df)

# NORMALIZED DATA

airbnb.df.norm <- sapply(airbnb_num.df, scale)
airbnb.df.norm <- scale(airbnb.df.norm, center = TRUE, scale = TRUE )
View(airbnb.df.norm)

# CORRELATION MATRIX

library(ggplot2)
library(reshape) 
cor.mat <- round(cor(airbnb_num.df),2) 
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))

#SCATTER PLOTS

ggplot(airbnb_num.df) + geom_point(aes(x = accommodates, y = price), colour = "navy", alpha = 0.7)

ggplot(airbnb_num.df) + geom_point(aes(x = bedrooms, y = price), colour = "navy", alpha = 0.7)

ggplot(airbnb_num.df) + geom_point(aes(x = bathrooms, y = price), colour = "navy", alpha = 0.7)

ggplot(airbnb_num.df) + geom_point(aes(x = reviews, y = price), colour = "navy", alpha = 0.7)

ggplot(airbnb_num.df) + geom_point(aes(x = overall_satisfaction, y = price), colour = "navy",alpha = 0.7)


# SCATTER PLOT MATRIX 

library(GGally)
ggpairs(airbnb_num.df)



