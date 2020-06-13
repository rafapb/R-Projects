# Data from: https://www.kaggle.com/c/bike-sharing-demand/data

# The main purpose of this script is to do some exploratory data analysis and
# build a simple linear regression model for forecasting rental demand in the Capital Bikeshare
# program in Washington, D.C. 

library(ggplot2)
library(dplyr)

bike_df <- read.csv("CSV files for ML Projects/bikeshare.csv")
head(bike_df)

# EDA
pl <- ggplot(bike_df, aes(x=temp, y=count))

print(pl + geom_point(alpha=0.3, aes(color=temp)) + theme_bw())

# CONVERT TO POSIXct()
bike_df$datetime <- as.POSIXct(bike_df$datetime)

pl <- ggplot(bike_df, aes(x=datetime, y=count)) + geom_point(aes(color=temp), alpha=0.5)
pl <- pl + scale_color_continuous(low="#55D8CE", high="#FF6E2E") + theme_bw()

print(pl)

# CORRELATION
cor(bike_df[,c("temp", "count")])

pl <- ggplot(bike_df, aes(x=factor(season), count)) + geom_boxplot(aes(color=factor(season))) + theme_bw()

print(pl)

# FEATURE ENGINEERING
bike_df$hour <- sapply(bike_df$datetime, function(x){format(x, "%H")})
head(bike_df)

# SCATTERPLOT WORKING DAYS
pl <- ggplot(filter(bike_df, workingday==1), aes(hour,count)) 
pl <- pl + geom_point(position = position_jitter(w=1, h=0), aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours=c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl <- pl + theme_bw()

print(pl)

# SCATTERPLOT NON-WORKING DAYS
pl <- ggplot(filter(bike_df, workingday==0), aes(hour,count)) 
pl <- pl + geom_point(position = position_jitter(w=1, h=0), aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours=c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl <- pl + theme_bw()

print(pl)

# BUILD MODEL USING COUNT AND TEMP
temp.model <- lm(count ~ temp, data=bike_df)

print(summary(temp.model))

# HOW MANY BIKE RENTAL COUNTS AT 25C?

# Intercept + Slope * x
6.0462 + 9.1705*25

temp.test <- data.frame(temp=c(25))
predict(temp.model, temp.test)

# CHANGE THE HOUR COLUMN TO A COLUMN OF NUMERIC VALUES
bike_df$hour <- sapply(bike_df$hour, as.numeric)
head(bike_df)

# BUILD MODEL WITH ALL FEATURES EXCEPT CASUAL, REGISTERED, DATETIME AND ATEMP
model <- lm(count ~ . -casual - registered -datetime -atemp, data = bike_df)

print(summary(model))

# END