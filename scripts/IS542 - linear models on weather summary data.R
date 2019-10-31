# Assignment 6 - Linear Regression
#
# by Adarsh Agarwal 10/17/2019
# R version 3.6.1


#load some data - https://www.kaggle.com/smid80/weatherww2/download
# The summary of weather data will be a csv file of around 11 MB
df = read.csv("Summary of Weather.csv", header = TRUE)
summary(df)

# Distribution of explanatory and response variables
hist(df$MinTemp)
hist(df$MaxTemp)

# What does relationship between the two variables look like?
plot(df$MinTemp, df$MaxTemp)

# Applying simple linear model to the data
lm.fit = lm(MaxTemp~MinTemp,data=df)
summary(lm.fit)
plot(df$MinTemp, df$MaxTemp)
abline(lm.fit)
yhat = predict(lm.fit)
plot(df$MinTemp,yhat)
res = df$MinTemp-yhat
plot(df$MinTemp,res)


# Adding curvature to the model
lm2.fit = lm(MaxTemp~MinTemp+I(MinTemp^2),data=df)
summary(lm2.fit)
plot(df$MinTemp, df$MaxTemp)
yhat = predict(lm2.fit)
plot(df$MinTemp,yhat)
res = df$MinTemp-yhat
plot(df$MinTemp,res)

# adding another variable to the model
lm3.fit = lm(MaxTemp~MinTemp+I(MinTemp^2)+MeanTemp,data=df)
summary(lm3.fit)
plot(df$MinTemp, df$MaxTemp)
plot(df$MeanTemp, df$MaxTemp)
yhat = predict(lm3.fit)
plot(df$MinTemp,yhat)
res = df$MinTemp-yhat
plot(df$MinTemp,res)