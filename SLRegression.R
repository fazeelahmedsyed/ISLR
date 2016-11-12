#This function is for practice of Simple Linear Regression

#Preparing Data

library(MASS)
library(ISLR)

#Reading the file is not needed if you are loading ISLR. ISLR already includes Boston as a dataset.
Boston <- read.csv("Boston.csv")
Boston <- Boston[,-1]

#Creating Variable list
a <- c("per capita crime rate by town.", "proportion of residential land zoned for lots over 25,000 sq.ft.", "proportion of non-retail business acres per town.","Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)","nitrogen oxides concentration (parts per 10 million)","average number of rooms per dwelling.","proportion of owner-occupied units built prior to 1940.","weighted mean of distances to five Boston employment centres.","index of accessibility to radial highways.","full-value property-tax rate per $10,000.","pupil-teacher ratio by town","1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.","lower status of the population (percent).","median value of owner-occupied homes in $1000s.")
Boston_desc <- cbind(colnames(Boston), a)
colnames(Boston_desc) <- c("Variable","Variable Description")
rm(a)

#Storing fitted linear model into a lm class variable
lm.fit <- lm(medv ~ lstat)

#Checking summaries
Summary(lm.fit)

#Making interval predictions
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

#Plotting the two variables
plot(lstat, medv, col = "blue", pch = 4) #col = color, pch describes symbol to use
abline(lm.fit, lwd = 3, col = "blue") #lwd dictates how thick the line will be on the graph 

#Viewing exploratory plots
par(mfrow = c(2,2)) #splitting R's plot screen into a 2x2 structure to view 4 graphs simultaneously 
plot(lm.fit, col = "dark grey", pch = 4)

#Checking residual plots
plot(predict(lm.fit), residuals(lm.fit), col="dark grey", pch = 4) # y-cap vs actual residuals
plot(predict(lm.fit), rstudent(lm.fit), col = "dark grey", pch = 4) # y-cap vs studentized residuals

#Checking leverage statistics for the data points
plot(hatvalues(lm.fit), col="dark grey",pch=4)
hatvalues(lm.fit)[which.max(hatvalues(lm.fit))] #checking which data point has the largest leverage statistic