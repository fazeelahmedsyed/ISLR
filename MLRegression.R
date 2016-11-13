# This function is for Multiple Linear Regression

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

attach(Boston)

#Fitting a multiple regression model of medv on all other variables in Boston

lm.fit <- lm(medv ~ ., data = Boston)

#Creating all Statistics as separate objects

b <- summary(lm.fit)

for (i in 1:length(b)){
        k <- unlist(b[i])
        assign(paste("lm.fit_",names(b)[i],sep=""), k)
}

#Cleaning the environment

lm.fit_Summary <- b
rm(b)
rm(k)
rm(i)
