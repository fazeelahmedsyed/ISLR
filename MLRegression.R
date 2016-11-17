# This function is for Multiple Linear Regression
# For convenience: after running the code, you can run the command fix(models) to have a look at the models used here.

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

#Checking colinearity between variables within the model
library(car)
vif(lm.fit)

#Regressing medv on all variables but rad
lm.fit1 <- update(lm.fit, ~.-rad)
vif(lm.fit1)

#To denote an interaction term we use x1:x2. 
lm.fit2 <- lm(medv~ lstat*age) #lstat*age = lstat + age + lstat:age

#To check non-linear transformations of lstat
#We use I() because ^ has a different meaning in R as opposed to denoting power
lm.fit3 <- lm(medv~ lstat + I(lstat^2)) 

#Comparing models with analysis of variance
lm.fit4 <- lm(medv ~ lstat)
anova(lm.fit4, lm.fit3)# Ho: Both models equally fit, H-alpha: Model2 is better 

#To check for higher powers of lstat
lm.fit5 <- lm(medv~poly(lstat, 5))
lm.fit6 <- lm(medv~poly(lstat, 6))
lm.fit8 <- lm(medv~poly(lstat,8))
rbind(summary(lm.fit5)$coefficients, summary(lm.fit6)$coefficients, summary(lm.fit8)$coefficients)
#the comparision shows powers upto 5 are relevant only 

#For the convenience of the readers
a <- c("lm.fit","lm.fit1", "lm.fit2", "lm.fit3", "lm.fit4", "lm.fit5", "lm.fit6", "lm.fit8")
b <- c("medv~ all predictors", "medv~ .-rad", "medv~ lstat+age+lstat:age", "medv~ lstat+I(lstat^2)", "medv~ lstat", "medv~poly(lstat,5)", "medv~ poly(lstat, 6)","medv~ poly(lstat, 8)")
models <- cbind(a,b)
colnames(models) <- c("Model name", "Model description")
rm(a)
rm(b)


