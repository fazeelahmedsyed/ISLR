#This script for the practice of Best subset selection

#Loading Libraries
library(ISLR)

#Creating Data set
Hitters <- Hitters

#Checking for missing data and resolving
is.na(Hitters$Salary) #Only Salary has missing values. A better way to go about checking is sum(!complete.cases(Hitters))
Hitters <- na.omit(Hitters) # Alternatively Hitters <- Hitters[complete.cases(Hitters),]

#BEST SUBSET SELECTION

library(leaps)

#Creating Models
regfit <- regsubsets(Salary ~ . , data = Hitters)
summary(regfit) #By default the regsubsets provides models up to 8 variables, we can extend this limitation

regfit2 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
summary(regfit2)
summary(regfit2)$rsq #Rsq for all 19 models
names(summary(regfit2)) #List of available statistics for each model

#Plotting Models to see best model
par(mfrow = c(2,2))
plot(summary(regfit2)$rsq, xlab = "Number of var", ylab = "R squared", col = 'dark grey', type = 'l')
plot(summary(regfit2)$adjr2, xlab = "Number of var", ylab = "Adj R squared", col = 'dark grey', type = "l")

#Identifying best model on the basis of adjusted R sq
which.max(summary(regfit2)$adjr2)
points(11, summary(regfit2)$adjr2[11], col = "red", pch = 20)

#Using regsubsets in-built plot function to see best model
par(mfrow = c(2,2))
plot(regfit2, scale = "r2")
plot(regfit2, scale = "adjr2")
plot(regfit2, scale = "bic") #Each selected variable is given by a dark square(not white). Here the best model is a combination of AtBat,Hits,Walks, CRBI, DivisionW,PutOuts.

coef(regfit2, 6) #Calling coefficients for the 6-var model. As our plot suggests best(lowest) BIC is on the 6-var model.

#FORWARD AND BACKWARD SELECTION

regfit3 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
regfit4 <- regsubsets(Salary ~. , data = Hitters, nvmax = 19, method = "backward")
