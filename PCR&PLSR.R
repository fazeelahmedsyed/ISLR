#This script is for PCR and PLSR

#Loading Libraries
library(ISLR)
library(pls)

#Creating Dataset
Hitters <- Hitters
Hitters <- Hitters[complete.cases(Hitters),]

#Performing PCR
set.seed(2)
pcr_fit <- pcr(Salary ~ . , data = Hitters, scale = T, validation = "CV") #here scale = T standardizes all the measurements, validation = CV tells pcr to calculate 10-foldcv errors for all possible values of M.(M = no. of components)
summary(pcr_fit)

##Exploring options for best Model based on CV error
validationplot(pcr_fit, val.type = "MSEP")

##Attempting a validation set approach to PCR
set.seed(1)
train <- sample(1:nrow(Hitters), nrow(Hitters)/2)
test <- (-train)

pcr2_fit <- pcr(Salary ~ . , data = Hitters, subset = train, scale = T, validation = "CV")  
validationplot(pcr2_fit, val.type = 'MSEP')                     #Lowest CV MSE occurs at M = 7

x <- model.matrix(Salary ~., data = Hitters)[,-1]
pcr2_preds <- predict(pcr2_fit, x[test,], ncomp = 7)
val_error <- mean((pcr2_preds - Hitters$Salary[test])^2)
val_error

##Using all data to fit our final model
pcr_final <- pcr(Salary ~ ., data = Hitters, scale = T, ncomp = 7)
summary(pcr_final)
pcr_final$loadings

#Performing PLSR
set.seed(1)
pls_fit <- plsr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls_fit)
validationplot(pls_fit)                                 #Lowest CV error occurs at M = 10, but we choose M = 2 to reduce dimensionality

#Calculating validation error at M = 2
pls_preds <- predict(pls_fit, newdata = x[test,], ncomp = 2)
pls_valerror <- mean((pls_preds - Hitters$Salary[test])^2)
pls_valerror

#Fitting final model
pls_final <- plsr(Salary~., data = Hitters, scale = T, ncomp = 2)
summary(pls_final)
