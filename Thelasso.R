#This script is for the practice of the Lasso

#Loading Libraries
library(ISLR)
library(glmnet)

#Creating Dataset
Hitters <- Hitters
Hitters <- Hitters[complete.cases(Hitters),]

#Creating dependencies

x <- model.matrix(Salary~., data = Hitters)[,-1]
y <- Hitters$Salary
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
grid <- 10^seq(10,-2, length = 100)

#Performing the lasso on our data

lasso_fit <- glmnet(x[train,], y[train],  alpha = 1, lambda = grid)

#Finding best lambda based on lowest training error
set.seed(1)
lasso_cv <- cv.glmnet(x[train,],y[train],alpha = 1)
best_lambda <- lasso_cv$lambda.min

#Finding validation error of best lambda

lasso_fit_preds <- predict(lasso_fit, newx = x[test,], s = best_lambda)
validation_error <- mean((lasso_fit_preds - y[test])^2)

#Coefficients based on best lambda

lasso_final <- glmnet(x,y,alpha = 1, lambda = grid)
predict(lasso_final, s = best_lambda, type = "coefficients")            #Behold! Feature Selection! 
