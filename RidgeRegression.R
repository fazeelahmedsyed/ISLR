#This script is for the practice of Ridge Regression and Lasso

#Loading Libraries
library(ISLR)
library(glmnet)

#Creating Dataset
Hitters <- Hitters
Hitters <- Hitters[complete.cases(Hitters),]

#Creating dependencies for performing Ridge regression and Lasso
x <- model.matrix(Salary ~., data = Hitters) #glmnet can only understand quantitative variables, thus model.matrix provides us our required x matrix
x <- x[,-1]

y <- Hitters$Salary

#Performing Ridge Regression
grid <- 10^seq(10,-2,length =100)                       #Making a grid of lambda values
ridge_fit <- glmnet(x,y,alpha = 0, lambda = grid)       #We did not standardize the variables because glmnet does that on its own, alpha here identifies if we want to do ridge regression(0) or lasso(1). 
dim(coef(ridge_fit))                                    #coef gives the beta coefficients associated with each value of lambda

#Assessing lambdas and coefficients
ridge_fit$lambda[50]                                    #Provides the value of 50th lambda(out of 100) in the grid. This should give us low values of Beta coefs as Lambda is large (compared to the 100th lambda).
coef(ridge_fit)[,50]                                    #Provides the coefficient values at Lambda = 11497.6(50th lambda)
rbind((coef(ridge_fit)[,100]),(coef(ridge_fit)[,50]))   #For comparison. Lambda at 50th place shrinks the coefficients close to zero.
predict(ridge_fit, s =  type = "coefficients")          #Provides prediction of coefficients for a new value of lambda that we provide (here, lambda = 190). check ?predict.glmnet

#Finding Validation set error for lambda = 4
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)

ridge_fit2 <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge_fit2_preds <- predict(ridge_fit2, newx = x[test,], s = 4)
val4_error <- mean((ridge_fit2_preds- y[test])^2)
val4_error

#Assessing validation error for lambda = 0 (least square regression)
ridge_fit2_preds <- predict(ridge_fit2, newx = x[test,], s = 0, exact = T) #exact= T for values outside of grid. If exact = F the value of lambda to be used will be interpolated using grid.
val0_error <- mean((ridge_fit2_preds - y[test])^2)
val0_error

#Finding Best lambda by Cross validation
set.seed(1)
ridge_cv <- cv.glmnet(x[train,],y[train], alpha = 0)    #Letting glmnet choose its own lambda values
plot(ridge_cv)

best_lambda <- ridge_cv$lambda.min                                     #gives out the lambda with lowest training error

ridge_fit2_preds <- predict(ridge_fit2, s = best_lambda, newx = x[test,])
valbest_error <- mean((ridge_fit2_preds - y[test])^2)                   #gives a lower validation error than least squares

#Choosing best model based on our best lambda
ridge_fit3 <- glmnet(x,y,alpha = 0)
predict(ridge_fit3, type = "coefficients", s = best_lambda)             #Provides best model using ridge regression


