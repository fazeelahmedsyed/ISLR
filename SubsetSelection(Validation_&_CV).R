#This script is for Best Subset Selection using Validation Sets and Cross Validation Approaches

#Preparing data and Loading Libraries
library(ISLR)
Hitters <- Hitters
Hitters <- Hitters[complete.cases(Hitters),]

#Creating a train and test vector
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), replace = T)
test = (!train)

#SUBSET SELECTION BASED ON VALIDATION SET ERRORS

#Running Best Subset Selection on train data
regfit5 <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)
test_mat <- model.matrix(Salary ~., data = Hitters[test,]) #model.matrix creates a matrix for the predictors. It automatically makes dummy variables for the predictors. Try test_mat[1:5,2:5] == (Hitters[test,])[1:5,1:4]

#Running a loop that provides a set of Cross validation errors for each model
validation_errors <- numeric(length = 19)

for (i in 1:19){
        Coeffs <- coef(regfit5, id = i)
        regfit5_preds <- test_mat[,names(Coeffs)] %*% Coeffs #%*% is a symbol used to denote matrix mutliplication.run ?'%*%' for information
        validation_errors[i] <- mean((Hitters$Salary[test] - regfit5_preds)^2)
        }

coef(regfit5, which.min(validation_errors)) #Indicates the best model (which has 10 variables and has the lowest validation error)

regfit5 <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(regfit5, 10) #Indicates the model we should use utilizing Best Selection technique based on Validation Errors

#SUBSET SELECTION BASED ON CROSS VALIDATION

k = 10          #Defining the number of folds needed
set.seed(1)

#Creating a subset vector that defines the folds
folds <- sample(1:k, nrow(Hitters), replace = T) #This creates each fold of a similar size. 

#Creating empty matrix for the collection of CV errors
cv_errors = matrix(NA, nrow = k, ncol = 19, dimnames = list(NULL, paste(1:19)))
val_errors = numeric(length = 19)

#Creating function for collecting CV errors for our best subset selection
for (j in 1:k){
        best_fit <- regsubsets(Salary ~ . , data = Hitters[folds!=j,], nvmax = 19)
        formula <- as.formula(best_fit$call[[2]])
        test_matrix <- model.matrix(formula, data = Hitters[folds==j,])
        for (i in 1:19){
                coefs <- coef(best_fit, i)
                best_fit_preds <- test_matrix[,names(coefs)] %*% coefs
                val_errors[i] <- mean((Hitters$Salary[folds==j] - best_fit_preds)^2) 
                }
        cv_errors[j,] <- val_errors
        }
cv_errors <- colMeans(cv_errors)

#Viewing lowest CV error graphically
plot(cv_errors, type = 'l', col = 'dark grey', xlab = "No. of variables in Model")
points(which.min(cv_errors),cv_errors[which.min(cv_errors)], col ='red', pch = 20)

#Choosing the model
best_fit <- regsubsets(Salary ~. , data = Hitters, nvmax = 19)
coef(best_fit, 11) #required model
