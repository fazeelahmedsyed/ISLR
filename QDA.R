#This function is for Quadratic Discriminant Analysis

#Loading Libraries and preparing Dataset
library(ISLR)
library(MASS)
Smarket <- Smarket
a <- c("The year that the observation was recorded","Percentage return for previous day","Percentage return for 2 days previous","Percentage return for 3 days previous","Percentage return for 4 days previous","Percentage return for 5 days previous","Volume of shares traded (number of daily shares traded in billions)","Percentage return for today","A factor with levels Down and Up indicating whether the market had a positive or negative return on a given day")
Smarket_desc <- data.frame(cbind(colnames(Smarket),a))
colnames(Smarket_desc) <- c("variable","Variable Description")
rm(a)

#Fitting Model for Lag1 and Lag2 and observations before 2005
attach(Smarket)
train <- Year < 2005
qda_fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda_fit

#Calculating predictions and confusion matrix
Smarket_2005 <- Smarket[!train,]
Actual <- Smarket_2005$Direction
qda_preds <- predict(qda_fit, Smarket_2005)
Prediction <- qda_preds$class
qda_conf <- addmargins(table(Prediction,Actual))
qda_conf

#Calculating Errors
qda_Error <- (qda_conf[1,2]+qda_conf[2,1])/qda_conf[3,3]
qda_TPR <- qda_conf[2,2]/qda_conf[3,2]
qda_FPR <- qda_conf[2,1]/qda_conf[3,1]

#Alternate Error checking
1 - qda_Error
mean(Prediction == Actual)
1 - qda_Error == mean(Prediction == Actual)