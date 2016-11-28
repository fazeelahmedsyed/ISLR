#This function is for Linear Discriminant Analysis 

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
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

#LDA coefficients provide the values of the linear discriminants such that d(x) =-0.64(X1)-0.51(X2) or d(x)=[Lag1 Lag2]T.[-0.64 -0.51] 

#Making Predictions on test data
Smarket_2005 <- Smarket[!train,]
lda.fit_preds <- predict(lda.fit, Smarket_2005)

lda.fit_preds_summary <- data.frame(Smarket_2005$Lag1,Smarket_2005$Lag2,lda.fit_preds[[2]],lda.fit_preds[[1]],lda.fit_preds[[3]])
colnames(lda.fit_preds_summary) <- c("Lag1","Lag2","P(Down)","P(Up)","Prediction","Discriminant")

#Checking Errors
Actual <- Smarket_2005$Direction
Prediction <- lda.fit_preds$class 
lda.fit_conf <- addmargins(table(Prediction,Actual))
rm(list= c("Actual","Prediction"))
lda.fit_Error <- (lda.fit_conf[1,2]+lda.fit_conf[2,1])/lda.fit_conf[3,3] 
lda.fit_TPR <- lda.fit_conf[2,2]/lda.fit_conf[3,2] 
lda.fit_FPR <- lda.fit_conf[2,1]/lda.fit_conf[3,1]

#Changing Thresholds

##Threshold changes and the corresponding changes can be observed by isolating the predictions where
##posterior porbabilities for whichever group were on that threshold. 
## for e.g
sum(lda.fit_preds$posterior[,1] > 0.9)
##here the threshold for classifying an observation as 'Down' has been increased to 0.9. 
##The example only provides the number of these predictions to calculate error rates.