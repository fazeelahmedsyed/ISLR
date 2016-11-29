#This function is for K-Nearest Neighbors

#loading libraries and preparing dataset
library(ISLR)
library(MASS)
library(class)
Smarket <- Smarket
a <- c("The year that the observation was recorded","Percentage return for previous day","Percentage return for 2 days previous","Percentage return for 3 days previous","Percentage return for 4 days previous","Percentage return for 5 days previous","Volume of shares traded (number of daily shares traded in billions)","Percentage return for today","A factor with levels Down and Up indicating whether the market had a positive or negative return on a given day")
Smarket_desc <- data.frame(cbind(colnames(Smarket),a))
colnames(Smarket_desc) <- c("variable","Variable Description")
rm(a)
train <- Smarket$Year<2005
Smarket_2005 <- Smarket[!train,]
Direction_2005 <- Smarket_2005$Direction

#Making Variables required for KNN fit
knn_train <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
knn_test <- cbind(Smarket_2005$Lag1,Smarket_2005$Lag2)
Direction_train <- Smarket$Direction[train]
Direction_test <- Direction_2005

#Fitting model
set.seed(1)
knn_preds <- knn(knn_train, knn_test, Direction_train, k=1)

#Making Confusion matrix
Prediction <- knn_preds
Actual <- Direction_2005
knn_conf <- addmargins(table(Prediction, Actual))
rm(list = c("Actual", "Prediction"))

#calculating Errors
knn_Error <- (knn_conf[1,2]+knn_conf[2,1])/knn_conf[3,3]
knn_TPR <- knn_conf[2,2]/knn_conf[3,2]
knn_FPR <- knn_conf[2,1]/knn_conf[3,1]

#Efficiency of the model
#1 - knn_Error
mean(knn_preds == Direction_2005)
mean(knn_preds != Direction_2005)

#Applying KNN on new Data set

##Cleaning the environment
a <- ls()
rm(list = a)
rm(a)

##Loading Dataset
###visit for details on the data: "http://liacs.leidenuniv.nl/~puttenpwhvander/library/cc2000/data.html"
Caravan <- Caravan

#Potential Problem with KNN:
#Since KNN works by finding mean observations around which observations of predictors lie,
#it is highly dependent upon distances between the points. This creates a problem in KNN when
#it considers distances based on units. If the units are small (for e.g 1000 pence), it might
#consider it a large distance compared to a large unit (for e.g 50 years). (Alternatively, it will
#produce an entirely different fit if the units were in Rs and age in seconds. 
#Thus, we standardize our data set. 

Caravan_st <- scale(Caravan[,-86]) 

##Creating Variables for KNN fitting
X_test <- Caravan_st[1:1000,]
X_train <- Caravan_st[1001:nrow(Caravan_st),]
Y_test <- Caravan[,86][1:1000]
Y_train <- Caravan[,86][1001:nrow(Caravan)]

#Classifying using KNN Classifier
set.seed(1)
knn_preds <- knn(X_train,X_test,Y_train,k = 1)

#Preparing confusion matrix
Prediction <- knn_preds
Actual <- Y_test
knn_conf <- addmargins(table(Prediction,Actual))
knn_conf
rm(list=c("Actual","Prediction"))

#Checking Efficiency
mean(knn_preds == Y_test)

#Checking Error
mean(knn_preds != Y_test)

#Calculating important measures
knn_Error <- mean(knn_preds != Y_test)
knn_TPR <- knn_conf[2,2]/knn_conf[3,2]
knn_FPR <- knn_conf[2,1]/knn_conf[3,1]
knn_prec <- knn_conf[2,2]/knn_conf[2,3] #precision = TP/P*

#Trying other values for K

        #k = 5
        set.seed(1)
        knn5_preds <- knn(X_train,X_test,Y_train,k = 5)
        
        Prediction <- knn5_preds
        Actual <- Y_test
        knn5_conf <- addmargins(table(Prediction,Actual))
        knn5_conf
        rm(list=c("Actual","Prediction"))
        
        knn5_prec <- knn5_conf[2,2]/knn5_conf[2,3] #precision = TP/P*
        knn5_prec
        
        #k = 7
        set.seed(1)
        knn7_preds <- knn(X_train,X_test,Y_train,k = 7)
        
        Prediction <- knn7_preds
        Actual <- Y_test
        knn7_conf <- addmargins(table(Prediction,Actual))
        knn7_conf
        rm(list=c("Actual","Prediction"))
        
        knn7_prec <- knn7_conf[2,2]/knn7_conf[2,3] #precision = TP/P*
        knn7_prec
        
