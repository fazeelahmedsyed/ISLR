#This function is for Logistic Regression

#Loading Libraries
library(ISLR)

#Preparing Dataset
Smarket <- Smarket
a <- c("The year that the observation was recorded","Percentage return for previous day","Percentage return for 2 days previous","Percentage return for 3 days previous","Percentage return for 4 days previous","Percentage return for 5 days previous","Volume of shares traded (number of daily shares traded in billions)","Percentage return for today","A factor with levels Down and Up indicating whether the market had a positive or negative return on a given day")
Smarket_desc <- data.frame(cbind(colnames(Smarket),a))
colnames(Smarket_desc) <- c("variable","Variable Description")
rm(a)

#Creating Correlation matrix and identifying highly correlated variables
Smarket_cor <- cor(Smarket[,-9])
sort(Smarket_cor, decreasing = T)
which(Smarket_cor > 0.5 & Smarket_cor < 1, arr.ind = T)
colnames(Smarket_cor)[c(1,7)]

#Observing the highly correlated variables
plot(Smarket$Volume, col = "dark grey", pch = 4)
print("Volume appears to increase with time")

#Running Logistic Regression on the Dataset
attach(Smarket)
glm.fit <- glm(Direction ~. -Year -Today, data = Smarket, family = binomial)
summary(glm.fit)

#Looking at probabilities P(Y = 1|X)
glm.fit_probs <- predict(glm.fit, type = "response")
summary(glm.fit_probs)

#Creating vector for predictions of Market Movement
glm.fit_preds <- rep("Down",length(glm.fit_probs))
glm.fit_preds[glm.fit_probs > 0.5] = "Up"
summary(glm.fit_preds)

#Creating confusion matrix
glm.fit_conf <- addmargins(table(glm.fit_preds, Direction)) 
glm.fit_conf

#Storing values for errors

#Training Error Rate
glm.fit_Eff <- (glm.fit_conf[1,1]+glm.fit_conf[2,2])/glm.fit_conf[3,3]
glm.fit_Error <- 1 - glm.fit_Eff
glm.fit_Eff
glm.fit_Error

#True Positive Rate (sensitivity)
glm.fit_sens <- glm.fit_conf[2,2]/glm.fit_conf[3,2]
glm.fit_TPR <- glm.fit_sens
glm.fit_TPR

#False Positive Rate (1 - Specificity)
glm.fit_spec <- glm.fit_conf[1,1]/glm.fit_conf[3,1]
glm.fit_FPR <- 1 - glm.fit_spec
glm.fit_FPR

#Cleaning environment
a <- ls()
a <- a[-(grep("Smarket$",a))]
rm(list = a)
rm(a)

#Splitting Smarket into data before 2005 and data in and after 2005
#Year<2005 is training set
#!(Year<2005) is testing set

#Training set
attach(Smarket)
train <- Year < 2005
Smarket_2005 <- Smarket[!train,]
glm.fit2 <- glm(Direction ~ . -Year -Today, data = Smarket, family = binomial, subset = train)
summary(glm.fit2)$coefficients
glm.fit2_probs <- predict(glm.fit2, type = "response")
glm.fit2_preds <- rep("Down", length(glm.fit2_probs))
glm.fit2_preds[glm.fit2_probs > 0.5] = "Up"
Direction_2001 <- Direction[train]
Direction_2005 <- Direction[!train]


        #Creating confusion matrix
        glm.fit2_conf <- addmargins(table(glm.fit2_preds, Direction_2001)) 
        glm.fit2_conf
        
        #Storing values for errors
        
        #Training Error Rate
        glm.fit2_Eff <- (glm.fit2_conf[1,1]+glm.fit2_conf[2,2])/glm.fit2_conf[3,3]
        glm.fit2_Error <- 1 - glm.fit2_Eff
        
        #True Positive Rate (sensitivity)
        glm.fit2_sens <- glm.fit2_conf[2,2]/glm.fit2_conf[3,2]
        glm.fit2_TPR <- glm.fit2_sens
        
        #False Positive Rate (1 - Specificity)
        glm.fit2_spec <- glm.fit2_conf[1,1]/glm.fit2_conf[3,1]
        glm.fit2_FPR <- 1 - glm.fit2_spec
        
#Testing set
glm.fit2_test_probs <- predict(glm.fit2, Smarket_2005, type = "response")
glm.fit2_test_preds <- rep("Down", length(glm.fit2_test_probs))
glm.fit2_test_preds[glm.fit2_test_probs > 0.5] = "Up"

        #Creating confusion matrix
        glm.fit2_test_conf <- addmargins(table(glm.fit2_test_preds, Direction_2005)) 
        glm.fit2_test_conf
        
        #Storing values for errors
        
        #Training Error Rate
        glm.fit2_test_Eff <- (glm.fit2_test_conf[1,1]+glm.fit2_test_conf[2,2])/glm.fit2_test_conf[3,3]
        glm.fit2_test_Error <- 1 - glm.fit2_test_Eff
        
        #True Positive Rate (sensitivity)
        glm.fit2_test_sens <- glm.fit2_test_conf[2,2]/glm.fit2_test_conf[3,2]
        glm.fit2_test_TPR <- glm.fit2_test_sens
        
        #False Positive Rate (1 - Specificity)
        glm.fit2_test_spec <- glm.fit2_test_conf[1,1]/glm.fit2_test_conf[3,1]
        glm.fit2_test_FPR <- 1 - glm.fit2_test_spec

#compiling results        
glm.fit2_results <- cbind(c(glm.fit2_Error,glm.fit2_TPR, glm.fit2_FPR), c(glm.fit2_test_Error,glm.fit2_test_TPR, glm.fit2_test_FPR))
rownames(glm.fit2_results) <- c("Error Rate", "TP/P", "FP/N")
colnames(glm.fit2_results) <- c("Train", "Test")
glm.fit2_results

#Cleaning environment
a <- c("glm.fit2_Eff", "glm.fit2_Error", "glm.fit2_sens", "glm.fit2_TPR", "glm.fit2_spec","glm.fit2_FPR","glm.fit2_test_Eff", "glm.fit2_test_Error", "glm.fit2_test_sens", "glm.fit2_test_TPR", "glm.fit2_test_spec","glm.fit2_test_FPR")
rm(list = a)
rm(a)
