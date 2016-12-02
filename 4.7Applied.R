#This is for ISLR 4.7 Applied

#10(a)
library(ISLR)
Weekly <- Weekly
summary(Weekly)
pairs(Weekly)
Weekly_cor <- cor(Weekly[,-9])
sort(Weekly_cor, decreasing = T)
which(Weekly_cor > 0.5 & Weekly_cor != 1, arr.ind = T)
plot(Weekly$Year, Weekly$Volume, pch = 4, col = "dark grey")
print("Both graphical and numberical analysis suggests there is an obvious trend of Volume over the Years. This appears to be a proportional relationship graphically")

#10(b)
glm_fit <- glm(Direction ~. -Year -Today, data = Weekly, family = "binomial")
summary(glm_fit)
print("Variables 'Lag2' and the 'Intercept' appear to be statistically significant with confidence levels of 95% and 99% respectively.")

#10(c)
contrasts(Weekly$Direction)
glm_probs <- predict(glm_fit, type = "response")
glm_preds <- rep("Down", nrow(Weekly))
glm_preds[glm_probs > 0.5] = "Up"
Prediction <- glm_preds
Actual <- Weekly$Direction
glm_conf <- addmargins(table(Prediction,Actual))
glm_conf
glm_Error <- mean(Prediction != Actual)
glm_Perf <- mean(Prediction == Actual)
glm_TPR <- glm_conf[2,2]/glm_conf[3,2]
glm_FPR <- glm_conf[2,1]/glm_conf[3,1]
glm_prec_Up <- glm_conf[2,2]/glm_conf[2,3] 
glm_prec_Down <- glm_conf[1,1]/glm_conf[1,3] 
print("Here the financial analyst would be intested in predicting when the market would go down, in order to mitigate risks. It appears we are better able to identify down movements (53% of the times, correct) with the model as opposed to randomly (9% of the times, correct)")

#Cleaning environment
a <- character
a <- ls()
a <- a[-(grep("^Weekly",a))]
rm(list = a)

#10(d)
test <- Weekly$Year > 2008
Direction_test <- Weekly$Direction[test]
glm_fit <- glm(Direction ~. -Year -Today, data = Weekly, family = "binomial", subset = (!test))
summary(glm_fit)
glm_probs <- predict(glm_fit, Weekly[test,], type = "response")
contrasts(Weekly$Direction)
glm_preds <- rep("Down",length(glm_probs))
glm_preds[glm_probs > 0.5] = "Up"
glm_conf <- addmargins(table(Prediction = glm_preds, Actual = Direction_test))
glm_conf
glm_correct <- mean(glm_preds == Direction_test) #(glm_conf[1,1]+glm_conf[2,2])/glm_conf[3,3]
glm_correct

#10(e)
library(MASS)
Weekly_test <- Weekly[test,]
lda_fit <- lda(Direction ~ . -Year -Today, data = Weekly, subset = (!test))
lda_fit
lda_preds <- predict(lda_fit, Weekly_test)$class 
lda_conf <- addmargins(table(Prediction = lda_preds, Actual = Direction_test))
lda_conf
lda_correct <- mean(lda_preds == Direction_test)
lda_correct

#10(f)
qda_fit <- qda(Direction ~ . -Year -Today, data = Weekly, subset = (!test))
qda_fit
qda_preds <- predict(qda_fit, Weekly_test)$class
qda_conf <- addmargins(table(Prediction = qda_preds, Actual = Direction_test))
qda_conf
qda_correct <- mean(qda_preds == Direction_test)
qda_correct

#10(g)
library(class)
Weekly_st <- scale(Weekly[,c(-1,-8,-9)])
X_train <- Weekly_st[!test,]
X_test <- scale(Weekly_test[,c(-1,-8,-9)])
Y_train <- Weekly$Direction[!test]
Y_test <- Direction_test
set.seed(1)
knn_preds <- knn(X_train,X_test, Y_train, k = 1)
knn_conf <- addmargins(table(Prediction = knn_preds, Actual = Direction_test))
knn_conf
knn_correct <- mean(knn_preds == Direction_test)
knn_correct

#10(h)
c(glm = glm_correct, lda = lda_correct, qda = qda_correct, knn = knn_correct)
print("logistic regression, LDA and KNN(k = 1) appear to give the same kind of results. They can predict correctly where the market would move 46% of the times. Nevertheless an analysis of other measures needs to take place for proper assessment.")

#Cleaning environment
a <- character
a <- ls()
rm(list = a)

#11
#Preparing Dataset
Auto <- Auto
summary(Auto)

#11(a)
mpg01 <- rep(0, length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)]= 1
Auto <- data.frame(Auto,mpg01)
rm(mpg01)

#11(b)
Auto_cor <- cor(Auto[,-9])
Auto_cor[,9]
Auto$mpg01 <- as.factor(Auto$mpg01)
par(mfrow = c(3,3))
plot(Auto$mpg01, Auto$cylinders)
plot(Auto$mpg01, Auto$displacement)
plot(Auto$mpg01, Auto$horsepower)
plot(Auto$mpg01, Auto$weight)
plot(Auto$mpg01, Auto$acceleration)
plot(Auto$mpg01, Auto$year)
plot(Auto$mpg01, Auto$origin)
print("high magnitudes of correlation with Cylinders, displacement, horsepower, weight")

#11(c)
test <- Auto$year > 79
Auto_test <- (Auto[,-9])[test,]
mpg01_test <- Auto$mpg01[test]

#11(d)
lda_fit <- lda(mpg01 ~ cylinders+displacement+horsepower+weight, data = Auto, subset = (!test))
lda_fit
contrasts(Auto$mpg01)
lda_preds <- predict(lda_fit, Auto_test)
lda_conf <- addmargins(table(Prediction = lda_preds$class, Actual = mpg01_test))
lda_conf
lda_error <- mean(lda_preds$class != mpg01_test)
lda_error

#11(e)
qda_fit <- qda(mpg01~ cylinders+displacement+horsepower+weight, data = Auto, subset = (!test))
qda_fit
qda_preds <- predict(qda_fit, Auto_test)
qda_conf <- addmargins(table(Prediction = qda_preds$class, Actual = mpg01_test))
qda_conf
qda_error <- mean(qda_preds$class != mpg01_test)
qda_error

#11(f)
glm_fit <- glm(mpg01~ cylinders+displacement+horsepower+weight, data = Auto, family = 'binomial',subset = (!test))
summary(glm_fit)
glm_probs <- predict(glm_fit, Auto_test)
glm_preds <- rep(1, length(glm_probs))
glm_preds[glm_probs > 0.5] = 2
glm_conf <- addmargins(table(Prediction = glm_preds, Actual = mpg01_test))
glm_conf
glm_error <- mean(glm_preds != mpg01_test)
glm_error

#11(g)
Auto_st <- scale(Auto[,c(2,3,4,5)])
X_train <- Auto_st[!test,]
X_test <- Auto_st[test,]
Y_train <- Auto$mpg01[!test]
knn1_preds <- knn(X_train, X_test, Y_train, k =1) 
knn3_preds <- knn(X_train, X_test, Y_train, k =3)
knn5_preds <- knn(X_train, X_test, Y_train, k =5)
knn7_preds <- knn(X_train, X_test, Y_train, k =7)
knn9_preds <- knn(X_train, X_test, Y_train, k =9)
knn11_preds <- knn(X_train, X_test, Y_train, k =11)

knn1_error <- mean(knn1_preds != mpg01_test)
knn3_error <- mean(knn3_preds != mpg01_test)
knn5_error <- mean(knn5_preds != mpg01_test)
knn7_error <- mean(knn7_preds != mpg01_test)
knn9_error <- mean(knn9_preds != mpg01_test)
knn11_error <- mean(knn11_preds != mpg01_test)

c("k=1" = knn1_error,"k=3" = knn3_error,"k=5" = knn5_error,"k=7" = knn7_error,"k=9" = knn9_error,"k=11" = knn11_error)

#Cleaning environment
a <- character
a <- ls()
rm(list = a)

#13

##Preparing data
Boston <- Boston
summary(Boston)
crim01 <- rep(0, length(Boston$crim))
crim01[Boston$crim > median(Boston$crim)] = 1
Boston <- data.frame(crim01, Boston)

##Exploring dependencies within variables
Boston_cor <- cor(Boston)
Boston_cor[,1]
Boston$crim01 <- as.factor(Boston$crim01)
for (i in 2:ncol(Boston)){
        a <- paste("Boston$",sep = "", colnames(Boston)[i])
        plot(Boston$crim01, Boston[,i], xlab = "Bostoncrim01", ylab = a)
}
print("Having had a look at correlations and the graphs, we can observe that crim01 has high magnitudes of correlations with the predictors: indus, nox, age, dis, rad and tax")
print("The variables also make intuitive sense when correlations for crim are observed along of that of crim01")

##Creating Training and Test set
Boston_train <- Boston[1:430,]
Boston_test <- Boston[431:506,]

##Fitting Logistic Regression Model
glm_fit <- glm(crim01 ~ indus+nox+age+dis+rad+tax, data = Boston_train, family = "binomial")
summary(glm_fit)
print("only nox and rad appear to be significant estimators")
glm_probs <- predict(glm_fit, Boston_test, type = "response")
glm_preds <- rep(0,length(glm_probs))
glm_preds[glm_probs > 0.5] = 1
glm_conf <- addmargins(table(Prediction = glm_preds, Actual = Boston_test$crim01))
glm_conf
glm_correct <- mean(glm_preds == Boston_test$crim01)
glm_correct
print("the model predicts an accurate response 93% of the times, based on the test error")

###checking other possibilities to improve model
glm2_fit <- glm(crim01 ~ nox+rad, data = Boston_train, family = "binomial")
summary(glm2_fit)
print("All estimators used in the model appear to be statistically significant")
glm2_probs <- predict(glm2_fit, Boston_test, type = "response")
glm2_preds <- rep(0,length(glm2_probs))
glm2_preds[glm2_probs > 0.5] = 1
glm2_conf <- addmargins(table(Prediction = glm2_preds, Actual = Boston_test$crim01))
glm2_conf
glm2_correct <- mean(glm2_preds == Boston_test$crim01)
glm2_correct
print("The model worsens which shows there are other variables or combination of variables which were left out initially that should be included in the model to explain the good fit in glm_fit.")

#Cleaning environment
a <- character
a <- ls()
a <- a[grep("^glm2",a)]
rm(list = a)
rm(i)

##Fitting an LDA model
library(MASS)
lda_fit <- lda(crim01 ~ indus+nox+age+dis+rad+tax, data = Boston_train)
lda_fit
contrasts(Boston_train$crim01)
lda_preds <- predict(lda_fit, Boston_test)
lda_conf <- addmargins(table(Prediction = lda_preds$class, Actual = Boston_test$crim01))
lda_conf
lda_correct <- mean(lda_preds$class == Boston_test$crim01)
lda_correct
print("The model is worse off than glm_fit in predicting the correct outcome")

##Fitting a QDA model
qda_fit <- qda(crim01 ~ indus+nox+age+dis+rad+tax, data = Boston_train)
qda_fit
contrasts(Boston_train$crim01)
qda_preds <- predict(qda_fit, Boston_test)
qda_conf <- addmargins(table(Prediction = qda_preds$class, Actual = Boston_test$crim01))
qda_conf
qda_correct <- mean(qda_preds$class == Boston_test$crim01)
qda_correct
print("The model is better than lda but worse than glm in predicting the outcome")

##Fitting a model using the KNN classifier for values of k 
library(class)
Boston_train_st <- scale(Boston_train[,-1])
Boston_test_st <- scale(Boston_test[,-1])
X_train <- Boston_train_st[,c(3,5,7,8,9,10)]
X_test <- Boston_test_st[,c(3,5,7,8,9,10)]
Y_train <- Boston_train$crim01
Y_test <- Boston_test$crim01

set.seed(1)
knn1_preds <- knn(X_train, X_test, Y_train, k = 1)
knn1_correct <- mean(knn1_preds == Y_test)
knn1_correct

set.seed(1)
knn3_preds <- knn(X_train, X_test, Y_train, k = 3)
knn3_correct <- mean(knn3_preds == Y_test)
knn3_correct

set.seed(1)
knn7_preds <- knn(X_train, X_test, Y_train, k = 7)
knn7_correct <- mean(knn7_preds == Y_test)
knn7_correct

print("the knn classifiers do a terrible job at predicting the outcomes compared to other models")