#This is for the practice of Cross Validation and Bootstrap methods of Resampling

#Loading Libraries and Data
library(ISLR)
Auto <- Auto

#CROSS VALIDATION

##Creating train and test subset 1
set.seed(1)
train <- sample.int(392, size = 196)

##Observing MSE for different Linear Models

lm_fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
lm_preds <- predict(lm_fit, Auto)[-train]
lm_MSE <- mean((Auto$mpg[-train] - lm_preds)^2)
lm_MSE

lm2_fit <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
lm2_preds <- predict(lm2_fit, Auto)[-train]
lm2_MSE <- mean((Auto$mpg[-train] - lm2_preds)^2)
lm2_MSE

lm3_fit <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
lm3_preds <- predict(lm3_fit, Auto)[-train]
lm3_MSE <- mean((Auto$mpg[-train] - lm3_preds)^2)
lm3_MSE

##Creating train and test subset 2
set.seed(2)
train2 <- sample.int(392, size = 196)

##Observing MSE for different Linear Models
lm4_fit <- lm(mpg ~ horsepower, data = Auto, subset = train2)
lm4_preds <- predict(lm4_fit, Auto)[-train2]
lm4_MSE <- mean((Auto$mpg[-train2] - lm4_preds)^2)
lm4_MSE

lm5_fit <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train2)
lm5_preds <- predict(lm5_fit, Auto)[-train2]
lm5_MSE <- mean((Auto$mpg[-train2] - lm5_preds)^2)
lm5_MSE

lm6_fit <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train2)
lm6_preds <- predict(lm6_fit, Auto)[-train2]
lm6_MSE <- mean((Auto$mpg[-train2] - lm6_preds)^2)
lm6_MSE

Results_CV <- data.frame(rbind(c(lm_MSE, lm2_MSE, lm3_MSE), c(lm4_MSE, lm5_MSE, lm6_MSE)))
colnames(Results_CV) <- c("Linear", "Quadratic", "Cubic")
rownames(Results_CV) <- c("CV1", "CV2")
Results_CV

#Cleaning Environment
a <- character
a <- ls()
a <- a[-grep("^Auto", a)]
rm(list = a)

#LOOCV

##Lodaing libraries
library(boot)

##Model
lm_fit <- glm(mpg ~ horsepower, data = Auto) #using glm() because it is compatible with boot library functions
lm_LOOCV <- cv.glm(Auto, lm_fit)
lm_LOOCV_error <- lm_LOOCV$delta
lm_LOOCV_error

##Automating model
Results_LOOCV <- rep(0,5)
for (i in 1:5){
        lm_fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
        Results_LOOCV[i] <- (cv.glm(Auto, lm_fit))$delta[1]
}
names(Results_LOOCV) <- c("Lin","Quad","Cub","Quar","Quin")        
Results_LOOCV

#Cleaning Environment
a <- character
a <- ls()
a <- a[-grep("^Auto", a)]
rm(list = a)

#K-FOLD CV

set.seed(17)
Results_10fold <- rep(0,10)
for (i in 1:10){
        lm_fit <- glm(mpg~ poly(horsepower, i), data = Auto)
        Results_10fold[i] <- (cv.glm(Auto, lm_fit))$delta[1]
}
names(Results_10fold) <- c(1:10)
Results_10fold

#Cleaning Environment
a <- character
a <- ls()
a <- a[-grep("^Auto", a)]
rm(list = a)

#BOOTSTRAP

##Forming a function
## for linear function

boot_fn <- function(data, train){
        lm_fit <- lm(mpg ~ horsepower, data = data, subset = train)
        lm_coef <- coef(lm_fit)
        lm_coef
}
        
##bootstrapping
###boot(data, statistic, iterations,...)
set.seed(1)
boot(Auto, boot_fn, 1000)

## for quadratic function

boot_fn2 <- function(data, train){
        lm_fit <- lm(mpg ~ poly(horsepower,2, raw = TRUE), data = data, subset = train)
        lm_coef <- coef(lm_fit)
        lm_coef
}

set.seed(1)
boot(Auto, boot_fn2, 1000)

############################################
#EXPERIMENTAL#

bootstrap <- function(data, power, itr){
        booter <- function(data, train){
                lm_fit <- lm(mpg ~ poly(horsepower,power, raw = TRUE), data = data, subset = train)
                lm_coef <- coef(lm_fit)
                lm_coef
        }

        boot(data, booter, itr)
}
bootstrap(Auto, 2, 1000)

############################################
#EXPERIMENTAL#
library(class)
library(MASS)

Default <- Default
Default$default <- as.numeric(Default$default)
Default$student <- as.numeric(Default$student)
Default_cor <- cor(Default)
Default_cor

Default$default <- as.factor(Default$default)
Default$student <- as.numeric(Default$student)


bootstrap <- function(data, model, train, P = 0.5,  ){
        
        library(MASS)
        library(class)
        
        booter1 <- function (data, train, P){
                glm_fit <- glm(default ~ balance + income, data = data, family = "binomial", subset = train)
                glm_probs <- predict(glm_fit, Default[!train,], type = response)
                glm_preds <- rep(1,length((Default[!train,])))
                glm_preds[glm_probs > P, ] = 2
                mean(glm_preds != Default$default[!train])
        }
        
        booter2 <- function(){
                lda_fit <- lda(default ~ balance + income, data = data)
                lda_preds <- predict(lda_fit, Default[!train])
                mean(lda_preds$class != Default$default[!train])
        }
        
        booter3 <- function(){
                qda_fit <- qda(default ~ balance + income, data = data)
                qda_preds <- predict(qda_fit, Default[!train])
                mean(qda_preds$class != Default$default[!train])
        }
        
        booter4 <- function(){}
        
        if (model == "glm") {
                
                
                }
        } Else if (model ==)
lda_fit <- lda(default ~ balance + income, data = Default, family = "binomial")
lda_fit

}