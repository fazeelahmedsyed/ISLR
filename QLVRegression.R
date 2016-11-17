# This function is for the Qualitative predictors

#Preparing Data
library(MASS)
library(ISLR)
Carseats <- Carseats
a <- colnames(Carseats)
b <- c("Unit sales (in thousands) at each location", "Price charged by competitor at each location","Community income level (in thousands of dollars)","Local advertising budget for company at each location (in thousands of dollars)","Population size in region (in thousands)","Price company charges for car seats at each site","A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site","Average age of the local population","Education level at each location","A factor with levels No and Yes to indicate whether the store is in an urban or rural location","A factor with levels No and Yes to indicate whether the store is in the US or not")
Carseats_desc <- cbind(a,b)
colnames(Carseats_desc) <- c("Variable", "Variable Description")
rm(a)
rm(b)

#Fitting a model
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)

#Checking Dummy Variables created
contrasts(ShelveLoc)
contrasts(Urban)
contrasts(US)
