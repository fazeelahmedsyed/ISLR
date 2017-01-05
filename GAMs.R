#This script is for Generalized Additive Models

library(gam)
library(ISLR)
Wage <- Wage

#For GAMs used on functions that have basis forms, we can simply use lm
#For example Natural Splines

GAM1 <- lm(wage ~ ns(year,df = 4) + ns(age, df = 5) + education, data = Wage)
summary(GAM1)

#For functions with no basis form, We will use the GAM library
#For example smoothing splines

GAM2 <- gam(wage ~ s(year, df=4) + s(age, df = 5) + education, data = Wage)

#Creating graoh of GAM2
par(mfrow = c(3,1))
plot(GAM2, se = T, col = 'blue')
plot.gam(GAM1, se = T, col = 'red')

#year appears to be linear in shape so we do further analysis

GAM1 <- gam(wage ~ s(age, df=5) +education, data = Wage) #excludes year
GAM3 <- gam(wage ~ year + s(age, df=5) + education, data = Wage) #Assumes linear form of year

anova(GAM1, GAM3, GAM2, test = "F") #year in model is better than no year. But we are indifferent about having year as linear or as a smoothing spline.

#Observing statistics for GAM
summary(GAM2)

#We can also use GAM for local Regression
GAM4 <- gam(wage ~ s(year, df=4) + lo(age, span = 0.7) + education, data = Wage)

##Using lo() to include interaction terms

GAM5 <- gam(wage ~ lo(year,age, span = 0.5) + education, data = Wage)
summary(GAM5)

##plotting the interaction plane
library(akima)
plot(GAM5)

#Fitting a GAM as a logistic regression
GAM_LogR <- gam(I(wage>250) ~ year + s(age,df=5) + education, family = 'binomial', data = Wage)
par(mfrow = c(3,1))
plot(GAM_LogR, se = T, col = 'green')

table(education, I(wage>250))

GAM2_LogR <- gam(I(wage>250) ~ year + s(age,df=5) + education, family = 'binomial', data = Wage, subset = (education != "1. < HS Grad"))
plot(GAM2_LogR, se = T, col = 'Green')
