#This script is for splines 

library(ISLR)
Wage <- Wage

#SIMPLE SPLINE

##Basis matrix for regression. Poly = 3
bs(age, knots = c(25,40,60), degree = 3) #Creates age of the form age, age^2, age^3, (x-25)^3, (x-40)^3 and (x-60)^3.

##Fitting a cubic spline 
spl3 <- lm(wage ~ bs(age, knots = c(25,40,60), degree = 3), data = Wage)

##Plotting for entire range of age
agelims <- range(age)
agerange <- seq(agelims[1],agelims[2])

spl3_preds <- predict(spl3, newdata = list(age = agerange), se.fit = T)
spl3_sebands <- cbind(spl3_preds$fit + 2*spl3_preds$se.fit,spl3_preds$fit - 2*spl3_preds$se.fit)

plot(age,wage,col = 'dark gray')
lines(agerange, spl3_preds$fit, lwd = 2, col = 'blue')
matlines(agerange, spl3_sebands, lwd = 1, lty = 'dashed', col = 'blue')

#NATURAL SPLINE

##Basis matrix of natural spline
ns(age, df = 4) #Has only one knot. Is a cubic spline. Does not have the option of other degrees.

##Fitting model
nspl3 <- lm(wage ~ ns(age, df=4), data = Wage)

##plotting for the entire range of age

nspl3_preds <- predict(nspl3, newdata = list(age = agerange), se.fit = T)

lines(agerange, nspl3_preds$fit, lwd = 2, col = 'red')
lines(agerange, nspl3_preds$fit +2*nspl3_preds$se.fit, lwd = 1, col = 'red', lty ='dashed')
lines(agerange, nspl3_preds$fit -2*nspl3_preds$se.fit, lwd = 1, col = 'red', lty ='dashed')

#SMOOTHING SPLINES

##Fitting a smoothing spline by providing df and calculating lambda
sspl <- smooth.spline(age,wage, df =16) #has no function for degree again
sspl$lambda

##Fitting a smoothing spline by finding best lambda using CV
sspl2 <- smooth.spline(age,wage, cv = T)
sspl2$df
sspl2$lambda

##Plotting and comparing both smoothing splines

plot(age,wage,xlim = agelims, cex = 0.5, col = "darkgrey")

lines(sspl, col = 'red', lwd = 2)
lines(sspl2, col = 'blue', lwd =2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c('red','blue'), lty = 1, lwd = 2, cex = 0.8)

#LOCAL REGRESSION

##Fitting model

lr <- loess(wage ~ age, span = 0.2, data = Wage)
lr2 <- loess(wage ~ age, span = 0.5, data = Wage)

##Making predictions for range of age
lr_preds <- predict(lr,newdata = data.frame(age =agerange))
lr2_preds <- predict(lr2, newdata = data.frame(age =agerange))

##PLotting results
plot(age, wage, cex = 0.5, col = 'darkgrey')
title("LOCAL REGRESSION")
lines(agerange, lr_preds, col = 'red', lwd = 2 )
lines(agerange, lr2_preds, col = 'blue', lwd = 2)
legend("topright", legend = c("Span 0.2", "Span 0.5"), col =c('red','blue'), lwd = 2, cex = 0.8, lty = 1)
