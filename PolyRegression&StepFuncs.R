#This script is for the practice of polynomical and Step functions

library(ISLR)
Wage <- Wage

#REGRESSION POLYNOMIAL

lm_fit <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
summary(lm_fit)

##Alternate method. This is equalent to providing a matrix to the formula for regression
lm2_fit <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
summary(lm2_fit)

##calculating predictions
agelims <- range(age)
agerange <- seq(agelims[1],agelims[2])
lm_preds <- predict(lm_fit, newdata = list(age = agerange),se.fit = T)

##Creating graph
par(mfrow = c(1,2), mar = c(4,4,1,1), oma = c(0,0,4,0)) #mar is used to set white space within each plot. oma is used to set whitespace in the entire plot result.
lm_SEbands <- cbind(lm_preds$fit + 2*lm_preds$se.fit, lm_preds$fit - 2*lm_preds$se.fit) #we includes bands as well

plot(age, wage, xlim = agelims, cex = 0.5, col = 'dark grey')

title("Degree - 4 Polynomial", outer = T)
lines(agerange, lm_preds$fit, lwd = 2, col = 'blue')
matlines(agerange, lm_SEbands, lwd = 1, col = 'blue', lty = 3)

##Testing Order of Polynomial
lm1 <- lm(wage ~ age, data = Wage)
lm2 <- lm(wage ~ poly(age,2), data = Wage)
lm3 <- lm(wage ~ poly(age,3), data = Wage)
lm4 <- lm(wage ~ poly(age,4), data = Wage)
lm5 <- lm(wage ~ poly(age,5), data = Wage)

anova(lm1,lm2,lm3,lm4,lm5) #Poly 3 appears best. Instead of using ANOVA we could have also used Cross Validation to select the best polynomial.

#CLASSIFICATION POLYNOMIAL

##Creating categorical variable
wage2 <- rep(0, length(Wage$wage))
wage2[Wage$wage >250] = 1

##Fitting Model
glm_fit <- glm(wage2~ poly(age,4), data = Wage, family = 'binomial')

##Creating graph for all age

###agelims <- range(Wage$age)
###agerange <- seq(agelims[1],agelims[2])

glm_odds <- predict(glm_fit, newdata = list(age = agerange), se.fit = T)

glm_pfits <- exp(glm_odds$fit)/(1+exp(glm_odds$fit))

glm_sebands <- cbind(glm_odds$fit + 2*glm_odds$se.fit, glm_odds$fit - 2*glm_odds$se.fit) 
###These bands are currently for the logit. To convert them to bands for P(Y =1|X), we usxe the formula P(Y=1|X) = exp(linear Model)/1+exp(linear model).
###Since in log Regression. Logit = Linear Model. We can safely conculde that P(Y=1|X) = exp(Logit)/1+exp(logit)
###Note that we do not contruct bands for thr probs because it would create negative probs. which would become menaingless.
glm_sebands <- exp(glm_sebands)/(1+exp(glm_sebands))

plot(age, wage2, xlim = agelims, type = "n", ylim = c(0, 0.2))
points(age,wage2/5, cex = 0.5, pch = "l", col = 'dark grey')
lines(agerange, glm_pfits, lwd = 2, col = 'blue')
matlines(agerange, glm_sebands, lwd = 1, col = 'blue', lty = 3)

#STEP FUNCTIONS

##Making 4 cuts within age
table(cut(age,4))

##Fitting model
lm_step <- lm(wage ~ cut(age,4), data = Wage) #This is equivalent to using categorical variables that equal to 1 if X lies in the bracket. Thus, the regression uses dummy variables.
summary(lm_step)
