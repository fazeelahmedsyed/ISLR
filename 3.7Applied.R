#This is for ISLR Exercise 3.7 Applied

library(ISLR)
Auto <- Auto
attach(Auto)

#8(a)
lm.fit <- lm(mpg~ horsepower)
summary(lm.fit)
#i,ii,iii
print("On average, by increasing engine horsepower by 1 engine efficiency increases by a reduction of  1/0.15= 6 miles per gallon. We are very confident on this relationship with statistic significance of 99.99%. The relationship between mpg and horsepower is inverse")
#iv
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")

#8(b)
plot(horsepower, mpg, col = "dark grey", pch = 4)
abline(lm.fit, col = "red", lwd = 3)
print("The values appear to show a non-linear trend")

#8(c)
par(mfrow = c(2,2))
plot(lm.fit)

#9(a)
pairs(Auto[,-9])

#9(b)
a <- cor(Auto[,-9])

#9(c)
lm.fit2 <- lm(mpg ~ .-name, data = Auto)
summary(lm.fit2)
print("There appears to be strong relationships between mpg and the following predictors: displacement, weight, year and origin, with significance of more than 99%. Here, the coefficient for the predictor year suggests that on average efficiency of vehicles has increased by 0.75 miles per gallon each year.")

#9(d)
plot(lm.fit2)
print("There is an obvious trend in the residual plot. The fit appears to be non linear. The residual plot also indicates outliers at observations 323, 327 and 326. The leverage plot shows unsually high leverage points at obv 14, 327 and 394.")

#9(e)
#experimental: apply(a, MARGIN = 1:2, FUN = function(x) all(x > 0.9 | x < -0.9))
#choosing highest values from correlation matrix
lm.fit3 <- lm(mpg ~ cylinders*displacement + displacement*weight, data = Auto)
summary(lm.fit3)

#9(f)

