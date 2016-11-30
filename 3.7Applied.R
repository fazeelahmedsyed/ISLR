#This is for ISLR 3.7 Applied

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

rm(a)
rm(Auto)
#10(a)
Carseats <- Carseats
lm.fit4 <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm.fit4)

#10(b)
print("Since significance for the statistic for Urban is negligent, we can conclude that whether a store is in a rural or urban neighborhood has no impact on the amount of sales expected from that store. However, there is a significant impact of the location of the store w.r.t the country. A US store is expected to make 1200 unit sales more than a store outside US on average. Price has a significant impact as well. A price increase of one unit will decrease sales by 55 units on average.")

#10(d)
print("We can reject the null hypothesis for the predictors Price and US")

#10(e)
lm.fit5 <- lm(Sales ~ Price + US, data = Carseats)
summary(lm.fit5)

#10(f)
print("Since the multiple R Squared figure and RSE for both the models only marginally improve, we cannot conclude if lm.fit5 fits the data better than lm.fit4. However lm.fit5 does not have any unsignificant variable included.")

#10(g)
confint(lm.fit5, level = 0.95)

#10(h)
#experimental: labels on the graph
sort(hatvalues(lm.fit5), decreasing = T)[1:5]
plot(hatvalues(lm.fit5), pch = 4, col = "dark grey")
print("Observation no. 43 has unusually high leverage")

#11(a)
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
lm.fit6 <- lm(y ~ x + 0)
summary(lm.fit6)
print("The co-efficient estimate for x is 1.99 which is close to the actual model value of 2. Standard error of the estimate is 0.11, which means for every 100 samples we draw, 99 samples will contain a coefficient between 1.66 and 2.32")
