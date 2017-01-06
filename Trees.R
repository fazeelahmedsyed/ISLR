#This script is for Decision Trees

#Classification Trees

library(tree)
library(ISLR)
Carseats <- Carseats
High <- ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

##Fitting Classification Tree
Tree_Carseats <- tree(High ~ . - Sales, data = Carseats)
summary(Tree_Carseats)

##Observing Plot
plot(Tree_Carseats)
text(Tree_Carseats, pretty = 0, cex = 0.5)
Tree_Carseats

##Fitting model again using Validation set approach
set.seed(2)
train <- sample(1:nrow(Carseats), size = 200)

Carseats_test <- Carseats[-train,]
High_test <- High[-train]

Tree_Carseats <- tree(High ~ . - Sales, data = Carseats, subset = train)
Tree_preds <- predict(Tree_Carseats, newdata = Carseats_test, type = 'class')

conf <- addmargins(table(Tree_preds, High_test))
conf

error <- (conf[2,1]+conf[1,2])/conf[3,3] 
error

##Creating a pruned Tree

set.seed(3)
CV_Carseats <- cv.tree(Tree_Carseats, FUN = prune.misclass) #The function basically performs CV for the optimum complexity of the tree. It also uses k which is equal to alpha, the pruning parameter. dev here is the CV error rate.
CV_Carseats

###Creating plot to better understand results
par(mfrow = c(1,2))
plot(CV_Carseats$size, CV_Carseats$dev, type = 'b')
plot(CV_Carseats$k, CV_Carseats$dev, type = 'b')

###Pruned Tree
Pruned_Carseats <- prune.misclass(Tree_Carseats, best = 9)
plot(Pruned_Carseats)
text(Pruned_Carseats, pretty = 0, cex = 0.5)

Tree2_preds <- predict(Pruned_Carseats, Carseats_test, type = 'class')

conf2 <- addmargins(table(Tree2_preds, High_test))
conf2

error2 <- (conf2[2,1]+conf2[1,2])/conf2[3,3]
error2
