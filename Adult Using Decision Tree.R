## Using Decision Tree for Adult Problem
head(adult)
head(adult.test)
head(adult.final.test)

## Loading the rpart package
require(rpart)
str(adult)

##Modelling using Decision Tree and Visualization
tree <- rpart(income ~ .,method = "class", data = adult)
printcp(tree)
plot(tree,uniform = TRUE,main = "Adult")
text(tree,use.n = TRUE, all = TRUE)
require(rpart.plot)
prp(tree)

##Predicting the outcome of Test set
predict.decTree <- predict(tree,adult.final.test,type = 'class')
head(predict.decTree)


adult.test$predict.DicTree <- predict.decTree
head(adult.test)

##Confusion Matrix
table(adult.test$income,adult.test$predict.DicTree)

##Missclassifcation error
missclassifcation = (630+1901)/16281
missclassifcation

##End Result - 0.1554573 (Lesser than Logistic Regression)


actual.income <- adult.test$income
cost <- 0
for (cost in seq(0.01,0.1,0.005)){
    ap <- prune(tree,cp=cost)
    new.predict <- predict(ap,adult.final.test,type = 'class')
    print("Confusion Matrix for Cp, '%d'",cost)
    print(table(actual.income,new.predict))
}
