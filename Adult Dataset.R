## Read Adult data set from UCI Machine Learning Data Set

adult <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
head(adult)


## Names for the Data frame
names(adult) <- c("age","workclass","fnlwgt"
                  ,"education","education-num"
                  ,"marital-status","occupation"
                  ,"relationship","race","sex","capital-gain"
                  ,"capital-loss","hours-per-week"
                  ,"native-country","income")

str(adult)

##Change the response variable to binary and data clean up
adult$income <- ifelse(adult$income == " <=50K",0,1)
head(adult)
adult$income <- as.factor(adult$income)
adult$'education-num' <- as.factor(adult$`education-num`)
adult$workclass <- as.factor(adult$workclass)
adult$workclass
adult[adult$workclass == " ?",]$workclass <- " Unknown"
adult[is.na(adult$`native-country`),]$`native-country` <- " United-States"
adult[is.na(adult$occupation),]$occupation <- " Unknown"




#Check for any NA's
any(is.na(adult))

##Visualizations
str(adult)
require(ggplot2)
ggplot(adult,aes(x=adult$age)) + geom_histogram(aes(fill=adult$workclass))
ggplot(adult,aes(x=adult$workclass)) + geom_bar()
ggplot(adult,aes(x=adult$`native-country`)) + geom_bar(aes(fill=income))
ggplot(adult,aes(x=adult$race)) + geom_bar(aes(fill=income))
ggplot(adult,aes(x=adult$sex)) + geom_bar(aes(fill=income))
ggplot(adult,aes(x=adult$relationship)) + geom_bar(aes(fill=income))
ggplot(adult,aes(x=adult$occupation)) + geom_bar(aes(fill=factor(adult$`education-num`)))
levels(adult$`native-country`)
levels(adult$occupation)
head(adult)
str(adult)

##Modeling - Logistic Regression
model <- glm(income ~ ., data = adult, family = binomial)
summary(model)

##Test Data - Reading
adult.test <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",header = F,skip=1)
head(adult.test)
any(is.na(adult.test))
str(adult.test)

## Names for test data
names(adult.test) <- c("age","workclass","fnlwgt"
                  ,"education","education-num"
                  ,"marital-status","occupation"
                  ,"relationship","race","sex","capital-gain"
                  ,"capital-loss","hours-per-week"
                  ,"native-country","income")

#Data Cleaning
adult.test$workclass <- as.character(adult.test$workclass)
adult.test[is.na(adult.test$workclass),]$workclass <- " Unknown"
adult.test$workclass <- as.factor(adult.test$workclass)
adult.test$`education-num` <- as.factor(adult.test$`education-num`)
adult.test$occupation <- as.character(adult.test$occupation)
adult.test[adult.test$occupation == " ?",]$occupation <- " Unknown"
adult.test$occupation <- as.factor(adult.test$occupation)
ggplot(adult.test,aes(x=adult.test$`native-country`)) + geom_bar()
adult.test$`native-country` <- as.factor(adult.test$`native-country`)
adult.test[is.na(adult.test$`native-country`),]$`native-country` <- " United-States"

str(adult)
str(adult.test)
require(dplyr)
adult.final.test <- select(adult.test,-income)
head(adult.final.test)
adult.test[adult.test$income == " <=50K.",]
adult.test$income <- ifelse(adult.test$income == " <=50K.",0,1)
adult.test.new$income
adult.test$income <- as.factor(adult.test$income)


##Predicting
predict <- predict(model,adult.final.test,type = 'response')
NROW(predict)
predict
predict <- ifelse(predict < 0.5,0,1)


adult.test$predictedincome <- predict
str(adult.test)
adult.test$income <- adult.test.new$income
str(adult.test)
## Confusion Matrix
table(adult.test$income,adult.test$predictedincome)
##Missclassification rate
missclassifcation = (854+1548)/16281
missclassifcation
