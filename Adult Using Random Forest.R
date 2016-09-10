#Setting Environment for Random Forest
require(randomForest)
str(adult)
str(adult.final.test)
#Modeling Random Forest
adult.rfmodel <- randomForest(income ~ adult$age
                              +adult$workclass
                              +adult$fnlwgt
                              +adult$education
                              +adult$`education-num`
                              +adult$`marital-status`
                              +adult$occupation
                              +adult$relationship
                              +adult$race
                              +adult$sex
                              +adult$`capital-gain`
                              +adult$`capital-loss`
                              +adult$`hours-per-week`
                              +adult$`native-country`,data = adult)


names(adult$`education-num`) <- c("educationnum")
names(adult$`marital-status`) <- c("maritalstatus")
names(adult$`capital-gain`) <- c("capitalgain")
names(adult$`capital-loss`) <- c("capitalloss")
names(adult$`hours-per-week`) <- c("hoursperweek")
names(adult$`native-country`) <- c("nativecountry")
names(adult) <- c("age","workclass","fnlwgt","education","educationnum","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","nativecountry","income")

adult.rfmodel <- randomForest(income ~ .,data = adult)
str(adult.final.test)
names(adult.final.test) <- c("age","workclass","fnlwgt","education","educationnum","maritalstatus","occupation","relationship","race","sex","capitalgain","capitalloss","hoursperweek","nativecountry")
str(adult)

print(adult.rfmodel)

##Predicting Test data
predict.RForest <- predict(adult.rfmodel,adult.final.test)
tail(predict.RForest)
predict.RForest <- as.data.frame(predict.RForest)

predict.RForest <- predict.RForest[1:16281,]
adult.test$predict.RForest <- predict.RForest

##Confusion Matrix
table(adult.test$income,adult.test$predict.RForest)

##Misclassification
missclassifcation <- (2819+29)/16281
missclassifcation

##Ending - Misclassification error rate - 0.1749278
