##Black Friday Problem using H20
##Setting Environment for H20
require(data.table)

##Read Testing and Training data
train <- fread("train.csv",stringsAsFactors = T)
test <- fread("test-comb.csv",stringsAsFactors = T)

##Dimensions of train and test data
dim(train)
dim(test)

str(train)

##With just mean of the entire population
sub_mean <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = mean(train$Purchase))
write.csv(sub_mean,"first_sub.csv",row.names = F)


##Second Approach

##Summary of Data
summary(train)
summary(test)


##Add Purchase column in test data

test$Purchase <- mean(train$Purchase)
head(test)
str(test)
require(dplyr)
test <- select(test,-Comb)


## Combine Train and Test
combin <- rbind(train,test)

##Analyze univariate 
##Gender Variable
combin[,prop.table(table(Gender))]

##Age
combin[,prop.table(table(Age))]

##City Category Variable
combin[,prop.table(table(City_Category))]

##Stay in City in years
combin[,prop.table(table(Stay_In_Current_City_Years))]

##Unique values of User Id and Product Id
NROW(unique(combin$User_ID))
NROW(unique(combin$Product_ID))

colSums(is.na(combin))

##We need to encode Gender variable into 0 and 1 (good practice).
##We’ll also need to re-code the Age bins.
##Since there are three levels in City_Category, we can do one-hot encoding.
##The “4+” level of Stay_in_Current_Years needs to be revalued.
##The data set does not contain all unique IDs. This gives us enough hint for feature engineering.
##Only 2 variables have missing values. In fact, a lot of missing values, which could be capturing a hidden trend. We’ll need to treat them differently.

##Analyze bivariate Variables
require(ggplot2)

##Age vs Gender
ggplot(combin,aes(Age)) + geom_bar(aes(fill=Gender))

##Age vs City Category
ggplot(combin,aes(Age)) + geom_bar(aes(fill=combin$City_Category))

##We can also create cross tables for analyzing categorical variables
##We use gmodels package for this

require(gmodels)
CrossTable(combin$Occupation,combin$City_Category)

##Data Manipulation using Data.table
##There are lots of NA's which feels like we will miss some important trend
##Creating new variable for NA's in product category 2 and 3
combin$Product_Category_2_NA <- ifelse(is.na(combin$Product_Category_2),1,0)
combin$Product_Category_3_NA <- ifelse(is.na(combin$Product_Category_3),1,0)

##Moving NA's to arbitrary value -999 for product category 2 and 3
combin[is.na(combin$Product_Category_2),]$Product_Category_2 <- -999
combin[is.na(combin$Product_Category_3),]$Product_Category_3 <- -999

##Change 4+ to 4
combin[combin$Stay_In_Current_City_Years == "4+",]$Stay_In_Current_City_Years <- "4"

##Regrouping Age Groups

levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6
combin$Age <- as.numeric(combin$Age)

#convert Gender into numeric
combin[, Gender := as.numeric(as.factor(Gender)) - 1]

##Feature Engineering
##Number of times user has made the purchase
combin[, User_Count := .N, by = User_ID]
##Number of times product has been purchased
combin[, Product_Count := .N, by = Product_ID]

#Mean Purchase of Product
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]

#Mean purchase made by customer
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]

##one hot encoding of City_Category variable
require(dummies)
combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
sapply(combin, class)

#converting Product Category 2 & 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)

##Model Building using H2O

##Divide test and train

c.train <- combin[1:NROW(train),]
c.test <- combin[-(1:NROW(train)),]
c.train <- c.train[c.train$Product_Category_1 <=18,]

require(h2o)
localH2O <- h2o.init(nthreads = -1)

#data to h2o cluster
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)

#dependent variable (Purchase)
y.dep <- 14
#independent variables (dropping ID variables)
x.indep <- c(3:13,15:20)

#Multiple Regression in H2O
regression.model <- h2o.glm(y = y.dep, x = x.indep, training_frame = train.h2o, family = "gaussian")
h2o.performance(regression.model)

##Prediction
predict.reg <- as.data.frame(h2o.predict(regression.model, test.h2o))
sub_reg <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase =  predict.reg$predict)
write.csv(sub_reg, file = "sub_reg.csv", row.names = F)


##Random Forest in H2O
system.time(rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122))

h2o.performance(rforest.model)

#check variable importance
h2o.varimp(rforest.model)
#making predictions on unseen data
system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
sub_rf <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase =  predict.rforest$predict)
write.csv(sub_rf, file = "sub_rf.csv", row.names = F)


##GBM in H2O
system.time(
    gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
)

#making prediction and writing submission file
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
sub_gbm <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = predict.gbm$predict)
write.csv(sub_gbm, file = "sub_gbm.csv", row.names = F)

##Deep Learning in H2O
#deep learning models
system.time(
    dlearning.model <- h2o.deeplearning(y = y.dep,
                                        x = x.indep,
                                        training_frame = train.h2o,
                                        epoch = 60,
                                        hidden = c(100,100),
                                        activation = "Rectifier",
                                        seed = 1122
    )
)

h2o.performance(dlearning.model)
#making predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))

#create a data frame and writing submission file
sub_dlearning <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, Purchase = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new.csv", row.names = F)
