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



## Combine Train and Test
combin <- rbind(train,test)

##Analyze univariate Gender Variable
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


