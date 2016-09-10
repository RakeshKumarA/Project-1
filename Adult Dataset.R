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

##Modeling
