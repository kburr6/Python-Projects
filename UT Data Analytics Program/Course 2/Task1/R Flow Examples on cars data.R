# data loading steps

library(readr)
cars <- read.csv("cars.csv")

# analysis and prep of data steps

attributes(cars)#List your attributes within your data set.

summary(cars) #Prints the min, max, mean, median, and quartiles of each attribute.

str(cars) #Displays the structure of your data set.

names(cars) #Names your attributes within your data set.

cars$name.of.car #Will print out the instances within that particular column in your data set.

hist(cars$speed.of.car)

plot(cars$speed.of.car,cars$distance.of.car)#scatter plot when both numeric

plot(cars$name.of.car,cars$distance.of.car)#bar plot when both numeric

qqnorm(cars$speed.of.car)# to show whether or not the data is normally distributed

# DatasetName$ColumnName<-as.typeofdata(DatasetName$ColumnName) -- retypes a column's data

# names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") --renames columns in order

summary(cars) #Will count how many NA’s you have.

is.na(cars) #Will show your NA’s through logical data. (TRUE if it’s missing, FALSE if it’s not.)

# na.omit(DatasetName$ColumnName#Drops any rows with missing values and omits them forever.
        
# na.exclude(DatasetName$ColumnName)#Drops any rows with missing values, but keeps track of where they were.

# replace na with mean DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)

# ----------------------------------------------------------------
  
# training and testing steps

set.seed(123) # common seed value is 123 in order to reproduce random results

# to calculate the size of the training and test data sets from the input data set

trainSize<-round(nrow(cars)*0.7) 

testSize<-nrow(cars)-trainSize

#to actually create the training and test data sets

training_indices<-sample(seq_len(nrow(cars)),size =trainSize)

trainSet<-cars[training_indices,]

testSet<-cars[-training_indices,] 

# to create the linear regression model to predict distance based on speed from the cars dataset

lm_PredictDistance<-lm(distance.of.car~ speed.of.car, trainSet)

summary(lm_PredictDistance)

DistancePredictions <- predict(lm_PredictDistance,testSet)


d = data.frame(testSet, predict(lm_PredictDistance, testSet, interval="prediction"))
d <- d[ -c(5:6) ]
d
write.csv(d, file = "carsPredictions.csv")
