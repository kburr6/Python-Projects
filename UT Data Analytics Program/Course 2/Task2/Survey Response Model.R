###############
# Project Notes
###############

# Summarize project: This is a simple R pipeline to help understand how to organize
# a project in R Studio using the the caret package. This is a basic pipeline that
# does not include feature selection (filtering/wrapper methods/embedded methods).  

# Assignment "<-" short-cut: 
#   OSX [Alt]+[-] (next to "+" sign)
#   Win [Alt]+[-] 

# Comment multiple lines
# OSX: CTRL + SHIFT + C
# WIN: CMD + SHIFT + C


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
wd <- getwd()
# set working directory
setwd(wd)
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

#install.packages("caret")
#install.packages("corrplot")
#install.packages("readr")
#install.packages("C50")
#install.packages("inum")
library(caret)
library(corrplot)
library(corpcor)
library(doParallel)
library(readr)
library(C50)

#####################
# Parallel processing
#####################

#--- for WIN ---#
#install.packages("doParallel") # install in 'Load packages' section above
#library(doParallel)  # load in the 'Load Packages' section above
detectCores()  # detect number of cores
cl <- makeCluster(4)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
#Stop Cluster. After performing your tasks, make sure to stop your cluster. 
#stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ####

# --- Load Train/Existing data (Dataset 1) --- #
CompleteResponses <- read.csv("CompleteResponses.csv", stringsAsFactors = FALSE)
class(CompleteResponses)  # "data.frame"
str(CompleteResponses)

# --- Load Predict/New data (Dataset 2) --- #

IncompleteResponses <- read.csv("SurveyIncomplete.csv", stringsAsFactors = FALSE)
class(IncompleteResponses)  # "data.frame"
str(IncompleteResponses)


################
# Evaluate data
################

#--- Dataset 1 ---#

str(CompleteResponses)  
names(CompleteResponses)
summary(CompleteResponses)
head(CompleteResponses)
tail(CompleteResponses)

# plot
hist(CompleteResponses$age)
hist(CompleteResponses$elevel)
plot(CompleteResponses$elevel, CompleteResponses$car)
qqnorm(CompleteResponses$salary)
# check for missing values 
anyNA(CompleteResponses)
is.na(CompleteResponses)


#--- Dataset 2 ---#

# If there is a dataset with unseen data to make predictions on, then preprocess here
# to make sure that it is preprossed the same as the training dataset.

str(IncompleteResponses)  
names(IncompleteResponses)
summary(IncompleteResponses)
head(IncompleteResponses)
tail(IncompleteResponses)

# plot
hist(IncompleteResponses$age)
hist(IncompleteResponses$elevel)
plot(IncompleteResponses$elevel, IncompleteResponses$car)
qqnorm(IncompleteResponses$salary)
# check for missing values 
anyNA(IncompleteResponses)
is.na(IncompleteResponses)

#############
# Preprocess
#############

#--- Dataset 1 ---#

# change data types
#DatasetName$ColumnName <- as.typeofdata(DatasetName$ColumnName)

# rename a column
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 

# check for missin values (NAs)
#any(is.na(CompleteResponses)) 

# handle missing values 
#na.omit(DatasetName$ColumnName)
#na.exclude(DatasetName$ColumnName)        
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

# remove obvious features (e.g., ID, other)
#WholeYear7v <- WholeYear   # make a copy 
#WholeYear7v$X <- NULL   # remove ID
#str(WholeYear7v) # 35136 obs. of  7 variables

#normalize columns with large values that could impact regression model
CompleteResponsesScaled <- CompleteResponses
CompleteResponsesScaled[c(1, 6)] <- scale(CompleteResponsesScaled[c(1, 6)])
CompleteResponsesScaled
CompleteResponsesScaled$brand <- as.factor(CompleteResponsesScaled$brand)
# do feature engineering - correlation matrix to identify colinearity

M <- cor(CompleteResponsesScaled) #this won't work once change brand to factor
corrplot(M, method = "number") #all correlation coefficients are <.21 (largest is salary to brand)

cor2pcor(cov(CompleteResponsesScaled)) #no high collinearity identified

# save preprocessed dataset
write.csv(CompleteResponsesScaled,file = "CompleteResponsesPreprocessed.csv")


#--- Dataset 2 ---#

# Note: Be sure to alwasy procecess DS1 and DS2 (if available) in the same areas 
# of the pipeline

#normalize columns with large values that could impact regression model
IncompleteResponsesScaled <- IncompleteResponses
IncompleteResponsesScaled[c(1, 6)] <- scale(IncompleteResponsesScaled[c(1, 6)])
IncompleteResponsesScaled
IncompleteResponsesScaled$brand <- as.factor(IncompleteResponsesScaled$brand)
# do feature engineering - correlation matrix to identify colinearity

M <- cor(IncompleteResponsesScaled)
corrplot(M, method = "number") #all correlation coefficients are <.02 (largest is salary to brand)

cor2pcor(cov(IncompleteResponsesScaled)) #no high collinearity identified

# save preprocessed dataset
write.csv(IncompleteResponsesScaled,file = "IncompleteResponsesPreprocessed.csv")



################
# Sampling
################

# ---- Sampling ---- #

# Note: The set.seed function has to be executed immediately preceeding any 
# function that needs a seed value

set.seed(seed)
CompleteResponsesScaledv1k <- CompleteResponsesScaled[sample(1:nrow(CompleteResponsesScaled), 1000, replace=FALSE),]
head(CompleteResponsesScaledv1k) # ensure randomness
nrow(CompleteResponsesScaledv1k) # ensure number of obs
# create 10% sample for 7v ds
set.seed(seed) # set random seed
CompleteResponsesScaledv10p <- CompleteResponsesScaled[sample(1:nrow(CompleteResponsesScaled), round(nrow(CompleteResponses)*.1),replace=FALSE),]
nrow(CompleteResponsesScaledv10p)
head(CompleteResponsesScaledv10p) # ensure randomness


##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(CompleteResponsesScaled$brand, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- CompleteResponsesScaled[inTraining,]   
testSet <- CompleteResponsesScaled[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   


################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

?modelLookup()
modelLookup("C5.0")

# C5.0 train/fit

set.seed(seed)
system.time(C50Fit1 <- train(brand~., data=trainSet, method="C5.0", importance=T, trControl=fitControl)) #importance is needed for varImp
C50Fit1

varImp(C50Fit1)

# RF train/fit

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

set.seed(seed)
system.time(rfFit1 <- train(brand~., data=trainSet, method="rf", importance=T, trControl=fitControl, tuneGrid=rfGrid)) #importance is needed for varImp
rfFit1

varImp(rfFit1)


#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResultsCompleteScaled <- resamples(list(c50=C50Fit1, rf=rfFit1))
# output summary metrics for tuned models 
summary(ModelFitResultsCompleteScaled)
# ds (CompleteResponsesScaled) Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# CompleteResponsesScaled
# copy results from the Console and past here

# Added to word document 



##--- Conclusion ---##
# Make a note of which model is the top model, and why

# top model is random forest - rf with mtry = 3 is barely higher on  Kappa and Accuracy.



########################
# Validate top model
########################

# make predictions with test data set
rfPred1 <- predict(rfFit1, testSet)

write.csv(rfPred1, file = "predictions.csv")

# performace measurment
postResample(rfPred1, testSet$brand)
# RMSE      Rsquared  
# (make  note of performance metrics)
# for classification models use accuracy and kappa
#Accuracy     Kappa 
#0.9272433 0.8459727


# plot predicted verses actual
plot(rfPred1,testSet$brand)
# print predictions
rfPred1

str(rfPred1)
summary(rfPred1)

########################
# Predict with top model
########################

# make predictions
rfPred2 <- predict(rfFit1, IncompleteResponsesScaled)

write.csv(rfPred2, file = "IncompleteSurveyResponsesPredictions.csv")

postResample(rfPred2,IncompleteResponsesScaled$brand)

summary(rfPred2)

########################
# Save validated model
########################

##--- Save top performing model ---##

# save top performing model after it has been validated

# save model 
saveRDS(rfFit1, "Model.rds")  # Q: What type of object does saveRDS create?

# load and name model to make predictions with new data
RFfit1 <- readRDS("Model.rds") # Q: What type of object does readRDS create?





