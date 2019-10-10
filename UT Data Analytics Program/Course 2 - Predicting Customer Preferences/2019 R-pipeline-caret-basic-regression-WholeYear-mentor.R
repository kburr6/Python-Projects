# Title: R_caret_pipeline_basic_regression_WholeYear

# Last update: 2019.09

# File/project name: 2019 R-pipeline-caret-basic-regression-WholeYear.R
# RStudio Project name: See resources for details on R projects

###############
# Project Notes
###############

# Summarize project: This is a simple R pipeline to help understand how to organize
# a project in R Studio using the the caret package. This is a basic pipeline that
# does not include feature selection (filtering/wrapper methods/embedded methods).  

# Summarize top model and/or filtered dataset

# Objects
x <- 5 + 3 + 6  
y <- 10
x + y

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
getwd()
# set working directory
setwd("the directory to your folder for this project")
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
library(caret)
library(corrplot)
library(doMC)
library(doParallel)
library(mlbench)
library(readr)


#####################
# Parallel processing
#####################

# NOTE: Be sure to use the correct package for your operating system.

#--- for OSX ---#
install.packages("doMC")  # install in 'Load packages' section above 
library(doMC)
detectCores()   # detect number of cores
registerDoMC(cores = 2)  # set number of cores; 2 in this example (don't use all available)

#--- for WIN ---#
install.packages("doParallel") # install in 'Load packages' section above
library(doParallel)  # load in the 'Load Packages' section above
detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ####

# --- Load Train/Existing data (Dataset 1) --- #
WholeYear <- read.csv("WholeYear.csv", stringsAsFactors = FALSE)
class(WholeYear)  # "data.frame"
str(WholeYear)



# --- Load Predict/New data (Dataset 2) --- #

# There is no additional dataset for this project



#### --- Load preprocessed datasets --- ####

ds_name <- read.csv("dataset_name.csv", stringsAsFactors = FALSE) 


################
# Evaluate data
################

#--- Dataset 1 ---#
str(WholeYear)  # 35136 obs. of  8 variables 
names(WholeYear)
summary(WholeYear)
head(WholeYear)
tail(WholeYear)

# plot
hist(WholeYear$SolarRad)
plot(WholeYear$TimeofDay, WholeYear$SolarRad)
qqnorm(WholeYear$SolarRad)
# check for missing values 
anyNA(WholeYear)
is.na(WholeYear)


#--- Dataset 2 ---#

# If there is a dataset with unseen data to make predictions on, then preprocess here
# to make sure that it is preprossed the same as the training dataset.


#############
# Preprocess
#############

#--- Dataset 1 ---#

# change data types
DatasetName$ColumnName <- as.typeofdata(DatasetName$ColumnName)

# rename a column
names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 

# check for missin values (NAs)
any(is.na(WholeYear)) 

# handle missing values 
na.omit(DatasetName$ColumnName)
na.exclude(DatasetName$ColumnName)        
DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

# remove obvious features (e.g., ID, other)
WholeYear7v <- WholeYear   # make a copy 
WholeYear7v$X <- NULL   # remove ID
str(WholeYear7v) # 35136 obs. of  7 variables

# save preprocessed dataset
write.csv()


#--- Dataset 2 ---#

# Note: Be sure to alwasy procecess DS1 and DS2 (if available) in the same areas 
# of the pipeline


################
# Sampling
################

# ---- Sampling ---- #

# Note: The set.seed function has to be executed immediately preceeding any 
# function that needs a seed value

# Note: For this task, use the 1000 sample, and not the 10%

# 1k sample
set.seed(seed)
WholeYear7v1k <- WholeYear7v[sample(1:nrow(WholeYear7v), 1000, replace=FALSE),]
head(WholeYear7v1k) # ensure randomness
nrow(WholeYear7v1k) # ensure number of obs
# create 10% sample for 7v ds
set.seed(seed) # set random seed
WholeYear7v10p <- WholeYear7v[sample(1:nrow(WholeYear7v), round(nrow(WholeYear)*.1),replace=FALSE),]
nrow(WholeYear7v10p)
head(WholeYear10p7v) # ensure randomness


##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(WholeYear7v1k$SolarRad, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- WholeYear7v1k[inTraining,]   
testSet <- WholeYear7v1k[-inTraining,]   
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
modelLookup("rf")


## ------- LM ------- ##

# LM train/fit
set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
lmFit1 <- train(SolarRad~., data=trainSet, method="lm", trControl=fitControl)
lmFit1  
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
varImp(lmFit1)





## ------- RF ------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(SolarRad~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
rfFit1

varImp(rfFit1)


## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(SolarRad~., data=trainSet, method="svmLinear", trControl=fitControl)
svmFit1


#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults1k <- resamples(list(lm=lmFit1, rf=rfFit1, svm=svmFit1))
# output summary metrics for tuned models 
summary(ModelFitResults1k)
# ds (WholeYear7v1k) Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# WholeYear7v1k
# copy results from the Console and past here





##--- Conclusion ---##
# Make a note of which model is the top model, and why





########################
# Validate top model
########################

# make predictions
rfPred1 <- predict(rfFit1, testSet)

# performace measurment
postResample(rfPred1, testSet$SolarRad)
# RMSE      Rsquared  
# (make  note of performance metrics)

# plot predicted verses actual
plot(rfPred1,testSet$SolarRad)
# print predictions
rfPred1


########################
# Predict with top model
########################

# make predictions
rfPred1 <- predict(rfFit1, new_dataset)




########################
# Save validated model
########################

##--- Save top performing model ---##

# save top performing model after it has been validated

# save model 
saveRDS()  # Q: What type of object does saveRDS create?

# load and name model to make predictions with new data
RFfit1 <- readRDS() # Q: What type of object does readRDS create?





