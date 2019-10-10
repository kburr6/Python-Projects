# Title: C2T3 Pipeline

# Last update: 2020.09

# File/project name: C2T3 Pipeline.R
# RStudio Project name: Task3R.proj

###############
# Project Notes
###############

# #The sales team has again consulted with me with some concerns about 
# ongoing product sales in one of our stores. Specifically, they have 
# been tracking the sales performance of specific product types and would 
# like us to redo our previous sales prediction analysis, but this time 
# they'd like us to include the ‘product type’ attribute in our 
# predictions to better understand how specific product types perform 
# against each other. They have asked our team to analyze historical 
# sales data and then make sales volume predictions for a list of new 
# product types, some of which are also from a previous task. This will 
# help the sales team better understand how types of products might impact 
# sales across the enterprise.

# I have attached historical sales data and new product data sets to 
#this email. I would like for you to do the analysis with the goals of:
# 
# Predicting sales of four different product types: PC, Laptops, 
#Netbooks and Smartphones
# Assessing the impact services reviews and customer reviews have on 
#sales of different product types
# When you have completed your analysis, please submit a brief report 
# that includes the methods you employed and your results. I would also 
# like to see the results exported from R for each of the methods.

# Summarize top model and/or filtered dataset

# Comment multiple lines

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

if(!require(caret)){
  install.packages("caret")
  library(caret)
}

if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

if(!require(readr)){
  install.packages("readr")
  library(readr)
}


#####################
# Parallel processing
#####################

#--- for WIN ---#

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParellel)
}
detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
#stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ############################

# --- Load Train/Existing data (Dataset 1) --- ############
existingProduct <- read.csv("existingproductattributes2017.csv", stringsAsFactors = FALSE)
class(existingProduct)  # "data.frame"
str(existingProduct)


# --- Load Predict/New data (Dataset 2) --- ###############

newProduct <- read.csv("newproductattributes2017.csv", stringsAsFactors = FALSE)
class(newProduct)  # "data.frame"
str(newProduct)



#### --- Load preprocessed datasets --- ##################
#no preprocessed data sets for this project
#ds_name <- read.csv("dataset_name.csv", stringsAsFactors = FALSE) 


################
# Evaluate data
################

#--- Dataset 1 ---##########################################

str(existingProduct)  # 80 obs. of  18 variables 
names(existingProduct)
summary(existingProduct)
head(existingProduct)
tail(existingProduct)

# plot
hist(existingProduct$x5StarReviews)
plot(existingProduct$ProductNum, existingProduct$Volume)
qqnorm(existingProduct$Price)

# check for missing values 
anyNA(existingProduct)  #TRUE
is.na(existingProduct)  # BestSellersRank has NA's


#--- Dataset 2 ---##########################################

str(newProduct)  # 24 obs. of  18 variables 
names(newProduct)
summary(newProduct)
head(newProduct)
tail(newProduct)

# plot
hist(newProduct$x5StarReviews)
plot(newProduct$ProductNum, newProduct$Volume)
qqnorm(newProduct$Price)

# check for missing values 
anyNA(newProduct) #FALSE
is.na(newProduct)


#############
# Preprocess
#############

#--- Dataset 1 ---#

#dumify the data - convert categorical predictors to (0,1)
newDataFrame1 <- dummyVars(" ~ .", data = existingProduct)
readyData_existingProduct <- data.frame(predict(newDataFrame1, newdata = existingProduct))
#View(readyData_existingProduct)

str(readyData_existingProduct)

# change data types
#DatasetName$ColumnName <- as.typeofdata(DatasetName$ColumnName)

# rename a column
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 

# check for missing values (NAs)
summary(readyData_existingProduct) #best sellers rank has 15 NA's
any(is.na(readyData_existingProduct)) #TRUE

# handle missing values 
readyData_existingProduct$BestSellersRank <- NULL #delete attribute BestSellersRank due to NA's
#na.omit(DatasetName$ColumnName)
#na.exclude(DatasetName$ColumnName)        
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

#Review correlation matrix  -  USE THIS TO ANSWER CORRELATION BETWEEN SERVICES AND PRODUCT REVIEWS ON SALES VOLULME
corrData <- cor(readyData_existingProduct)
corrplot(corrData)
corrData
volCorrData <- corrData[,"Volume", drop=FALSE]
write.csv(volCorrData, file = "volCorrData.csv")
#Looking for predictors highly correlated to the dependent variable volume
#where correlation greater than .95
# Volume
# ProductTypeAccessories       0.127803771
# ProductTypeDisplay          -0.037583856
# ProductTypeExtendedWarranty  0.070865276
# ProductTypeGameConsole       0.388298241
# ProductTypeLaptop           -0.069799582
# ProductTypeNetbook          -0.070010545
# ProductTypePC               -0.102891676
# ProductTypePrinter          -0.149200679
# ProductTypePrinterSupplies  -0.090403335
# ProductTypeSmartphone       -0.038508275
# ProductTypeSoftware          0.001196472
# ProductTypeTablet           -0.050941908
# ProductNum                   0.166120763
# Price                       -0.142343990
# x5StarReviews                1.000000000  #### highly correlated need to remove
# x4StarReviews                0.879006394
# x3StarReviews                0.763373189
# x2StarReviews                0.487279328
# x1StarReviews                0.255023904
# PositiveServiceReview        0.622260219
# NegativeServiceReview        0.309418989
# Recommendproduct             0.169541264
# ShippingWeight              -0.188023980
# ProductDepth                 0.066105249
# ProductWidth                -0.143436609
# ProductHeight               -0.160004003
# ProfitMargin                -0.013448603
# Volume                       1.000000000

readyData_existingProductCorFRemoved <- readyData_existingProduct   # make a copy 
readyData_existingProductCorFRemoved$x5StarReviews <- NULL   # remove 5Star
str(readyData_existingProductCorFRemoved) 

#Looking for colinearity amongst predictors where correlation greater than .90
# x2StarReviews::x1StarReviews   0.951912978   
# x3StarReviews::x4StarReviews   0.9372141751  
# 
# Remove feature with lower correlation to volume
# 
# Volume::x1StarReviews   0.255023904
# Volume::x2StarReviews   0.487279328
# Remove feature: x1StarReviews

readyData_existingProductCorFRemoved$x1StarReviews <- NULL   # remove 1Star
str(readyData_existingProductCorFRemoved)
#   
# Volume::x3StarReviews   0.763373189
# Volume::x4StarReviews   0.879006394
# Remove feature:  x3StarReviews

readyData_existingProductCorFRemoved$x3StarReviews <- NULL   # remove 1Star
str(readyData_existingProductCorFRemoved)

# remove obvious features (e.g., ID, other)
#WholeYear7v <- WholeYear   # make a copy 
#WholeYear7v$X <- NULL   # remove ID
#str(WholeYear7v) # 35136 obs. of  7 variables

#Scale features as needed weight and height, price and volume features
names(readyData_existingProductCorFRemoved)
readyData_existingProductCorFRemoved[c(14, 20:24)] <- scale(readyData_existingProductCorFRemoved[c(14, 20:25)])
summary(readyData_existingProductCorFRemoved)

# save preprocessed dataset
write.csv(readyData_existingProductCorFRemoved, file = "existingProductPreprocessed.csv")


#--- Dataset 2 ---###################################################

#dumify the data - convert categorical predictors to (0,1)
newDataFrame2 <- dummyVars(" ~ .", data = newProduct)
readyData_newProduct <- data.frame(predict(newDataFrame2, newdata = newProduct))
#View(readyData_newProduct)

str(readyData_newProduct)

# change data types
#DatasetName$ColumnName <- as.typeofdata(DatasetName$ColumnName)

# rename a column
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 

# check for missin values (NAs)
summary(readyData_newProduct) #best sellers rank has 15 NA's
any(is.na(readyData_newProduct)) #TRUE

# handle missing values 
readyData_newProduct$BestSellersRank <- NULL #delete attribute BestSellersRank due to NA's
#na.omit(DatasetName$ColumnName)
#na.exclude(DatasetName$ColumnName)        
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

#Remove same features as in data set 1
readyData_newProductCorFRemoved <- readyData_newProduct   # make a copy 
readyData_newProductCorFRemoved$x5StarReviews <- NULL   # remove 5Star
readyData_newProductCorFRemoved$x1StarReviews <- NULL   # remove 1Star
readyData_newProductCorFRemoved$x3StarReviews <- NULL   # remove 3Star
str(readyData_newProductCorFRemoved) 

#Scale features as needed weight and height, price and volume features
names(readyData_newProductCorFRemoved)
readyData_newProductCorFRemoved[c(14, 20:24)] <- scale(readyData_newProductCorFRemoved[c(14, 20:25)])
summary(readyData_newProductCorFRemoved)


# save preprocessed dataset
write.csv(readyData_newProductCorFRemoved, file = "newProductPreprocessed.csv")


# ################
# # Sampling
# ################
# 
# # ---- Sampling ---- #
# 
# # Note: The set.seed function has to be executed immediately preceeding any 
# # function that needs a seed value
# 
# # 1k sample
# set.seed(seed)
# WholeYear7v1k <- WholeYear7v[sample(1:nrow(WholeYear7v), 1000, replace=FALSE),]
# head(WholeYear7v1k) # ensure randomness
# nrow(WholeYear7v1k) # ensure number of obs
# 
# # create 10% sample for 7v ds
# set.seed(seed) # set random seed
# WholeYear7v10p <- WholeYear7v[sample(1:nrow(WholeYear7v), round(nrow(WholeYear)*.1),replace=FALSE),]
# nrow(WholeYear7v10p)
# head(WholeYear10p7v) # ensure randomness


##################
# Train/test sets
##################

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(readyData_existingProductCorFRemoved$Volume, p=0.75, list=FALSE)
# create training/testing dataset
trainSet <- readyData_existingProductCorFRemoved[inTraining,]   
testSet <- readyData_existingProductCorFRemoved[-inTraining,]   
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
modelLookup("xgbLinear")


## ------- LM ------- ##

# LM train/fit
set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
lmFit1 <- train(Volume~., data=trainSet, method="lm", trControl=fitControl)
lmFit1 

summary(lmFit1)
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
varImp(lmFit1)


## ------- RF ------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(Volume~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
rfFit1

varImp(rfFit1)


## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(Volume~., data=trainSet, method="svmLinear", trControl=fitControl)
svmFit1

varImp(svmFit1)

## ------- xgbLinear (Gradient Boosted Linear) ------- ##

#getModelInfo("xgbLinear")

# xgbLinear train/fit
set.seed(seed)
xgbFit1 <- train(Volume~., data=trainSet, method="xgbLinear", trControl=fitControl)
xgbFit1

varImp(xgbFit1)

#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults <- resamples(list(lm=lmFit1, rf=rfFit1, svm=svmFit1, xgb=xgbFit1))
# output summary metrics for tuned models 
summary(ModelFitResults)
# Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# readyData_existingProductCorFRemoved

# > summary(ModelFitResults)

# Note: Want highest R2 and lowest RMSE
# 
# Call:
#   summary.resamples(object = ModelFitResults)
# 
# Models: lm, rf, svm, xgb 
# Number of resamples: 10 
# 
# MAE 
# Min.   1st Qu.   Median     Mean  3rd Qu.      Max. NA's
# lm  224.43277 310.19714 351.4798 500.0237 610.8135 1409.7756    0
# rf   43.61290  92.00489 172.9491 368.1852 574.4290 1155.2863    0
# svm 294.62090 330.62673 449.8619 543.9170 614.4721 1300.7094    0
# xgb  34.75731  91.68834 164.9626 269.9347 187.2469  950.1506    0
# 
# RMSE 
#          Min.  1st Qu.   Median      Mean   3rd Qu.     Max. NA's
# lm  327.60791 440.9388 557.4824  823.3742  729.5439 2930.475    0
# rf   59.38242 142.5886 276.6184  796.5734 1314.8725 2748.497    0
# svm 367.96363 475.4135 756.8576 1007.6571 1000.0137 3063.391    0
# xgb  47.81387 151.0851 254.4968  551.9749  361.4943 2105.807    0
# 
# Rsquared 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# lm  0.07194006 0.7046653 0.8174609 0.7688136 0.9489137 0.9926069    0
# rf  0.73002510 0.8233774 0.9503664 0.9051584 0.9915552 0.9994424    0
# svm 0.15309262 0.7026464 0.8420368 0.7699935 0.9454583 0.9970552    0
# xgb 0.80191553 0.8440105 0.9596109 0.9195168 0.9758884 0.9999353    0




##--- Conclusion ---##
# Make a note of which model is the top model, and why

#xgbLinear has the highest R2 and lowest RMSE relative to the other models



########################
# Validate top model
########################

# make predictions with lm Model #######################
lmPred1 <- predict(lmFit1, testSet)

# performace measurment
postResample(lmPred1, testSet$Volume)
# RMSE     Rsquared          MAE 
# 1073.0452303    0.6771022  613.2289834 

# plot predicted verses actual
plot(lmPred1,testSet$Volume)
# print predictions
lmPred1

# ### the warning message: Warning message:
# In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading
# 
# is because there are more predictors than test cases in the data set. 
# one way to address this is add more test cases to the data set. in this
# case I'm not using LM so I'm disregarding this warning

#Also a lot of negative volume values which is not useful

#########################################################

# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, testSet)

# performace measurment
postResample(rfPred1, testSet$Volume)

# RMSE    Rsquared         MAE 
# 966.5925981   0.6186223 335.8232140 

# plot predicted verses actual
plot(rfPred1,testSet$Volume)
# print predictions
rfPred1

#########################################################

# make predictions with svm Model #######################

svmPred1 <- predict(svmFit1, testSet)

# performace measurment
postResample(svmPred1, testSet$Volume)
# RMSE     Rsquared          MAE 
# 1143.2779920    0.7048964  660.4057204 

# plot predicted verses actual
plot(svmPred1,testSet$Volume)
# print predictions
svmPred1

#for svm there are a few negative volume values predicted

#########################################################

# make predictions with xgbLinear Model
xgbPred1 <- predict(xgbFit1, testSet)

# performace measurment
postResample(xgbPred1, testSet$Volume)
# RMSE     Rsquared          MAE 
# 2092.0308502    0.4858962  606.1269125 

# plot predicted verses actual
plot(xgbPred1,testSet$Volume)
# print predictions
xgbPred1

#All positive values for Volume in xgbLinear model

########################
# Predict with top model
########################

#rf is top model based on lowest RMSE on testSet data of 966.592

# make predictions
finalPred <- predict(rfFit1, readyData_newProductCorFRemoved, predict.all = TRUE)
finalPred




########################
# Save validated model
########################

##--- Save top performing model ---##

# save top performing model after it has been validated

# save model 
saveRDS(rfFit1, "Model.RDS")  # Q: What type of object does saveRDS create?

# load and name model to make predictions with new data
#RFfit1 <- readRDS() # Q: What type of object does readRDS create?

output <- read.csv("newproductattributes2017.csv", stringsAsFactors = FALSE)
output$Predictions <- finalPred
write.csv(output, file="C2.T3output.csv", row.names = TRUE)

