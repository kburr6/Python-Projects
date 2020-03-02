# Title: C3T4 Pipeline

# Last update: 02/23/2020

# File/project name: C3T4 Pipeline.R
# RStudio Project name: Task 3 - Develop Models to Predict Sentiment.Rproj

###############
# Project Notes
###############

# The Helio project manager called me yesterday with some new developments. 
# The initial discussions with Apple and Samsung have progressed over the 
# last few weeks. At this point, they would like us to prioritize—and of 
# course speed up if we can—our sentiment analysis of the iPhone and the 
# Galaxy over the other handsets in the short list.
# 
# While you were working on collecting the Large Matrix, an Alert! Analytics
# team has been has manually labeling each instance of two small matrices 
# with sentiment toward iPhone and Samsung Galaxy. Manually labeling means 
# that the team read through each webpage and assigned a sentiment rating 
# based on their findings. I have attached two labelled matrices (one for 
#                                                                 each device).
# 
# Our analytic goal is to build models that understand the patterns in the 
# two small matrices and then use those models with the Large Matrix to
# predict sentiment for iPhone and Galaxy.
# 
# Our next steps are as follows:
#   
#   Set up parallel processing
# Explore the Small Matrices to understand the attributes
# Preprocessing & Feature Selection
# Model Development and Evaluation
# Feature Engineering
# Apply Model to Large Matrix and get Predictions
# Analyze results, write up findings report
# Write lessons learned report
# I would like you to use the R statistical programming language and 
# the caret package to perform this work. To get the best results, I 
# would like you to compare the performance metrics of four different 
# classifiers, namely C5.0, random forest, KKNN and support vector 
# machines. This should be done for both the iPhone and Galaxy data sets.
# 
# After comparing the performance of the classifiers in "out of the box 
# modeling, see if you can improve the performance metrics with feature 
# selection/feature engineering. You should explore the results from 
# several methods. This effort may or may not lead to better classifier 
# performance, but always worth trying.
# 
# After identifying your most optimal model use it to predict sentiment 
# in the Large Matrix.
# 
# In terms of your analysis, Helio prefers short reports rather than 
# presentations, so I would like you to prepare a document that summarizes
# your findings. In this summary, please lay out your interpretation of the
# results; your confidence in the results; and a high-level recap of what 
# you did. 
# 
# In addition to your Summary of Findings for Helio, I would like you 
# to prepare a brief Lessons Learned Report. This report will be valuable 
# tool to improve our processes for these types of projects in the future.

# Comment multiple lines

# WIN: Ctrl + SHIFT + C


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

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

if(!require(beepr)){
  install.packages("beepr")
  library(beepr)
}

if(!require(kknn)){
  install.packages("kknn")
  library(kknn)
}

if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}

#####################
# Parallel processing
#####################

#--- for WIN ---#

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParellel)
}

if(!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}

#parallel processor allocation for performance improvement

detectCores()  # detect number of cores
cl <- makeCluster(5)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio

# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
#stopCluster(cl)



###############
# Import data
##############

#Notes on sentiment values:
# 0: very negative
# 1: negative
# 2: somewhat negative
# 3: somewhat positive
# 4: positive
# 5: very positive

# Header meanings:
# iOS – counts mentions of iOS on a webpage
# iphonecampos – counts positive sentiment mentions of the iphone camera
# galaxydisneg – counts negative sentiment mentions of the Galaxy display
# htcperunc – counts the unclear sentiment mentions of HTC performance

#For iphone sentiment analysis use header definitions:
# iphone - counts mentions of iphone on a webpage
# iphonecampos - counts positive sentiment mentions of the iphone camera
# iphonecamneg - counts negative sentiment mentions of the iphone camera
# iphonecamunc - counts unclear sentiment mentions of the iphone camera
# iphonedispos - counts positive sentiment mentions of the iphone display
# iphonedisneg - counts negative sentiment mentions of the iphone display
# iphonedisunc - counts unclear sentiment mentions of the iphone display
# iphoneperpos - counts positive sentiment mentions of iphone performance
# iphoneperneg - counts negative sentiment mentions of iphone performance
# iphoneperunc - counts unclear sentiment mentions of iphone performance
# iphonesentiment - sentament result from manual processing


# --- Load oob iphone data set --- ############

iphonedfoob <- read.csv("iphone_smallmatrix_labeled_8d.csv", stringsAsFactors = FALSE)
class(iphonedfoob)  # "data.frame"
str(iphonedfoob)   #19937 obs. of  529 variables - all integers
summary(iphonedfoob)


################
# Evaluate data
################

#--- Dataset 1 - iphone data frame ---###############################

names(iphonedfoob) 
head(iphonedfoob)  
tail(iphonedfoob)   #no anomoly for head or tail

# plot - ANY INTERESTING PLOTS FOR OOB DATA??

plot_ly(iphonedfoob, x= ~iphonedfoob$iphonesentiment, type='histogram')
#approx 5@7500 and 0@2000 == 9500/13000 are 0 or 5

# check for missing values 
anyNA(iphonedfoob)  # FALSE
is.na(iphonedfoob)  # All FALSE


#############
# Preprocess
#############

#--- Dataset 1 - iphonedfoob ---#

#feature selection 

#for corr to work the dataframe have to be of type numeric
#so converting all columns to numeric in a new variable
#df[] <- lapply(df, as.numeric)

#create a smaller sample of records from the training set
#trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric[sample(1:nrow(trainingDataWCombinedNumeric), round(nrow(trainingDataWCombinedNumeric)*.1), replace=FALSE),]

#Review correlation matrix  -  are there correlations between attributes (not predicted/label attribute)
#trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric_sample[complete.cases(trainingDataWCombinedNumeric_sample), ]
#corrData <- cor(trainingDataWCombinedNumeric_sample[sapply(trainingDataWCombinedNumeric_sample, is.numeric)], use='pairwise')
iphonecorrData <- cor(iphonedfoob, use = "pairwise.complete.obs")
corrplot(iphonecorrData)
iphonecorrData
write.csv(iphonecorrData, file = "iphoneCorrData.csv")
#collinear predictors with cor > 0.90 are:

#ios:iphone                     == 0.922
#nokiacamneg:nokiacampos        == 0.914
#nokiacampos:nokiacamunc        == 0.940
#htcphone:htcdispos             == 0.978
#nokiacampos:nokiadisneg        == 0.906
#nokiacamunc:nokiadisneg        == 0.904
#nokiadisneg:nokiadispos        == 0.964
#samsungdisunc:samsungdispos    == 0.910
#nokiadisunc:nokiacamunc        == 0.912
#nokiaperpos:nokiacamunc        == 0.903
#samsungperneg:samsungdisneg    == 0.940
#nokiaperpos:nokiaperneg        == 0.957
#samsungdisunc:samsungperunc    == 0.940
#nokiaperunc:nokiacamunc        == 0.958
#nokiadisunc:nokiaperunc        == 0.924
#nokiaperpos:nokiaperunc        == 0.917
#nokiaperneg:nokiaperunc        == 0.905
#iosperpos:iosperneg            == 0.932
#googleperpos:googleperneg      == 0.957
#iosperpos:iosperunc            == 0.905

options(max.print=1000000)

volCorrData <- iphonecorrData[,"iphonesentiment", drop=FALSE]
write.csv(volCorrData, file = "volIphoneCorrData.csv") #no values above .90
#per task list - only remove features highly correlated with the dependant
#variable, which is none

## Examine Feature Variance ##

#nearZeroVar() with saveMetrics = TRUE returns an object containing a 
#table including: frequency ratio, percentage unique, zero variance 
#and near zero variance 

nzvMetrics <- nearZeroVar(iphonedfoob, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(iphonedfoob, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
iphoneNZV <- iphonedfoob[,-nzv]
str(iphoneNZV)

## Recursive Feature Elimination

# Let's sample the data before using RFE
set.seed(seed)
iphoneSample <- iphonedfoob[sample(1:nrow(iphonedfoob), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphoneRFE <- iphonedfoob[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphonedfoob$iphonesentiment

# review outcome
str(iphoneRFE)

## since there are only 5 discreet outcomes for iphone sentiment, this 
##is not a linear regression problem, it's a classification problem
## so the iphonesentiment attribute (dependant variable) needs to 
## be converted to a factor data type

iphonedfoobf <- iphonedfoob
iphonedfoobf$iphonesentiment<- as.factor(iphonedfoobf$iphonesentiment)
is.factor(iphonedfoobf$iphonesentiment)
write.csv(iphonedfoobf, file = "iphonedfoobf.csv")

iphoneNZVf <- iphoneNZV
iphoneNZVf$iphonesentiment <- as.factor(iphoneNZVf$iphonesentiment)
write.csv(iphoneNZVf, file = "iphoneNZVf.csv")

iphoneRFEf <- iphoneRFE
iphoneRFEf$iphonesentiment <- as.factor(iphoneRFEf$iphonesentiment)
write.csv(iphoneRFEf, file = "iphoneRFEf.csv")


# ################
# # Sampling - for training
# ################
# Since the sample data is so clean, no sampling will be done to achieve
# better accuracy


##################
# Train/test sets for OOB
##################

# short cuts to load the data in order to not run the above steps and 
#start at the same place as if having done so
#
#Load data sets for modeling
#
#setwd("C:\\Users\\kburr\\Desktop\\UT Data Analytics Program\\Course 4 - Data Science and Big Data\\Task 3 - Develop Models to Predict Sentiment")
#getwd()
#iphonedfoobf <- read.csv("iphonedfoobf.csv")
#iphonedfoobf$X <- NULL
#iphonedfoobf$iphonesentiment<- as.factor(iphonedfoobf$iphonesentiment)
#iphoneNZVf<- read.csv("iphoneNZVf.csv")
#iphoneRFEf<- read.csv("iphoneRFEf.csv")

# create the training partition that is 70%/30% split of total obs
#uing oob iphone data with iphonesentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(iphonedfoobf$iphonesentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- iphonedfoobf[inTraining,]   
testSet <- iphonedfoobf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(iphonedfoobf)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

#?modelLookup()
#modelLookup("xgbLinear")


## ------- LM ------- ##
##can't use LM because this is classfication problem
# LM train/fit
#set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
#lmFit1 <- train(label_location~., data=trainSet, method="lm", trControl=fitControl)
#lmFit1 

#summary(lmFit1)
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
#varImp(lmFit1)


## ------- RF (Random Forest)------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(iphonesentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "iphonedfoobfrfFit1Model.RDS") 
rfFit1 <- readRDS("iphonedfoobfrfFit1Model.RDS")

#Random Forest 
# 
# 9083 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7018607  0.3759693
# 30    0.7725431  0.5625417
# 58    0.7636254  0.5496866
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 30.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(iphonesentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "iphonedfoobfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

#k-Nearest Neighbors 
# 
# 9083 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.3104693  0.1560163
# 7     0.3230223  0.1594246
# 9     0.3283062  0.1609948
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning
# parameter 'kernel' was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 9, distance = 2 and kernel
# = optimal.

## ------- C5.0 ------- ##
str(trainSet)
is.factor(trainSet$iphonesentiment)
# C5.0 train/fit
set.seed(seed)
c50Fit1 <- train(iphonesentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "iphonedfoobfC50Fit1Model.RDS")
c50Fit1 <- readRDS("iphonedfoobfC50Fit1Model.RDS")

#C5.0 

# 9083 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7721012  0.5572920
# rules  FALSE   10      0.7603238  0.5399142
# rules  FALSE   20      0.7603238  0.5399142
# rules   TRUE    1      0.7734225  0.5601200
# rules   TRUE   10      0.7604328  0.5393405
# rules   TRUE   20      0.7604328  0.5393405
# tree   FALSE    1      0.7729815  0.5592708
# tree   FALSE   10      0.7630753  0.5460562
# tree   FALSE   20      0.7630753  0.5460562
# tree    TRUE    1      0.7728707  0.5593231
# tree    TRUE   10      0.7603195  0.5406196
# tree    TRUE   20      0.7603195  0.5406196
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow
# = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(iphonesentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "iphonedfoobfsvmFit1Model.RDS")
svmFit1 <- readRDS("iphonedfoobfsvmFit1Model.RDS")

#Support Vector Machines with Linear Kernel 
# 
# 9083 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.7078087  0.4046015
# 0.50  0.7088003  0.4107637
# 1.00  0.7086888  0.4123564
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cost = 0.5.

#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults <- resamples(list(rf=rfFit1, kknn=kknnFit1, c50=c50Fit1, svm=svmFit1))
# output summary metrics for tuned models 
summary(ModelFitResults)
# Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# 
# # Note: Want highest Kappa and Accuracy
# # 
# Call:
#   summary.resamples(object = ModelFitResults)
# 
# Models: rf, kknn, c50, svm 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.7599119 0.7635560 0.7717323 0.7725431 0.7806567 0.7920792    0
# kknn 0.3083700 0.3196828 0.3276442 0.3283062 0.3342511 0.3513216    0
# c50  0.7533040 0.7634912 0.7724030 0.7734225 0.7828621 0.7920792    0
# svm  0.6982379 0.6995026 0.7055594 0.7088003 0.7140120 0.7290749    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5332170 0.5425004 0.5613227 0.5625417 0.5804053 0.6065620    0
# kknn 0.1417769 0.1501527 0.1562114 0.1609948 0.1712841 0.1871676    0
# c50  0.5168090 0.5382150 0.5592303 0.5601200 0.5804472 0.6025526    0
# svm  0.3849653 0.3934384 0.4062031 0.4107637 0.4172563 0.4525589    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why

#RF has the highest Kappa and Accuracy of all the models
#BUT...Random Forest and C5.0 we extremely similar


#FUNCTION to write output to text file
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(output.title, modelname) {
  sink(file="modeloutputs.txt", append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("##########################",  row.names=FALSE)
  summary(modelname) #write summary of the model to text file 
}

writeOutput.F("rfFit1", rfFit1)
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnFit1", kknnFit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("c50Fit1", c50Fit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmFit1", svmFit1) 
closeAllConnections() #close all connections and restore all sink di

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("knnPred1 Results", knnPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("C5.0Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di

write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")
write.csv(rfPred1, file = "knnPred1Results.csv", sep = ",")
write.csv(rfPred1, file = "c50Pred1Results.csv", sep = ",")

########################
# Validate top model
########################


# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, testSet)
beep("fanfare")
# performace measurment
postResample(rfPred1, testSet$iphonesentiment)
# 
# Accuracy     Kappa 
# 0.7737789 0.5612870 

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.3383033 0.1713617 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7724936 0.5558736 


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7120823 0.4200502 

# print predictions
svmPred1

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnPred1 Results", kknnPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("C5.0Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmPred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di

write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")
write.csv(kknnPred1, file = "kknnPred1Results.csv", sep = ",")
write.csv(c50Pred1, file = "c50Pred1Results.csv", sep = ",")
write.csv(svmPred1, file = "svmPred1Results.csv", sep = ",")

########################
# Create confusion Matrixes
########################

#Confusion matrix for random forest

cmRF <-confusionMatrix(rfPred1, testSet$iphonesentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$iphonesentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$iphonesentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$iphonesentiment)
print(cmSVM)

as.table(cmSVM)
as.matrix(cmSVM,what="overall")
as.matrix(cmSVM, what = "classes")
write.csv(as.table(cmSVM), file = "cmSVM.csv")
write.csv(as.matrix(cmSVM,what="overall"), file = "cmSVM_overall.csv")
write.csv(as.matrix(cmSVM, what = "classes"), file = "cmSVM_classes.csv")

#Summary of Performance Statistics from confusion matrix results
# RF
#Accuracy     Kappa 
# 0.7738  0.5613

#kknn
# Accuracy     Kappa 
# 0.3383           0.1714          

#C5.0
# Accuracy     Kappa 
# 0.7725          0.5559          

#SVM
# Accuracy     Kappa 
# 0.7121           0.4201           

####################
#PLOT Performance metrics

plotData <- c(0.7738,  0.5613, 0.3383, 0.1714, 0.7725, 0.5559, 0.7121, 0.4201) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.7738, 0.5613), X = c(0.3383, 0.1714), 
                      Y = c(0.7725, 0.5559), Z = c(0.7121, 0.4201)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.7738, 0.3383, 0.7725,0.7121)
y2 <- c(0.5613,0.1714,0.5559,0.4201)
data <- data.frame(x, y, y2)

p <- data %>% 
  plot_ly() %>%
  add_trace(x = ~x, y = ~y, name = "Accuracy", type = 'bar', 
            text = y, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  add_trace(x = ~x, y = ~y2, name = "Kappa", type = 'bar', 
            text = y2, textposition = 'auto',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Performance Metrics OOB",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))


p


#####################################################
# Train/test sets for NZV
#####################################################

# short cuts to load the data in order to not run the above steps and 
#start at the same place as if having done so
#
#Load data sets for modeling
#
#setwd("C:\\Users\\kburr\\Desktop\\UT Data Analytics Program\\Course 4 - Data Science and Big Data\\Task 3 - Develop Models to Predict Sentiment")
#getwd()
#iphoneNZVf <- read.csv("iphoneNZVf.csv")
#iphoneNZVf$X <- NULL
#iphoneNZVf$iphonesentiment<- as.factor(iphoneNZVf$iphonesentiment)
#iphoneNZVf<- read.csv("iphoneNZVf.csv")
#iphoneRFEf<- read.csv("iphoneRFEf.csv")

# create the training partition that is 70%/30% split of total obs
#uing oob iphone data with iphonesentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(iphoneNZVf$iphonesentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- iphoneNZVf[inTraining,]   
testSet <- iphoneNZVf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(iphoneNZVf)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

#?modelLookup()
#modelLookup("xgbLinear")


## ------- LM ------- ##
##can't use LM because this is classfication problem
# LM train/fit
#set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
#lmFit1 <- train(label_location~., data=trainSet, method="lm", trControl=fitControl)
#lmFit1 

#summary(lmFit1)
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
#varImp(lmFit1)


## ------- RF (Random Forest)------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(iphonesentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "iphoneNZVfrfFit1Model.RDS") 
rfFit1 <- readRDS("iphoneNZVfrfFit1Model.RDS")

# Random Forest 
# 
# 9083 samples
# 11 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7603223  0.5278880
# 6    0.7570193  0.5273268
# 11    0.7483231  0.5152035
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 2.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(iphonesentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "iphoneNZVfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

# k-Nearest Neighbors 
# 
# 9083 samples
# 11 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.2881226  0.1234423
# 7     0.3045270  0.1342014
# 9     0.3094810  0.1359983
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning
# parameter 'kernel' was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 9, distance = 2 and kernel
# = optimal.

## ------- C5.0 ------- ##
# C5.0 train/fit

set.seed(seed)
c50Fit1 <- train(iphonesentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "iphoneNZVfC50Fit1Model.RDS")
c50Fit1 <- readRDS("iphoneNZVfC50Fit1Model.RDS")

# C5.0 
# 
# 9083 samples
# 11 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7563590  0.5205773
# rules  FALSE   10      0.7399570  0.4919145
# rules  FALSE   20      0.7399570  0.4919145
# rules   TRUE    1      0.7565791  0.5208229
# rules   TRUE   10      0.7416073  0.4938208
# rules   TRUE   20      0.7416073  0.4938208
# tree   FALSE    1      0.7551475  0.5191686
# tree   FALSE   10      0.7431470  0.5005746
# tree   FALSE   20      0.7431470  0.5005746
# tree    TRUE    1      0.7556973  0.5203736
# tree    TRUE   10      0.7438061  0.5001276
# tree    TRUE   20      0.7438061  0.5001276
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow
# = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(iphonesentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "iphoneNZVfsvmFit1Model.RDS")
svmFit1 <- readRDS("iphoneNZVfsvmFit1Model.RDS")

# Support Vector Machines with Linear Kernel 
# 
# 9083 samples
# 11 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.6727926  0.3223785
# 0.50  0.6753272  0.3279394
# 1.00  0.6832542  0.3450395
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cost = 1.

#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults <- resamples(list(rf=rfFit1, kknn=kknnFit1, c50=c50Fit1, svm=svmFit1))
# output summary metrics for tuned models 
summary(ModelFitResults)
# Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# 
# # Note: Want highest Kappa and Accuracy
# # 
# Call:
#   summary.resamples(object = ModelFitResults)
# 
# Models: rf, kknn, c50, svm 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.7488987 0.7528882 0.7567412 0.7603223 0.7685321 0.7755776    0
# kknn 0.2907489 0.3017621 0.3109519 0.3094810 0.3172671 0.3248899    0
# c50  0.7477974 0.7493114 0.7555066 0.7565791 0.7639139 0.7676211    0
# svm  0.6651982 0.6771256 0.6842644 0.6832542 0.6900637 0.6960352    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5025949 0.5092243 0.5205530 0.5278880 0.5465294 0.5619080    0
# kknn 0.1170142 0.1273923 0.1335672 0.1359983 0.1453112 0.1560276    0
# c50  0.4999167 0.5043918 0.5179636 0.5208229 0.5373982 0.5456134    0
# svm  0.3096490 0.3295143 0.3488252 0.3450395 0.3575829 0.3754209    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why

#C5.0 has the highest Kappa and Accuracy of all the models
#BUT...Random Forest and C5.0 we extremely similar


#FUNCTION to write output to text file
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(output.title, modelname) {
  sink(file="modeloutputs.txt", append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("##########################",  row.names=FALSE)
  summary(modelname) #write summary of the model to text file 
}

writeOutput.F("rfFit1", rfFit1)
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnFit1", kknnFit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("c50Fit1", c50Fit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmFit1", svmFit1) 
closeAllConnections() #close all connections and restore all sink di


########################
# Validate top model
########################


# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, testSet)
beep("fanfare")
# performace measurment
postResample(rfPred1, testSet$iphonesentiment)
# 
# Accuracy     Kappa 
# 0.7586118 0.5218854 

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.3236504 0.1482012 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7588689 0.5245481 


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.6871465 0.3548189 

# print predictions
svmPred1

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnPred1 Results", kknnPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("C5.0Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmPred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di

write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")
write.csv(kknnPred1, file = "kknnPred1Results.csv", sep = ",")
write.csv(c50Pred1, file = "c50Pred1Results.csv", sep = ",")
write.csv(svmPred1, file = "svmPred1Results.csv", sep = ",")

########################
# Create confusion Matrixes
########################

#Confusion matrix for random forest

cmRF <-confusionMatrix(rfPred1, testSet$iphonesentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$iphonesentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$iphonesentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$iphonesentiment)
print(cmSVM)

as.table(cmSVM)
as.matrix(cmSVM,what="overall")
as.matrix(cmSVM, what = "classes")
write.csv(as.table(cmSVM), file = "cmSVM.csv")
write.csv(as.matrix(cmSVM,what="overall"), file = "cmSVM_overall.csv")
write.csv(as.matrix(cmSVM, what = "classes"), file = "cmSVM_classes.csv")

#Summary of Performance Statistics from confusion matrix results
# RF
#Accuracy     Kappa 
# 0.7586           0.5219         

#kknn
# Accuracy     Kappa 
# 0.3237         0.1482                   

#C5.0
# Accuracy     Kappa 
# 0.7589          0.5245                    

#SVM
# Accuracy     Kappa 
# 0.6871       0.3548                  

####################
#PLOT Performance metrics

plotData <- c(0.7586,  0.5219, 0.3237, 0.1482, 0.7589, 0.5245, 0.6871, 0.3548) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.7586,  0.5219), X = c(0.3237, 0.1482), 
                       Y = c(0.7589, 0.5245), Z = c(0.6871, 0.3548)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics NZV \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.7586, 0.3237, 0.7589,0.6871)
y2 <- c(0.5219,0.1482,0.5245,0.3548)
data <- data.frame(x, y, y2)

p <- data %>% 
  plot_ly() %>%
  add_trace(x = ~x, y = ~y, name = "Accuracy", type = 'bar', 
            text = y, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  add_trace(x = ~x, y = ~y2, name = "Kappa", type = 'bar', 
            text = y2, textposition = 'auto',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Performance Metrics NZV",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))


p

#####################################################
# Train/test sets for RFE
#####################################################

# short cuts to load the data in order to not run the above steps and 
#start at the same place as if having done so
#
#Load data sets for modeling
#
#setwd("C:\\Users\\kburr\\Desktop\\UT Data Analytics Program\\Course 4 - Data Science and Big Data\\Task 3 - Develop Models to Predict Sentiment")
#getwd()
iphoneRFEf <- read.csv("iphoneRFEf.csv")
iphoneRFEf$X <- NULL
iphoneRFEf$iphonesentiment<- as.factor(iphoneRFEf$iphonesentiment)


# create the training partition that is 70%/30% split of total obs
#uing oob iphone data with iphonesentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(iphoneRFEf$iphonesentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- iphoneRFEf[inTraining,]   
testSet <- iphoneRFEf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(iphoneRFEf)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

#?modelLookup()
#modelLookup("xgbLinear")


## ------- LM ------- ##
##can't use LM because this is classfication problem
# LM train/fit
#set.seed(seed)
#lmFit1 <- train(SolarRad~., data=trainSet, method="leapSeq", trControl=fitControl)
#lmFit1 <- train(label_location~., data=trainSet, method="lm", trControl=fitControl)
#lmFit1 

#summary(lmFit1)
# Evaluate performance metrics, but don't add as comments to script file. Performance
# metrics will be added as comments to the script file in the Evaluate models below

# evaluate var importance
#varImp(lmFit1)


## ------- RF (Random Forest)------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(iphonesentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "iphoneRFEfrfFit1Model.RDS") 
rfFit1 <- readRDS("iphoneRFEfrfFit1Model.RDS")

# Random Forest 
# 
# 9083 samples
# 20 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7293833  0.4487126
# 11    0.7726527  0.5632152
# 20    0.7639558  0.5507869
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 11.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(iphonesentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "iphoneRFEfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

# k-Nearest Neighbors 
# 
# 9083 samples
# 20 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.3175141  0.1597982
# 7     0.3264328  0.1629105
# 9     0.3319375  0.1644631
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning
# parameter 'kernel' was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 9, distance = 2 and kernel
# = optimal.

## ------- C5.0 ------- ##
# C5.0 train/fit

set.seed(seed)
c50Fit1 <- train(iphonesentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "iphoneRFEfC50Fit1Model.RDS")
c50Fit1 <- readRDS("iphoneRFEfC50Fit1Model.RDS")

# C5.0 
# 
# 9083 samples
# 20 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7724317  0.5580689
# rules  FALSE   10      0.7588921  0.5375464
# rules  FALSE   20      0.7588921  0.5375464
# rules   TRUE    1      0.7732014  0.5594851
# rules   TRUE   10      0.7597727  0.5402694
# rules   TRUE   20      0.7597727  0.5402694
# tree   FALSE    1      0.7714409  0.5565635
# tree   FALSE   10      0.7593347  0.5402926
# tree   FALSE   20      0.7593347  0.5402926
# tree    TRUE    1      0.7727613  0.5590413
# tree    TRUE   10      0.7616436  0.5429762
# tree    TRUE   20      0.7616436  0.5429762
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow
# = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(iphonesentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "iphoneRFEfsvmFit1Model.RDS")
svmFit1 <- readRDS("iphoneRFEfsvmFit1Model.RDS")

# Support Vector Machines with Linear Kernel 
# 
# 9083 samples
# 20 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.7013142  0.3939307
# 0.50  0.7078076  0.4116095
# 1.00  0.7109974  0.4192838
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was cost = 1.

#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults <- resamples(list(rf=rfFit1, kknn=kknnFit1, c50=c50Fit1, svm=svmFit1))
# output summary metrics for tuned models 
summary(ModelFitResults)
# Make a note of the dataset that the performance metrics belong to.
# Note performance metrics. Add the summary output as a comment.

# 
# # Note: Want highest Kappa and Accuracy
# # 
# Call:
#   summary.resamples(object = ModelFitResults)
# 
# Models: rf, kknn, c50, svm 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.7610132 0.7630050 0.7711793 0.7726527 0.7798298 0.7898790    0
# kknn 0.3105727 0.3257159 0.3318693 0.3319375 0.3415917 0.3480176    0
# c50  0.7555066 0.7648678 0.7729524 0.7732014 0.7787276 0.7920792    0
# svm  0.6982379 0.7049270 0.7112153 0.7109974 0.7188100 0.7227723    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5355116 0.5419094 0.5607240 0.5632152 0.5800644 0.6026125    0
# kknn 0.1392308 0.1543536 0.1660002 0.1644631 0.1772311 0.1825231    0
# c50  0.5206381 0.5403563 0.5601190 0.5594851 0.5719873 0.6029448    0
# svm  0.3860702 0.4031632 0.4213799 0.4192838 0.4354604 0.4493902    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why

# Random Forest has the highest Kappa and Accuracy of all the models
#BUT...Random Forest and C5.0 we extremely similar


#FUNCTION to write output to text file
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(output.title, modelname) {
  sink(file="modeloutputs.txt", append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("##########################",  row.names=FALSE)
  summary(modelname) #write summary of the model to text file 
}

writeOutput.F("rfFit1", rfFit1)
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnFit1", kknnFit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("c50Fit1", c50Fit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmFit1", svmFit1) 
closeAllConnections() #close all connections and restore all sink di


########################
# Validate top model
########################


# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, testSet)
beep("fanfare")
# performace measurment
postResample(rfPred1, testSet$iphonesentiment)
# 
# Accuracy     Kappa 
# 0.7724936 0.5593859 

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.3437018 0.1757024 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7701799 0.5520520  


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$iphonesentiment)
# Accuracy     Kappa 
# 0.7107969 0.4183326 

# print predictions
svmPred1

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("kknnPred1 Results", kknnPred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("C5.0Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("svmPred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di

write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")
write.csv(kknnPred1, file = "kknnPred1Results.csv", sep = ",")
write.csv(c50Pred1, file = "c50Pred1Results.csv", sep = ",")
write.csv(svmPred1, file = "svmPred1Results.csv", sep = ",")

########################
# Create confusion Matrixes
########################

#Confusion matrix for random forest

cmRF <-confusionMatrix(rfPred1, testSet$iphonesentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$iphonesentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$iphonesentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$iphonesentiment)
print(cmSVM)

as.table(cmSVM)
as.matrix(cmSVM,what="overall")
as.matrix(cmSVM, what = "classes")
write.csv(as.table(cmSVM), file = "cmSVM.csv")
write.csv(as.matrix(cmSVM,what="overall"), file = "cmSVM_overall.csv")
write.csv(as.matrix(cmSVM, what = "classes"), file = "cmSVM_classes.csv")

#Summary of Performance Statistics from confusion matrix results
# RF
#Accuracy     Kappa 
# 0.7725          0.5594         

#kknn
# Accuracy     Kappa 
# 0.3437         0.1757                   

#C5.0
# Accuracy     Kappa 
# 0.7702           0.5521                              

#SVM
# Accuracy     Kappa 
# 0.7108         0.4183                  

####################
#PLOT Performance metrics

plotData <- c(0.7725,  0.5594, 0.3437, 0.1757, 0.7702, 0.5521, 0.7108, 0.4183) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.7725,  0.5594), X = c(0.3437, 0.1757), 
                       Y = c(0.7702, 0.5521), Z = c(0.7108, 0.4183)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics RFE \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.7725, 0.3437, 0.7702,0.7108)
y2 <- c(0.5594,0.1757,0.5521,0.4183)
data <- data.frame(x, y, y2)

p <- data %>% 
  plot_ly() %>%
  add_trace(x = ~x, y = ~y, name = "Accuracy", type = 'bar', 
            text = y, textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  add_trace(x = ~x, y = ~y2, name = "Kappa", type = 'bar', 
            text = y2, textposition = 'auto',
            marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "Performance Metrics RFE",
         barmode = 'group',
         xaxis = list(title = ""),
         yaxis = list(title = ""))


p


#############################################
# Summary of Best Performing Model and Dataset without feature engineering
#############################################

# RF with iphonedfoobf dataset
#Accuracy     Kappa 
# 0.7738  0.5613

#C5.0 with iphoneNZVf dataset
# Accuracy     Kappa 
# 0.7589          0.5245   

# RF with iphoneRFEf dataset
#Accuracy     Kappa 
# 0.7725          0.5594 

#Random Forest aaplied to out of the box data set had the best performance


#############################################
# Feature Engineering for Improved Performance
#############################################

#so what if we remapped the sentiment ratings:
# 1: negative
# 
# 2: somewhat negative
# 
# 3: somewhat positive
# 
# 4: positive

iphonedfoobf <- read.csv("iphonedfoobf.csv", header = TRUE, sep = ",")
iphonedfoobf$X <- NULL
iphonedfoobf$iphonesentiment <- as.factor(iphonedfoobf$iphonesentiment)
# create a new dataset that will be used for recoding sentiment
iphoneoobfRC <- iphonedfoobf
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneoobfRC$iphonesentiment <- recode(iphoneoobfRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(iphoneoobfRC)
str(iphoneoobfRC)
# make iphonesentiment a factor
iphoneoobfRC$iphonesentiment <- as.factor(iphoneoobfRC$iphonesentiment)
write.csv(iphoneoobfRC, file = "iphoneoobfRC.csv")

##### now apply the best model so far with this new data set

# create the training partition that is 70%/30% split of total obs
#uing oob iphone data with iphonesentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(iphoneoobfRC$iphonesentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- iphoneoobfRC[inTraining,]   
testSet <- iphoneoobfRC[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(iphoneoobfRC)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

## ------- RF (Random Forest)------- ##

# RF train/fit
set.seed(seed)
str(trainSet)
system.time(rfFit1 <- train(iphonesentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "iphoneoobfRCFit1Model.RDS") 
rfFit1 <- readRDS("iphoneoobfRCFit1Model.RDS")

# Random Forest 
# 
# 9083 samples
# 58 predictor
# 4 classes: '1', '2', '3', '4' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8176, 8175, 8175, 8174, 8174, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7744111  0.3642213
# 30    0.8481769  0.6238733
# 58    0.8426709  0.6145671
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 30.

writeOutput.F("rfFit1", rfFit1)
closeAllConnections() #close all connections and restore all sink di

########################
# Validate top model
########################


# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, testSet)
beep("fanfare")
# performace measurment
postResample(rfPred1, testSet$iphonesentiment)
# 
# Accuracy     Kappa 
# 0.9863753 0.9715681 

# print predictions
rfPred1

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")

#These results are FAR better
# confusion matrix for rf model with iphoneoobfRC data set

cmRF <-confusionMatrix(rfPred1, testSet$iphonesentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")


########################
# Principle Component Analysis
########################

#iphonedfoobf <- read.csv(file = "iphonedfoobf.csv", header = TRUE, sep = ",")
#iphonedfoobf$X <- NULL
#iphonedfoobf$iphonesentiment <- as.factor(iphonedfoobf$iphonesentiment)
#set.seed(seed) # set random seed
#use non-factor dependant for partion creation
#inTraining <- createDataPartition(iphonedfoobf$iphonesentiment, p=.70, list=FALSE)
# create training/testing dataset
#trainSet <- iphonedfoobf[inTraining,]   
#testSet <- iphonedfoobf[-inTraining,]   
# verify number of obs 
#nrow(trainSet)  
#nrow(testSet)   
#nrow(iphonedfoobf)


# data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(trainSet[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, trainSet[,-59])

# add the dependent to training
train.pca$iphonesentiment <- trainSet$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testSet[,-59])

# add the dependent to training
test.pca$iphonesentiment <- testSet$iphonesentiment

# inspect results
str(train.pca)
str(test.pca)


################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

## ------- RF (Random Forest)------- ##

# RF train/fit
set.seed(seed)
system.time(rfFit1 <- train(iphonesentiment~., data=train.pca, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "iphoneoobfPCAFit1Model.RDS") 
rfFit1 <- readRDS("iphoneoobfPCAFit1Model.RDS")

# Random Forest 
# 
# 9083 samples
# 25 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8174, 8173, 8175, 8175, 8175, 8174, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7612034  0.5413906
# 13    0.7618617  0.5435102
# 25    0.7583389  0.5374713
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 13.

writeOutput.F("rfFit1", rfFit1)
closeAllConnections() #close all connections and restore all sink di

########################
# Validate top model
########################


# make predictions with rf Model #######################

rfPred1 <- predict(rfFit1, test.pca)
beep("fanfare")
# performace measurment
postResample(rfPred1, testSet$iphonesentiment)
# 
# Accuracy     Kappa 
# 0.7640103 0.5435245 

# print predictions
rfPred1

writeOutput.F("rfPred1 Results", rfPred1) 
closeAllConnections() #close all connections and restore all sink di
write.csv(rfPred1, file = "rfPred1Results.csv", sep = ",")

#These results are poorer than oob
# confusion matrix for rf model with iphoneoobfRC data set

cmRF <-confusionMatrix(rfPred1, test.pca$iphonesentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

########################
# Predict with top model
########################

#RF oob with feature engineering is the best performing model
#repeat the feature engineering steps on the largematrix file

iphonedfoobf <- read.csv("LargeMatrix.csv", header = TRUE, sep = ",")
str(iphonedfoobf)
iphonedfoobf$X <- NULL
iphonedfoobf$iphonesentiment <- as.factor(iphonedfoobf$iphonesentiment)
# create a new dataset that will be used for recoding sentiment
iphoneoobfRC <- iphonedfoobf
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneoobfRC$iphonesentiment <- recode(iphoneoobfRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(iphoneoobfRC)
str(iphoneoobfRC)
# make iphonesentiment a factor
iphoneoobfRC$iphonesentiment <- as.factor(iphoneoobfRC$iphonesentiment)

write.csv(iphoneoobfRC, file = "iphoneoobfRCLargeMatrix.csv")

rfFit1 <- readRDS("iphoneoobfRCFit1Model.RDS")
# make predictions
str(iphoneoobfRC)
finalPred <- predict(rfFit1, iphoneoobfRC)
finalPred
write.csv(finalPred, file = "iphoneoobfRCLargeMatrixRFFinalPred.csv")

summary(finalPred)

#1     2     3     4 
#14277  2676  2732 15542 

# create a data frame for plotting.
# you can add more sentiment levels if needed
# Replace sentiment values 
pieData <- data.frame(COM = c("negative", "somewhat negative", "somewhat positive","positive"), 
                      values = c(14277 , 2676  ,2732, 15542 ))

# create pie chart
totals<-plot_ly(pieData, labels = ~COM, values = ~ values, type = "pie",
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste( values),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = F) %>%
  layout(title = 'iPhone Sentiment', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

totals

