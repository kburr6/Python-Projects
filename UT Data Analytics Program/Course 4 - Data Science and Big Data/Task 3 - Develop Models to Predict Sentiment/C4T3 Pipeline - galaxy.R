# Title: C3T4 Pipeline - galaxy

# Last update: 02/23/2020

# File/project name: C3T4 Pipeline - galaxy.R
# RStudio Project name: Task 3 - Develop Models to Predict Sentiment.Rproj

###############
# Project Notes
###############

# The Helio project manager called me yesterday with some new developments. 
# The initial discussions with Apple and Samsung have progressed over the 
# last few weeks. At this point, they would like us to prioritize—and of 
# course speed up if we can—our sentiment analysis of the galaxy and the 
# Galaxy over the other handsets in the short list.
# 
# While you were working on collecting the Large Matrix, an Alert! Analytics
# team has been has manually labeling each instance of two small matrices 
# with sentiment toward galaxy and Samsung Galaxy. Manually labeling means 
# that the team read through each webpage and assigned a sentiment rating 
# based on their findings. I have attached two labelled matrices (one for 
#                                                                 each device).
# 
# Our analytic goal is to build models that understand the patterns in the 
# two small matrices and then use those models with the Large Matrix to
# predict sentiment for galaxy and Galaxy.
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
# machines. This should be done for both the galaxy and Galaxy data sets.
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

# --- Load oob galaxy data set --- ############

galaxydfoob <- read.csv("galaxy_smallmatrix_labeled_9d.csv", stringsAsFactors = FALSE)
class(galaxydfoob)  # "data.frame"
str(galaxydfoob)   #19937 obs. of  529 variables - all integers
summary(galaxydfoob)


################
# Evaluate data
################

#--- Dataset 1 - galaxy data frame ---###############################

names(galaxydfoob) 
head(galaxydfoob)  
tail(galaxydfoob)   #no anomoly for head or tail

# plot - ANY INTERESTING PLOTS FOR OOB DATA??

plot_ly(galaxydfoob, x= ~galaxydfoob$galaxysentiment, type='histogram')
#approx 5@7500 and 0@2000 == 9500/13000 are 0 or 5

# check for missing values 
anyNA(galaxydfoob)  # FALSE
is.na(galaxydfoob)  # All FALSE


#############
# Preprocess
#############

#--- Dataset 1 - galaxydfoob ---#

#feature selection 

#for corr to work the dataframe have to be of type numeric
#so converting all columns to numeric in a new variable
#df[] <- lapply(df, as.numeric)

#create a smaller sample of records from the training set
#trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric[sample(1:nrow(trainingDataWCombinedNumeric), round(nrow(trainingDataWCombinedNumeric)*.1), replace=FALSE),]

#Review correlation matrix  -  are there correlations between attributes (not predicted/label attribute)
#trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric_sample[complete.cases(trainingDataWCombinedNumeric_sample), ]
#corrData <- cor(trainingDataWCombinedNumeric_sample[sapply(trainingDataWCombinedNumeric_sample, is.numeric)], use='pairwise')
galaxycorrData <- cor(galaxydfoob, use = "pairwise.complete.obs")
corrplot(galaxycorrData)
galaxycorrData
write.csv(galaxycorrData, file = "galaxyCorrData.csv")


options(max.print=1000000)

volCorrData <- galaxycorrData[,"galaxysentiment", drop=FALSE]
write.csv(volCorrData, file = "volgalaxyCorrData.csv") #no values above .90
#per task list - only remove features highly correlated with the dependant
#variable, which is none

## Examine Feature Variance ##

#nearZeroVar() with saveMetrics = TRUE returns an object containing a 
#table including: frequency ratio, percentage unique, zero variance 
#and near zero variance 

nzvMetrics <- nearZeroVar(galaxydfoob, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(galaxydfoob, saveMetrics = FALSE) 
nzv

# create a new data set and remove near zero variance features
galaxyNZV <- galaxydfoob[,-nzv]
str(galaxyNZV)

## Recursive Feature Elimination

# Let's sample the data before using RFE
set.seed(seed)
galaxySample <- galaxydfoob[sample(1:nrow(galaxydfoob), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 galaxysentiment) 
rfeResults <- rfe(galaxySample[,1:58], 
                  galaxySample$galaxysentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
galaxyRFE <- galaxydfoob[,predictors(rfeResults)]

# add the dependent variable to galaxyRFE
galaxyRFE$galaxysentiment <- galaxydfoob$galaxysentiment

# review outcome
str(galaxyRFE)

## since there are only 5 discreet outcomes for galaxy sentiment, this 
##is not a linear regression problem, it's a classification problem
## so the galaxysentiment attribute (dependant variable) needs to 
## be converted to a factor data type

galaxydfoobf <- galaxydfoob
galaxydfoobf$galaxysentiment<- as.factor(galaxydfoobf$galaxysentiment)
is.factor(galaxydfoobf$galaxysentiment)
write.csv(galaxydfoobf, file = "galaxydfoobf.csv")

galaxyNZVf <- galaxyNZV
galaxyNZVf$galaxysentiment <- as.factor(galaxyNZVf$galaxysentiment)
write.csv(galaxyNZVf, file = "galaxyNZVf.csv")

galaxyRFEf <- galaxyRFE
galaxyRFEf$galaxysentiment <- as.factor(galaxyRFEf$galaxysentiment)
write.csv(galaxyRFEf, file = "galaxyRFEf.csv")


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
#galaxydfoobf <- read.csv("galaxydfoobf.csv")
#galaxydfoobf$X <- NULL
#galaxydfoobf$galaxysentiment<- as.factor(galaxydfoobf$galaxysentiment)
#galaxyNZVf<- read.csv("galaxyNZVf.csv")
#galaxyRFEf<- read.csv("galaxyRFEf.csv")

# create the training partition that is 70%/30% split of total obs
#uing oob galaxy data with galaxysentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(galaxydfoobf$galaxysentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- galaxydfoobf[inTraining,]   
testSet <- galaxydfoobf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(galaxydfoobf)

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
system.time(rfFit1 <- train(galaxysentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "galaxydfoobfrfFit1Model.RDS") 
rfFit1 <- readRDS("galaxydfoobfrfFit1Model.RDS")

# Random Forest 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7058651  0.3586770
# 30    0.7639392  0.5303086
# 58    0.7566385  0.5206378
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 30.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(galaxysentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "galaxydfoobfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

# k-Nearest Neighbors 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.6591826  0.4097381
# 7     0.7336326  0.4887705
# 9     0.7209199  0.4785159
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning
# parameter 'kernel' was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 7, distance = 2 and kernel
# = optimal.

## ------- C5.0 ------- ##
str(trainSet)
is.factor(trainSet$galaxysentiment)
# C5.0 train/fit
set.seed(seed)
c50Fit1 <- train(galaxysentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "galaxydfoobfC50Fit1Model.RDS")
c50Fit1 <- readRDS("galaxydfoobfC50Fit1Model.RDS")

# C5.0 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7653784  0.5294103
# rules  FALSE   10      0.7537646  0.5086583
# rules  FALSE   20      0.7537646  0.5086583
# rules   TRUE    1      0.7658209  0.5298597
# rules   TRUE   10      0.7508891  0.4990666
# rules   TRUE   20      0.7508891  0.4990666
# tree   FALSE    1      0.7628352  0.5249169
# tree   FALSE   10      0.7567509  0.5144032
# tree   FALSE   20      0.7567509  0.5144032
# tree    TRUE    1      0.7636093  0.5258885
# tree    TRUE   10      0.7574152  0.5167121
# tree    TRUE   20      0.7574152  0.5167121
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow
# = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(galaxysentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "galaxydfoobfsvmFit1Model.RDS")
svmFit1 <- readRDS("galaxydfoobfsvmFit1Model.RDS")

# Support Vector Machines with Linear Kernel 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.6996676  0.3587314
# 0.50  0.7063032  0.3796011
# 1.00  0.7038711  0.3794882
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
# rf   0.7535912 0.7594687 0.7645438 0.7639392 0.7693584 0.7743363    0
# kknn 0.6983425 0.7269973 0.7362905 0.7336326 0.7419801 0.7511062    0
# c50  0.7580110 0.7602210 0.7652280 0.7658209 0.7706774 0.7765487    0
# svm  0.6924779 0.7035912 0.7078029 0.7063032 0.7110066 0.7146018    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5052111 0.5211696 0.5318253 0.5303086 0.5393723 0.5575496    0
# kknn 0.4369133 0.4766108 0.4900178 0.4887705 0.5094091 0.5172740    0
# c50  0.5118590 0.5161435 0.5262821 0.5298597 0.5443783 0.5514706    0
# svm  0.3333952 0.3758177 0.3877268 0.3796011 0.3904645 0.3981902    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why

#C50 has the highest Kappa and Accuracy of all the models
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
postResample(rfPred1, testSet$galaxysentiment)
# 
# Accuracy     Kappa 
# 0.7669853 0.5352832

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7318522 0.4877367 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7680186 0.5323955 


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.6974942 0.3642843  

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

cmRF <-confusionMatrix(rfPred1, testSet$galaxysentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$galaxysentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$galaxysentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$galaxysentiment)
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
# 0.767             0.5353          

#kknn
# Accuracy     Kappa 
# 0.7319           0.4877                    

#C5.0
# Accuracy     Kappa 
# 0.768          0.5324                    

#SVM
# Accuracy     Kappa 
# 0.6975           0.3643                     

####################
#PLOT Performance metrics

plotData <- c(0.767,  0.5353, 0.7319, 0.4877, 0.768, 0.5324, 0.6975, 0.3643) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.767,  0.5353), X = c(0.7319, 0.4877), 
                      Y = c( 0.768, 0.5324), Z = c(0.6975, 0.3643)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.767, 0.7319, 0.768,0.6975)
y2 <- c(0.5353,0.4877,0.5324,0.3643)
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
  layout(title = "Performance Metrics",
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
#galaxyNZVf <- read.csv("galaxyNZVf.csv")
#galaxyNZVf$X <- NULL
#galaxyNZVf$galaxysentiment<- as.factor(galaxyNZVf$galaxysentiment)
#galaxyNZVf<- read.csv("galaxyNZVf.csv")
#galaxyRFEf<- read.csv("galaxyRFEf.csv")

# create the training partition that is 70%/30% split of total obs
#uing oob galaxy data with galaxysentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(galaxyNZVf$galaxysentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- galaxyNZVf[inTraining,]   
testSet <- galaxyNZVf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(galaxyNZVf)

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
system.time(rfFit1 <- train(galaxysentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "galaxyNZVfrfFit1Model.RDS") 
rfFit1 <- readRDS("galaxyNZVfrfFit1Model.RDS")

# Random Forest 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7058651  0.3586770
# 30    0.7639392  0.5303086
# 58    0.7566385  0.5206378
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 30.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(galaxysentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "galaxyNZVfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

# k-Nearest Neighbors 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.6591826  0.4097381
# 7     0.7336326  0.4887705
# 9     0.7209199  0.4785159
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning
# parameter 'kernel' was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 7, distance = 2 and kernel
# = optimal.

## ------- C5.0 ------- ##
# C5.0 train/fit

set.seed(seed)
c50Fit1 <- train(galaxysentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "galaxyNZVfC50Fit1Model.RDS")
c50Fit1 <- readRDS("galaxyNZVfC50Fit1Model.RDS")

# C5.0 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7653784  0.5294103
# rules  FALSE   10      0.7537646  0.5086583
# rules  FALSE   20      0.7537646  0.5086583
# rules   TRUE    1      0.7658209  0.5298597
# rules   TRUE   10      0.7508891  0.4990666
# rules   TRUE   20      0.7508891  0.4990666
# tree   FALSE    1      0.7628352  0.5249169
# tree   FALSE   10      0.7567509  0.5144032
# tree   FALSE   20      0.7567509  0.5144032
# tree    TRUE    1      0.7636093  0.5258885
# tree    TRUE   10      0.7574152  0.5167121
# tree    TRUE   20      0.7574152  0.5167121
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow
# = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(galaxysentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "galaxyNZVfsvmFit1Model.RDS")
svmFit1 <- readRDS("galaxyNZVfsvmFit1Model.RDS")

# Support Vector Machines with Linear Kernel 
# 
# 9040 samples
# 58 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.6996676  0.3587314
# 0.50  0.7063032  0.3796011
# 1.00  0.7038711  0.3794882
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
# rf   0.7535912 0.7594687 0.7645438 0.7639392 0.7693584 0.7743363    0
# kknn 0.6983425 0.7269973 0.7362905 0.7336326 0.7419801 0.7511062    0
# c50  0.7580110 0.7602210 0.7652280 0.7658209 0.7706774 0.7765487    0
# svm  0.6924779 0.7035912 0.7078029 0.7063032 0.7110066 0.7146018    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5052111 0.5211696 0.5318253 0.5303086 0.5393723 0.5575496    0
# kknn 0.4369133 0.4766108 0.4900178 0.4887705 0.5094091 0.5172740    0
# c50  0.5118590 0.5161435 0.5262821 0.5298597 0.5443783 0.5514706    0
# svm  0.3333952 0.3758177 0.3877268 0.3796011 0.3904645 0.3981902    0


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
postResample(rfPred1, testSet$galaxysentiment)
# 
# Accuracy     Kappa 
# 0.7669853 0.5352832 

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7318522 0.4877367

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7680186 0.5323955 


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.6974942 0.3642843 

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

cmRF <-confusionMatrix(rfPred1, testSet$galaxysentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$galaxysentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$galaxysentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$galaxysentiment)
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
# 0.767           0.5353                   
#kknn
# Accuracy     Kappa 
# 0.7319         0.4877                             
#C5.0
# Accuracy     Kappa 
# 0.768           0.5324                              
#SVM
# Accuracy     Kappa 
# 0.6975         0.3643                            

####################
#PLOT Performance metrics

plotData <- c(0.767,  0.5353, 0.7319, 0.4877, 0.768, 0.5324, 0.6975, 0.3643) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.767,  0.5353), X = c(0.7319, 0.4877), 
                       Y = c(0.768, 0.5324), Z = c(0.6975, 0.3643)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics NZV \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.767, 0.7319, 0.768,0.6975)
y2 <- c(0.5353,0.4877,0.5324,0.3643)
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
galaxyRFEf <- read.csv("galaxyRFEf.csv")
galaxyRFEf$X <- NULL
galaxyRFEf$galaxysentiment<- as.factor(galaxyRFEf$galaxysentiment)

# create the training partition that is 70%/30% split of total obs
#uing oob galaxy data with galaxysentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(galaxyRFEf$galaxysentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- galaxyRFEf[inTraining,]   
testSet <- galaxyRFEf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(galaxyRFEf)

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
system.time(rfFit1 <- train(galaxysentiment~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "galaxyRFEfrfFit1Model.RDS") 
rfFit1 <- readRDS("galaxyRFEfrfFit1Model.RDS")

# Random Forest 
# 
# 9040 samples
# 44 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2    0.7097356  0.3696133
# 23    0.7631653  0.5288836
# 44    0.7558652  0.5192946
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 23.


## ------- KKNN ------- ##

# KKNN train/fit
set.seed(seed)
kknnFit1 <- train(galaxysentiment~., data=trainSet, method="kknn", trControl=fitControl)
beep("fanfare")
kknnFit1
saveRDS(kknnFit1, "galaxyRFEfkknnFit1Model.RDS") 
kknnFit1 <- readRDS("kknnFit1Model.rds")

# k-Nearest Neighbors 
# 
# 9040 samples
# 44 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   kmax  Accuracy   Kappa    
# 5     0.6594034  0.4101261
# 7     0.7340747  0.4895537
# 9     0.7212521  0.4789588
# 
# Tuning parameter 'distance' was held constant at a value of 2
# Tuning parameter 'kernel'
# was held constant at a value of optimal
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were kmax = 7, distance = 2 and kernel = optimal.

## ------- C5.0 ------- ##
# C5.0 train/fit

set.seed(seed)
c50Fit1 <- train(galaxysentiment~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "galaxyRFEfC50Fit1Model.RDS")
c50Fit1 <- readRDS("galaxyRFEfC50Fit1Model.RDS")

# C5.0 
# 
# 9040 samples
# 44 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.7652672  0.5293419
# rules  FALSE   10      0.7538730  0.5071282
# rules  FALSE   20      0.7538730  0.5071282
# rules   TRUE    1      0.7659311  0.5301317
# rules   TRUE   10      0.7536539  0.5045522
# rules   TRUE   20      0.7536539  0.5045522
# tree   FALSE    1      0.7628352  0.5248674
# tree   FALSE   10      0.7567509  0.5144032
# tree   FALSE   20      0.7567509  0.5144032
# tree    TRUE    1      0.7639410  0.5265565
# tree    TRUE   10      0.7542051  0.5087959
# tree    TRUE   20      0.7542051  0.5087959
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow = TRUE.

## ------- SVM ------- ##

# SVM train/fit
set.seed(seed)
svmFit1 <- train(galaxysentiment~., data=trainSet, method="svmLinear2", trControl=fitControl)
beep("fanfare")
svmFit1
saveRDS(svmFit1, "galaxyRFEfsvmFit1Model.RDS")
svmFit1 <- readRDS("galaxyRFEfsvmFit1Model.RDS")

# Support Vector Machines with Linear Kernel 
# 
# 9040 samples
# 44 predictor
# 6 classes: '0', '1', '2', '3', '4', '5' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8136, 8136, 8136, 8135, 8137, ... 
# Resampling results across tuning parameters:
#   
#   cost  Accuracy   Kappa    
# 0.25  0.6994460  0.3589993
# 0.50  0.7056403  0.3779398
# 1.00  0.7037605  0.3796365
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
# rf   0.7522124 0.7597449 0.7657811 0.7631653 0.7673579 0.7710177    0
# kknn 0.6983425 0.7272738 0.7362898 0.7340747 0.7430863 0.7524862    0
# c50  0.7580110 0.7602210 0.7646724 0.7659311 0.7706774 0.7787611    0
# svm  0.6951220 0.7000000 0.7079642 0.7056403 0.7107301 0.7146018    0
# 
# Kappa 
#           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf   0.5051868 0.5219044 0.5304557 0.5288836 0.5379785 0.5516271    0
# kknn 0.4363403 0.4771111 0.4900033 0.4895537 0.5118801 0.5172740    0
# c50  0.5119025 0.5161435 0.5251555 0.5301317 0.5443783 0.5563604    0
# svm  0.3435350 0.3681109 0.3815762 0.3779398 0.3937206 0.3988820    0


##--- Conclusion ---##
# Make a note of which model is the top model, and why

# C50 has the highest Kappa and Accuracy of all the models
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
postResample(rfPred1, testSet$galaxysentiment)
# 
# Accuracy     Kappa 
# 0.7677603 0.5370683 

# print predictions
rfPred1

#########################################################

# make predictions with kknn Model #######################

kknnPred1 <- predict(kknnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(kknnPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7326272 0.4888840 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.7685353 0.5344422 


# print predictions
c50Pred1

#########################################################

# make predictions with svm Model
svmPred1 <- predict(svmFit1, testSet)
beep("fanfare")
# performace measurment
postResample(svmPred1, testSet$galaxysentiment)
# Accuracy     Kappa 
# 0.6987858 0.3687871 

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

cmRF <-confusionMatrix(rfPred1, testSet$galaxysentiment)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for kknn

cmKKNN <-confusionMatrix(kknnPred1, testSet$galaxysentiment)
print(cmKKNN)

as.table(cmKKNN)
as.matrix(cmKKNN,what="overall")
as.matrix(cmKKNN, what = "classes")
write.csv(as.table(cmKKNN), file = "cmKKNN.csv")
write.csv(as.matrix(cmKKNN,what="overall"), file = "cmKKNN_overall.csv")
write.csv(as.matrix(cmKKNN, what = "classes"), file = "cmKKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$galaxysentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Confusion matrix for svm

cmSVM <-confusionMatrix(svmPred1, testSet$galaxysentiment)
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
# 0.7678          0.5371                  
#kknn
# Accuracy     Kappa 
# 0.7326         0.4889                             
#C5.0
# Accuracy     Kappa 
# 0.7685           0.5344                                        
#SVM
# Accuracy     Kappa 
# 0.6988       0.3688                            

####################
#PLOT Performance metrics

plotData <- c(0.7678,  0.5371, 0.7326, 0.4889, 0.7685, 0.5344, 0.6988, 0.3688) #rfA, rfK, kknnA, kknnK, c50A, c50K, svmA, svmK

data <- structure(list(W= c(0.7678,  0.5371), X = c(0.7326, 0.4889), 
                       Y = c(0.7685, 0.5344), Z = c(0.6988, 0.3688)), .Names = c("RF", "KKNN", "C5.0", "SVM"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics RFE \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KKNN', 'C5.0', 'SVM')
y <- c(0.7678, 0.7326, 0.7685,0.6988)
y2 <- c(0.5371,0.4889,0.5344,0.3688)
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

# C5.0 with galaxydfoobf dataset
#Accuracy     Kappa 
# 0.768     0.5324

#C5.0 with galaxyNZVf dataset
# Accuracy     Kappa 
# 0.768          0.5324   

# C50 with galaxyRFEf dataset
#Accuracy     Kappa 
# 0.7685          0.5344 

#C50 with galaxyRFEf data set had best accuracy and kappa


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

galaxydfoobf <- read.csv("galaxydfoobf.csv", header = TRUE, sep = ",")
galaxydfoobf$X <- NULL
galaxydfoobf$galaxysentiment <- as.factor(galaxydfoobf$galaxysentiment)
# create a new dataset that will be used for recoding sentiment
galaxyoobfRC <- galaxydfoobf
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
galaxyoobfRC$galaxysentiment <- recode(galaxyoobfRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(galaxyoobfRC)
str(galaxyoobfRC)
# make galaxysentiment a factor
galaxyoobfRC$galaxysentiment <- as.factor(galaxyoobfRC$galaxysentiment)
write.csv(galaxyoobfRC, file = "galaxyoobfRC.csv")

##### now apply the best model so far with this new data set

# create the training partition that is 70%/30% split of total obs
#uing oob galaxy data with galaxysentiment set to a factor
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(galaxyoobfRC$galaxysentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- galaxyoobfRC[inTraining,]   
testSet <- galaxyoobfRC[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(galaxyoobfRC)

################
# Train control
################

# set 10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


##############
# Train model
##############

## ------- C50------- ##
c50Fit1 <- readRDS("galaxyRFEfC50Fit1Model.RDS")

# RF train/fit
set.seed(seed)
str(trainSet)
system.time(c50Fit1 <- train(galaxysentiment~., data=trainSet, method="C5.0", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "galaxyoobfRCFit1Model.RDS") 
c50Fit1 <- readRDS("galaxyoobfRCFit1Model.RDS")

# C5.0 
# 
# 9039 samples
# 58 predictor
# 4 classes: '1', '2', '3', '4' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8135, 8135, 8135, 8135, 8135, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.8425723  0.5900179
# rules  FALSE   10      0.8307357  0.5566179
# rules  FALSE   20      0.8307357  0.5566179
# rules   TRUE    1      0.8420190  0.5877591
# rules   TRUE   10      0.8333884  0.5645396
# rules   TRUE   20      0.8333884  0.5645396
# tree   FALSE    1      0.8415761  0.5887482
# tree   FALSE   10      0.8349390  0.5724292
# tree   FALSE   20      0.8349390  0.5724292
# tree    TRUE    1      0.8419078  0.5891607
# tree    TRUE   10      0.8331667  0.5694068
# tree    TRUE   20      0.8331667  0.5694068
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow = FALSE.

writeOutput.F("c50Fit1", c50Fit1)
closeAllConnections() #close all connections and restore all sink di

########################
# Validate top model
########################


# make predictions with c50 Model #######################

c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$galaxysentiment)
# 
# Accuracy     Kappa 
# 0.8473657 0.6038792

# print predictions
c50Pred1

writeOutput.F("c50Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di
write.csv(c50Pred1, file = "c50Pred1Results.csv", sep = ",")

#These results are FAR better
# confusion matrix for rf model with galaxyoobfRC data set

cmC50 <-confusionMatrix(c50Pred1, testSet$galaxysentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")


########################
# Principle Component Analysis
########################

galaxydfoobf <- read.csv(file = "galaxydfoobf.csv", header = TRUE, sep = ",")
galaxydfoobf$X <- NULL
galaxydfoobf$galaxysentiment <- as.factor(galaxydfoobf$galaxysentiment)
set.seed(seed) # set random seed
#use non-factor dependant for partion creation
inTraining <- createDataPartition(galaxydfoobf$galaxysentiment, p=.70, list=FALSE)
# create training/testing dataset
trainSet <- galaxydfoobf[inTraining,]   
testSet <- galaxydfoobf[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(galaxydfoobf)


# data = training and testing from galaxyDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(trainSet[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, trainSet[,-59])

# add the dependent to training
train.pca$galaxysentiment <- trainSet$galaxysentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testSet[,-59])

# add the dependent to training
test.pca$galaxysentiment <- testSet$galaxysentiment

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

## ------- C50------- ##

# RF train/fit
set.seed(seed)
system.time(c50Fit1 <- train(galaxysentiment~., data=train.pca, method="c50", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "galaxyoobfPCAFit1Model.RDS") 
rfFit1 <- readRDS("galaxyoobfPCAFit1Model.RDS")

# C5.0 
# 
# 9039 samples
# 58 predictor
# 4 classes: '1', '2', '3', '4' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 8135, 8135, 8135, 8135, 8135, 8135, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.8425723  0.5900179
# rules  FALSE   10      0.8307357  0.5566179
# rules  FALSE   20      0.8307357  0.5566179
# rules   TRUE    1      0.8420190  0.5877591
# rules   TRUE   10      0.8333884  0.5645396
# rules   TRUE   20      0.8333884  0.5645396
# tree   FALSE    1      0.8415761  0.5887482
# tree   FALSE   10      0.8349390  0.5724292
# tree   FALSE   20      0.8349390  0.5724292
# tree    TRUE    1      0.8419078  0.5891607
# tree    TRUE   10      0.8331667  0.5694068
# tree    TRUE   20      0.8331667  0.5694068
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 1, model = rules and winnow = FALSE.

writeOutput.F("c50Fit1", c50Fit1)
closeAllConnections() #close all connections and restore all sink di

########################
# Validate top model
########################


# make predictions with c50 Model #######################

c50Pred1 <- predict(c50Fit1, test.pca)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$galaxysentiment)
# 
# Accuracy        Kappa 
# 0.1007491604 0.0009422687 

# print predictions
c50Pred1

writeOutput.F("c50Pred1 Results", c50Pred1) 
closeAllConnections() #close all connections and restore all sink di
write.csv(c50Pred1, file = "c50Pred1Results.csv", sep = ",")

#These results are poorer than oob
# confusion matrix for rf model with galaxyoobfRC data set

cmC50 <-confusionMatrix(c50Pred1, test.pca$galaxysentiment)
print(cmC50)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

########################
# Predict with top model
########################

#c50 oob with feature engineering is the best performing model
#repeat the feature engineering steps on the largematrix file

galaxydfoobf <- read.csv("LargeMatrix.csv", header = TRUE, sep = ",")
str(galaxydfoobf)
galaxydfoobf$X <- NULL
galaxydfoobf$galaxysentiment <- as.factor(galaxydfoobf$galaxysentiment)
# create a new dataset that will be used for recoding sentiment
summary(galaxydfoobf)
galaxyoobfRC <- galaxydfoobf
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
galaxyoobfRC$galaxysentiment <- recode(galaxyoobfRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(galaxyoobfRC)
str(galaxyoobfRC)
# make galaxysentiment a factor
galaxyoobfRC$galaxysentiment <- as.factor(galaxyoobfRC$galaxysentiment)

write.csv(galaxyoobfRC, file = "galaxyoobfRCLargeMatrix.csv")

c50Fit1 <- readRDS("galaxyoobfRCFit1Model.RDS")
# make predictions
str(galaxyoobfRC)
finalPred <- predict(c50Fit1, galaxyoobfRC)
finalPred
write.csv(finalPred, file = "galaxyoobfRCLargeMatrixRFFinalPred.csv")

summary(finalPred)

# 1     2     3     4 
# 13178  2691  1969 17389 

# create a data frame for plotting.
# you can add more sentiment levels if needed
# Replace sentiment values 
pieData <- data.frame(COM = c("negative", "somewhat negative", "somewhat positive",                        "positive"), 
                      values = c(13178 , 2691  ,1969, 17389 ))

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
  layout(title = 'galaxy Sentiment', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

totals

