# Title: C3T3 Pipeline

# Last update: 01/06/2020

# File/project name: C3T3 Pipeline.R
# RStudio Project name: Task 3 - Evaluate Techniques for WiFi Locationing.Rproj

###############
# Project Notes
###############

# Now it's time to begin a new project for a new client. Our client is 
# developing a system to be deployed on large industrial campuses, in 
# shopping malls, et cetera to help people to navigate a complex, 
# unfamiliar interior space without getting lost. While GPS works fairly 
# reliably outdoors, it generally doesn't work indoors, so a different 
# technology is necessary. Our client would like us to investigate the 
# feasibility of using "wifi fingerprinting" to determine a person's 
# location in indoor spaces. Wifi fingerprinting uses the signals from 
# multiple wifi hotspots within the building to determine location, 
# analogously to how GPS uses satellite signals. We have been provided 
# with a large database of wifi fingerprints for a multi-building 
# industrial campus with a location (building, floor, and location ID) 
# associated with each fingerprint. Your job is to evaluate multiple 
# machine learning models to see which produces the best result, 
# enabling us to make a recommendation to the client. If your 
# recommended model is sufficiently accurate, it will be incorporated 
# into a smartphone app for indoor locationing.
# 
# This is a deceptively difficult problem to solve. I'm looking forward 
# to seeing what you come up with. After completing your analyses, 
# please prepare an internal report on the project for IOT Analytics; 
# because this initial report will be delivered to an internal audience, 
# it can contain more technical detail than the report we will eventually 
# present to the client. I have attached some sample data for you to use 
# for your analysis.

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


detectCores()  # detect number of cores
cl <- makeCluster(5)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
#stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ############################

# --- Load Train/Existing data (Dataset 1) --- ############
trainingData <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
class(trainingData)  # "data.frame"
str(trainingData)   #19937 obs. of  529 variables


# --- Load Predict/New data (Dataset 2) --- ###############

validationData <- read.csv("validationData.csv", stringsAsFactors = FALSE)
class(validationData)  # "data.frame"
str(validationData)    #1111 obs. of  529 variables


################
# Evaluate data
################

#--- Dataset 1 - trainingData ---##########################################

#520 WAP columns, then FLOOR, BUILDINGID, SPACEID

names(trainingData)  #WAP001 - WAP520, FLOOR, BUILDINGID, SPACEID
summary(trainingData$SPACEID)#complete data set
head(trainingData)   #complete record including spaceid and relativeposition
tail(trainingData)   #no anomoly for head or tail

# plot - ANY INTERESTING PLOTS FOR OOB DATA??
#hist(existingProduct$x5StarReviews)
#plot(existingProduct$ProductNum, existingProduct$Volume)
#qqnorm(existingProduct$Price)

# check for missing values 
anyNA(trainingData)  # FALSE
is.na(trainingData)  # All FALSE


#--- Dataset 2 - validationData ---##########################################


names(validationData)  #WAP001 - WAP520, FLOOR, BUILDINGID, SPACEID
summary(validationData) #note WAP int range -100 - 0 for a signal, 100 == no signal
summary(validationData$SPACEID)#NO VALUES FOR SPACEID IN VALIDATION DATA SET
                               #might want to consider using lat/long
head(validationData)   #complete except sapceid and relativeposition are indeed 0
tail(validationData)   #complete except sapceid and relativeposition are indeed 0

# plot - ANY INTERESTING PLOTS FOR OOB DATA??
#hist(existingProduct$x5StarReviews)
#plot(existingProduct$ProductNum, existingProduct$Volume)
#qqnorm(existingProduct$Price)

# check for missing values 
anyNA(validationData)  # FALSE
is.na(validationData)  # All FALSE


#############
# Preprocess
#############

#--- Dataset 1 - trainingData ---#

#don't need to normalize the data since the data is all the same scale
#don't have categoricval predictor nor predicted valuez that need to 
#be converted to binary (0,1) with the dummyVars function

#drop unneeded columns

trainingData$TIMESTAMP <- NULL
trainingData$USERID <- NULL
trainingData$PHONEID <- NULL
trainingData$RELATIVEPOSITION <- NULL
trainingData$LATITUDE <- NULL
trainingData$LONGITUDE <- NULL

head(trainingData)

#DO NEED though to engineer a predicted feature (target) which
#is a composite of building/floor/space - might need to use
#the dummyVar function on this now that I'm thinking about it

#Need to ENGINEER a predicted value by concatenating BUILDING, FLOOR, and SPACEID

trainingDataWCombined = trainingData %>% unite("label_location", FLOOR:SPACEID, na.rm = TRUE, remove = FALSE)
summary(trainingDataWCombined)
summary(trainingDataWCombined$label_location)

#drop buildingid, spaceid, and floor since i have a combined attribute now

trainingDataWCombined$BUILDINGID <- NULL
trainingDataWCombined$SPACEID <- NULL
trainingDataWCombined$FLOOR <- NULL

#convert label_location to a factor attribute type - which is now a number corresponding to level

trainingDataWCombinedFactorLabel$label_location <- as.factor(trainingDataWCombined$label_location)
str(trainingDataWCombinedFactorLabel$label_location)

#convert back to character strings for label location if needed
fact_character <- levels(trainingDataWCombined$label_location)[as.numeric(trainingDataWCombined$label_location)]
fact_character
str(fact_character)

#DO I NEED TO DUMIFY THIS FACTOR ATTRIBUTE?? WILL KNOW WHEN I RUN ALGOS

#trainingDataWCombined$label_location <- as.integer(trainingDataWCombined$label_location)

#dumify the data - convert categorical predictors to (0,1)
#newDataFrame1 <- dummyVars(" ~ .", data = existingProduct)
#readyData_existingProduct <- data.frame(predict(newDataFrame1, newdata = existingProduct))
#View(readyData_existingProduct)

#str(readyData_existingProduct)

# change data types
#DatasetName$ColumnName <- as.typeofdata(DatasetName$ColumnName)

#for corr to work the dataframe have to be of type numeric
#so converting all columns to numeric in a new variable
#df[] <- lapply(df, as.numeric)

trainingDataWCombinedNumeric <- trainingDataWCombined
trainingDataWCombinedNumeric[] <- lapply(trainingDataWCombined, as.numeric)
str(trainingDataWCombinedNumeric)
str(trainingDataWCombinedNumeric$label_location)

# rename a column
#names(DatasetName)<-c("ColumnName","ColumnName","ColumnName") 


# handle missing values 
#readyData_existingProduct$BestSellersRank <- NULL #delete attribute BestSellersRank due to NA's
#na.omit(DatasetName$ColumnName)
#na.exclude(DatasetName$ColumnName)        
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)] <- mean(DatasetName$ColumnName,na.rm = TRUE)

#create a smaller sample of records from the training set
trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric[sample(1:nrow(trainingDataWCombinedNumeric), round(nrow(trainingDataWCombinedNumeric)*.1), replace=FALSE),]

#Review correlation matrix  -  are there correlations between attributes (not predicted/label attribute)
trainingDataWCombinedNumeric_sample <- trainingDataWCombinedNumeric_sample[complete.cases(trainingDataWCombinedNumeric_sample), ]
#corrData <- cor(trainingDataWCombinedNumeric_sample[sapply(trainingDataWCombinedNumeric_sample, is.numeric)], use='pairwise')
corrData <- cor(trainingDataWCombinedNumeric_sample, use = "pairwise.complete.obs")
#corrplot(corrData)
corrData
write.csv(corrData, file = "corrData.csv")
volCorrData <- corrData[,"label_location", drop=FALSE]
write.csv(volCorrData, file = "volCorrData.csv")


# save preprocessed dataset
write.csv(trainingDataWCombinedNumeric_sample, file = "trainingDataWCombinedNumeric_sample.csv")
write.csv(trainingDataWCombinedNumeric, file = "trainingDataWCombinedNumeric.csv")
write.csv(trainingDataWCombined, file = "trainingDataWCombined.csv")


#--- Dataset 2 - validationData ---#

#don't need to normalize the data since the data is all the same scale
#don't have categoricval predictor nor predicted valuez that need to 
#be converted to binary (0,1) with the dummyVars function

#drop unneeded columns

validationData$TIMESTAMP <- NULL
validationData$USERID <- NULL
validationData$PHONEID <- NULL
validationData$RELATIVEPOSITION <- NULL
validationData$LATITUDE <- NULL
validationData$LONGITUDE <- NULL

head(validationData)

#DO NEED though to engineer a predicted feature (target) which
#is a composite of building/floor/space - might need to use
#the dummyVar function on this now that I'm thinking about it

#Need to ENGINEER a predicted value by concatenating BUILDING, FLOOR, and SPACEID

validationDataWCombined = validationData %>% unite("label_location", FLOOR:SPACEID, na.rm = TRUE, remove = FALSE)
summary(validationDataWCombined)
summary(validationDataWCombined$label_location)

#drop buildingid, spaceid, and floor since i have a combined attribute now

validationDataWCombined$BUILDINGID <- NULL
validationDataWCombined$SPACEID <- NULL
validationDataWCombined$FLOOR <- NULL

write.csv(validationDataWCombined, file = "validationDataWCombined.csv")


# ################
# # Sampling - for training
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
setwd("C:\\Users\\kburr\\Desktop\\UT Data Analytics Program\\Course 3 - Data Analytics and Visualization\\Task 3 - Evaluate Techniques for WiFi Locationing")
getwd()
trainingDataWCombined <- read.csv("trainingDataWCombined.csv")
validationDataWCombined<- read.csv("validationDataWCombined.csv")
trainingDataWCombined$label_location <- as.character(trainingDataWCombined$label_location)

# create the training partition that is 75% of total obs
set.seed(seed) # set random seed
inTraining <- createDataPartition(trainingDataWCombined$label_location, p=.75, list=FALSE)
# create training/testing dataset
trainSet <- trainingDataWCombined[inTraining,]   
testSet <- trainingDataWCombined[-inTraining,]   
# verify number of obs 
nrow(trainSet)  
nrow(testSet)   
nrow(trainingDataWCombined)

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
system.time(rfFit1 <- train(label_location~., data=trainSet, method="rf", importance=T, trControl=fitControl)) #importance is needed for varImp
beep("fanfare")
rfFit1
saveRDS(rfFit1, "rfFit1Model.RDS") 
rfFit1 <- readRDS("rfFit1Model.rds")


## ------- KNN ------- ##

# KNN train/fit
set.seed(seed)
knnFit1 <- train(label_location~., data=trainSet, method="knn", trControl=fitControl)
beep("fanfare")
knnFit1
saveRDS(knnFit1, "knnFit1Model.RDS") 
knnFit1 <- readRDS("knnFit1Model.rds")

## ------- C5.0 ------- ##



# C5.0 train/fit
set.seed(seed)
c50Fit1 <- train(label_location~., data=trainSet, method="C5.0", trControl=fitControl)
beep("fanfare")
c50Fit1
saveRDS(c50Fit1, "C50Fit1Model.RDS")
c50Fit1 <- readRDS("C50Fit1Model.rds")

#################
# Evaluate models
#################

##--- Compare models ---##

# use resamples to compare model performance
ModelFitResults <- resamples(list(rf=rfFit1, knn=knnFit1, c50=c50Fit1))
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
# Models: rf, knn, c50 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5629820 0.5693431 0.5835351 0.5895261 0.6019656 0.6408978    1
# knn 0.4393838 0.4414393 0.4460836 0.4492757 0.4503600 0.4743421    0
# c50 0.7389558 0.7425527 0.7500657 0.7500051 0.7544644 0.7645140    0
# 
# Kappa 
#          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# rf  0.5618586 0.5683567 0.5825218 0.5885449 0.6010432 0.6400045    1
# knn 0.4383265 0.4403812 0.4450335 0.4482233 0.4493047 0.4733213    0
# c50 0.7384593 0.7420736 0.7495894 0.7495327 0.7539961 0.7640689    0   0
# 



##--- Conclusion ---##
# Make a note of which model is the top model, and why

#C5.0 has the highest Kappa and Accuracy of all the models


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
writeOutput.F("knnFit1", knnFit1) 
closeAllConnections() #close all connections and restore all sink di
writeOutput.F("c50Fit1", c50Fit1) 
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
postResample(rfPred1, testSet$label_location)
# 
# Accuracy     Kappa 
# 0.7136904 0.7131156 

# print predictions
rfPred1

#########################################################

# make predictions with knn Model #######################

knnPred1 <- predict(knnFit1, testSet)
beep("fanfare")
# performace measurment
postResample(knnPred1, testSet$label_location)
# Accuracy     Kappa 
# 0.4694728 0.4684432 

# print predictions
knnPred1

#########################################################

# make predictions with c5.0 Model
c50Pred1 <- predict(c50Fit1, testSet)
beep("fanfare")
# performace measurment
postResample(c50Pred1, testSet$label_location)
# Accuracy     Kappa 
# 0.7605751 0.7601242 


# print predictions
c50Pred1

########################
# Create confusion Matrixes
########################

#Confusion matrix for random forest

cmRF <-confusionMatrix(rfPred1, testSet$label_location)
print(cmRF)

as.table(cmRF)
as.matrix(cmRF,what="overall")
as.matrix(cmRF, what = "classes")
write.csv(as.table(cmRF), file = "cmRF.csv")
write.csv(as.matrix(cmRF,what="overall"), file = "cmRF_overall.csv")
write.csv(as.matrix(cmRF, what = "classes"), file = "cmRF_classes.csv")

#Confusion matrix for knn

cmKNN <-confusionMatrix(knnPred1, testSet$label_location)
print(cmKNN)

as.table(cmKNN)
as.matrix(cmKNN,what="overall")
as.matrix(cmKNN, what = "classes")
write.csv(as.table(cmKNN), file = "cmKNN.csv")
write.csv(as.matrix(cmKNN,what="overall"), file = "cmKNN_overall.csv")
write.csv(as.matrix(cmKNN, what = "classes"), file = "cmKNN_classes.csv")

#Confusion matrix for C5.0

cmC50 <-confusionMatrix(c50Pred1, testSet$label_location)
print(cmKNN)

as.table(cmC50)
as.matrix(cmC50,what="overall")
as.matrix(cmC50, what = "classes")
write.csv(as.table(cmC50), file = "cmC50.csv")
write.csv(as.matrix(cmC50,what="overall"), file = "cmC50_overall.csv")
write.csv(as.matrix(cmC50, what = "classes"), file = "cmC50_classes.csv")

#Summary of Performance Statistics from confusion matrix results
# RF
#Accuracy     Kappa 
# 0.713690352 0.713115634

#knn
# Accuracy     Kappa 
# 0.469472807 0.468443225

#C5.0
# Accuracy     Kappa 
# 0.760575120 0.760124186 

####################
#PLOT Performance metrics

plotData <- c(0.713690352,  0.713115634, 0.469472807, 0.468443225, 0.760575120, 0.760124186) #rfA, rfK, knnA, knnK, c50A, c50K

data <- structure(list(W= c(0.713690352, 0.713115634), X = c(0.469472807, 0.468443225), 
                      Y = c(0.760575120, 0.760124186)), .Names = c("RF", "KNN", "C5.0"),  class = "data.frame", row.names = c(NA, -2L))
attach(data)
print(data)
colours <- c("green", "yellow")

bp <- barplot(as.matrix(data), main="Performance Metrics \n Green = Accuracy, Blue = Kappa", ylab = "Values", ylim = c(0.0, 1.0), cex.lab = 1.0, cex.main = 1.0, beside=TRUE, col=c("lightblue", "lavender"))

x <- c('RF', 'KNN', 'C5.0')
y <- c(0.713690352, 0.469472807, 0.760575120)
y2 <- c(0.713115634,0.468443225,0.760124186)
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

########################
# Predict with top model
########################

#C5.0 is the best performing model

# make predictions
finalPred <- predict(c50Fit1, validationDataWCombined)
finalPred


########################
# Save validated model
########################

##--- Save top performing model ---##

# save top performing model after it has been validated

# save model 
saveRDS(c50Fit1, "BestPerformingModelC50.RDS")  # Q: What type of object does saveRDS create?

# load and name model to make predictions with new data
#RFfit1 <- readRDS() # Q: What type of object does readRDS create?

output <- read.csv("validationData.csv", stringsAsFactors = FALSE)
output$Predictions <- finalPred
write.csv(output, file="C3.T3output.csv", row.names = TRUE)

