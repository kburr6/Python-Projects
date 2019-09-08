install.packages("readr") #added "" around readr

library(readr) #removed "" around readr

IrisDataset <- read.csv("iris.csv") #added "" around filename

attributes(IrisDataset) #ok

summary(IrisDataset) # added I to risDataset mispelling 

str(IrisDataset) #removed s at end of IrisDataset

names(IrisDataset)#ok

NumericSpecies<-as.numeric(IrisDataset$Species) #have to convert Species which is factor to numeric to utilize hist
hist(NumericSpecies)
        
plot(IrisDataset$Sepal.Length) #added closing )
     
     qqnorm(IrisDataset$Sepal.Length) #can't use the whole dataset - have to plat one feature
     
     IrisDataset$Species<- as.numeric(IrisDataset$Species) #ok
     
     set.seed(123)#ok
     
     trainSize <- round(nrow(IrisDataset) * 0.7) #change % to .7 for training data set size
     
     testSize <- nrow(IrisDataset) - trainSize #change trainSet to trainsize to calculate test data set size
     
     trainSize # delete s misspelling
     
     testSize #ok
     
     training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize) #need to define the training set indices
     
     trainSet <- IrisDataset[training_indices, ] #ok
     
     testSet <- IrisDataset[-training_indices, ] #ok
     
     set.seed(405) # ok - no real reason to do this here though
     
     trainSet <- IrisDataset[training_indices, ] #ok
     
     testSet <- IrisDataset[-training_indices, ] #ok
     
     LinearModel<- lm(Petal.Length~ Petal.Width, trainSet) #Label is Length predicted by Width on trainSet, also don't use dataset name IrisDataset$Petal.Width
     
     summary(LinearModel) #ok
     
     d = data.frame(testSet, predict(LinearModel, testSet, interval="prediction")) #put the prediction in a dataframe so can output easier
     #prediction<-predict(LinearModel,testSet)#added comma in predict argument list
     
     df <- d[ -c(1) ]
     df
     
     write.csv(df, 'predictions.csv')
     