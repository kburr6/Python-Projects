install.packages(readr)

library(“readr”)

IrisDataset <- read.csv(iris.csv)

attributes(IrisDataset)

summary(risDataset) 

str(IrisDatasets)

names(IrisDataset)

hist(IrisDataset$Species)

plot(IrisDataset$Sepal.Length
     
     qqnorm(IrisDataset)
     
     IrisDataset$Species<- as.numeric(IrisDataset$Species) 
     
     set.seed(123)
     
     trainSize <- round(nrow(IrisDataset) * 0.2)
     
     testSize <- nrow(IrisDataset) - trainSet
     
     trainSizes
     
     testSize
     
     trainSet <- IrisDataset[training_indices, ]
     
     testSet <- IrisDataset[-training_indices, ]
     
     set.seed(405)
     
     trainSet <- IrisDataset[training_indices, ]
     
     testSet <- IrisDataset[-training_indices, ]
     
     LinearModel<- lm(trainSet$Petal.Width ~ testingSet$Petal.Length)
     
     summary(LinearModel)
     
     prediction<-predict(LinearModeltestSet)
     
     predictions