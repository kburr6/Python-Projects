#caret models
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html#dataframe = WholeYear
#Y Value = SolarRad

# Load library and set seed 

library(caret)
set.seed(998)

#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Linear Regression model
LMFit1 <- train(SolarRad~., data = training, method = "lm", trControl=fitControl)

#check the results
LMFit1