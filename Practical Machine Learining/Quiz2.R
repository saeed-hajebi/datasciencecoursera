#Q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


#Q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing  = mixtures[-inTrain,]

featurePlot(x = training[,c('Cement',
                            'BlastFurnaceSlag',
                            'FlyAsh',
                            'Water',
                            'Superplasticizer',
                            'CoarseAggregate',
                            'FineAggregate', 'Age')],
            y = training$CompressiveStrength )


library(Hmisc)
splitOn <- cut2(training$Age, g = 4)
library(plyr)
splitOn <- mapvalues(splitOn,
                     from = levels(factor(splitOn)),
                     to = c("red", "blue", "yellow", "green"))
# automatically includes index of samples
plot(training$CompressiveStrength, col = splitOn)



#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing  = mixtures[-inTrain,]

qplot(log10(training$Superplasticizer))
#ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()



#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_variables = grep("^IL", names(training), value = TRUE)
preProc <- preProcess(training[, IL_variables], method = "pca", thresh = 0.8)
preProc



#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4, list = F)
training = adData[ inTrain,]
testing  = adData[-inTrain,]

## train the data using the first method
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

model1 = train(diagnosis ~ ., method = "glm", data = training)
C1 <- confusionMatrix(testing$diagnosis, predict(model1, testing))
C1

model2 = train(diagnosis ~ ., method = "glm", data = training, preProcess = "pca", Control = trainControl(preProcOptions = list(thresh = 0.8)))

C2 <- confusionMatrix(testing$diagnosis, predict(model2, testing))
C2
