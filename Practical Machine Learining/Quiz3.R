#Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

inTrain = createDataPartition(segmentationOriginal$Case, p = 3/4, list = F)
training = segmentationOriginal[ inTrain,]
testing  = segmentationOriginal[-inTrain,]
set.seed(125)

modFit <- train(Class ~ ., data = training, method = "rpart")
modFit$finalModel

library(rattle)
fancyRpartPlot(modFit$finalModel)
fancyRpartPlot(modFit)

#Q2
The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size. 

#Q3
library(pgmm)
data(olive)
olive = olive[,-1]

library(randomForest)
model <- train(Area ~ ., data = olive, method = "rpart2")

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata = newdata)

#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
model = train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
              data = trainSA, method = 'glm', family = 'binomial' )

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != 
                                              values)/length(values)}

missClass(trainSA$chd, predict(model, newdata = trainSA))
missClass(testSA$chd, predict(model, newdata = testSA))


#Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
# Fit a random forest predictor relating the factor variable y to the remaining variables.
a <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
b <- varImp(a)
order(b)
