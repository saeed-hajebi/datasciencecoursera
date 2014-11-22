#Q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

set.seed(33833)
model1 = train(y ~ ., data = vowel.train, method = 'rf',
               trControl = trainControl(method="cv"), number=4)
model2 = train(y ~ ., data = vowel.train, method = 'gbm', verbose = F)

pred1 = predict(model1, newdata = vowel.test)
pred2 = predict(model2, newdata = vowel.test)

comb = data.frame(pred1, pred2, y = vowel.test$y)
model3 = train(y ~ ., data = comb, method = 'gam')
pred3 = predict(model3, newdata = comb)

c1 = confusionMatrix(pred1, vowel.test$y)
c2 = confusionMatrix(pred2, vowel.test$y)
c3 = confusionMatrix(pred3, vowel.test$y)


#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

fit1 <- train(diagnosis ~ ., data = training, method = "rf", trControl = trainControl(number = 4))
fit2 <- train(diagnosis ~ ., data = training, method = "gbm", verbose = F)
fit3 <- train(diagnosis ~ ., data = training, method = "lda")

# predict test
predict1 <- predict(fit1, newdata = testing)
predict2 <- predict(fit2, newdata = testing)
predict3 <- predict(fit3, newdata = testing)

DF_combined <- data.frame(predict1, predict2, predict3, diagnosis = testing$diagnosis) # training$diagnosis?
fit_combined <- train(diagnosis ~ ., data = DF_combined, method = "rf", trControl = trainControl(number = 4))
predict4 <- predict(fit_combined, newdata = testing)
# confusion matrixes
c1 <- confusionMatrix(predict1, testing$diagnosis)
c2 <- confusionMatrix(predict2, testing$diagnosis)
c3 <- confusionMatrix(predict3, testing$diagnosis)
c4 <- confusionMatrix(predict4, testing$diagnosis)

print(paste(c1$overall[1], c2$overall[1], c3$overall[1], c4$overall[1]))



#Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
# Set the seed to 233 and fit a lasso model to predict Compressive Strength.
# Which variable is the last coefficient to be set to zero as the penalty increases?
# (Hint: it may be useful to look up ?plot.enet).
library(caret)
set.seed(233)
fit = train(CompressiveStrength ~ ., data = training, method = "lasso")
# Since we are interested in the shrinkage of coefficients as the penalty(lambda) increases, "
# penalty" looks promising for an xvar argument value.
plot.enet(fit$finalModel, xvar = "penalty", use.color = TRUE)


#Q4
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)
library(quantmod)

fit <- bats(tstrain)

h <- dim(testing)[1]

fcast <- forecast(fit, level = 95, h = h)

accuracy(fcast, testing$visitsTumblr)

result <- c()
l <- length(fcast$lower)
for (i in 1:l){
  x <- testing$visitsTumblr[i]
  a <- fcast$lower[i] < x & x < fcast$upper[i]
  result <- c(result, a)
}
sum(result)/l * 100


#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
library(caret)
fit = train(CompressiveStrength ~ ., data = training, method = "svmRadial")
prediction = predict(fit, testing)
accuracy(prediction, testing$CompressiveStrength)
