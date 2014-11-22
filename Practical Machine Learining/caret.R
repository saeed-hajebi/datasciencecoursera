library(caret); 
library(kernlab); 
data(spam);

inTrain = createDataPartition(y = spam$type, p = 0.75, list = FALSE);
training = spam[inTrain,]
testing = spam[-inTrain,]
dim(training)

set.seed(32343);
modelFit = train(type ~., data = training, method = "glm");
modelFit;

modelFit$finalModel;

predictions = predict(modelFit, newdata = testing);
predictions;

confusionMatrix(predictions, testing$type)



# Data Slicng
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE)
sapply(folds,length)

set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)


set.seed(32323)
tme = 1:1000
folds = createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)


library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")



# Ploting
library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

inTrain = createDataPartition(y = Wage$wage, p = 0.7, list = F)
training = Wage[inTrain, ]
testing = Wage[-inTrain, ]
dim(training); dim(testing)

featurePlot(x = training[, c('age', 'education', 'jobclass')],
            y = training$wage,
            plot = 'pairs')


qplot(age, wage, data = training, colour = jobclass)

qq = qplot(age, wage, data = training, colour = education)
qq + geom_smooth(method = 'lm', formula = y~x)

library(Hmisc)
cutWage = cut2(training$wage, g = 3)
table(cutWage)

p1 = qplot(cutWage, age, data = training, fill = cutWage, geom = c('boxplot'))
p1

p2 = qplot(cutWage, age, data = training, fill = cutWage, geom = c('boxplot', 'jitter'))
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)  

t1 = table(cutWage, training$jobclass)
t1

qplot(wage, data = training, colour = education, geom = 'density')



# Preprocessing
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main = '', xlab = 'avg. capital run length')

mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve = training$capitalAve;
trainCapAves = (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(trainCapAves)
sd(trainCapAves)

testCapAve = testing$capitalAve;
testCapAves = (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAves)
sd(testCapAves)

preObj = preProcess(training[, -58], method = c('center','scale'))
trainCapAvs = predict(preObj, training[,-58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)

set.seed(32343)
modelFit = train(type ~ ., data = training, preProcess = c('center', 'scale'), method = 'glm')
modelFit

preObj = preProcess(training[, -58], method = c('BoxCox'))
trainCapAves = predict(preObj, training[,-58])$capitalAve
par(mfrow = c(1,2)); hist(trainCapAves); qqnorm(trainCapAves)

set.seed(13343)
training$capAve = training$capitalAve
selectNA = rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
selectNA
training$capAve[selectNA] = NA

preObj = preProcess(training[, -58], method = 'knnImpute')
capAvg = predict(preObj, training[, -58])$capAve
capAveTruth = training$capitalAve
capAveTruth = (capAveTruth -mean (capAveTruth))/sd(capAveTruth)



# Covariate
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

table(training$jobclass)

dummies = dummyVars(wage ~ jobclass, data = training)

head(predict(dummies, newdata = training))


nzv = nearZeroVar(training, saveMetrics = T)
nzv

library(splines)
bsBasis = bs(training$age, df = 3)
bsBasis

lm1 = lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col = 'red', pch = 19, cex = 0.5)

predict(bsBasis, age = testing$age)



# Preprocessing with principal components analysis
library(caret); library(kernlab); data(spam);
inTrain = createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training = spam[inTrain, ]
testing = spam[-inTrain, ]

M = abs(cor(training[,-58]))
diag(M) = 0
which(M>0.8, arr.ind = T)

names(spam)[c(34,32)]
plot(spam[,34], spam[,32])

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

smallSpam = spam[, c(34,32)]
prComp = prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])

typeColor = (spam$type == 'spam')*1 + 1
prComp = prcomp(log10(spam[, -58]+1))
plot(prComp$x[,1], prComp$x[,2], col = typeColor, xlab = 'PC1', ylab = 'PC2')


preProc = preProcess(log10(spam[,-58]+1), method = 'pca', pcaComp = 2)
spamPC = predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col = typeColor)


preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

testPC = predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

modelFit = train(training$type ~ ., method = 'glm', preProcess = 'pca', data = training)
confusionMatrix(testing$type, predict(modelFit, testing))



# Regression
library(caret); data(fatithful); set.seed(333);
inTrain = createDataPartition(y = faithful$waiting, p = 0.5, list = F)
trainFaith = faithful[inTrain, ]; 
testFaith  = faithful[-inTrain,];
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')

lm1 = lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')
lines(trainFaith$waiting, lm1$fitted, lwd=3)

coefficients(lm1)[1] + coefficients(lm1)[2]*80

newdata = data.frame(waiting = 80)
newdata
predict(lm1, newdata)

par(mfrow = c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')
lines(trainFaith$waiting, predict(lm1), lwd = 3)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, col = 'blue', xlab = 'Waiting', ylab = 'Duration')
lines(testFaith$waiting, predict(lm1, newdata = testFaith), lwd = 3)

SSEtrain = sqrt(sum((lm1$fitted.values - trainFaith$eruptions)^2))
SSEtest = sqrt(sum((predict(lm1, newdata = testFaith) - testFaith$eruptions)^2))

pred1 = predict(lm1,newdata=testFaith,interval='prediction')
ord = order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch = 19, xlab = 'Waiting', ylab = 'Duration')
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)


modelFit = train(eruptions ~ waiting, data = trainFaith, method = 'lm')
summary(modelFit$finalModel)



# Multi variate regression
library(ISLR); library(caret); library(ggplot2);
data(Wage); 
Wage = subset(Wage, select = -c(logwage))
summary(Wage)

inTrain = createDataPartition(y = Wage$wage, p = 0.7, list = F)
training = Wage[inTrain,]; testing = Wage[-inTrain,];
dim(training); dim(testing);

featurePlot(x = training[, c('age', 'education', 'jobclass')], y = training$wage, plot = 'pairs')
qplot(age, wage, data = training, colour = education)

modFit = train(wage ~ age + jobclass + education, method = 'lm', data = training);
finMod = modFit$finalModel
print(modFit)

plot(finMod, 1, pch = 19, cex = 0.5, col = '#00000010')

qplot(finMod$fitted.values, finMod$residuals, colour = race, data = training)

plot(finMod$residuals, pch = 19)

pred = predict(modFit, testing)
qplot(wage, pred, colour = year, data = testing)

modFitAll = train(wage ~ ., data = training, method = 'lm')
pred = predict(modFitAll, testing)
qplot(testing$wage, pred)


# Decision trees
data(iris); library(ggplot2);
names(iris)
table(iris$Species)

inTrain = createDataPartition(y = iris$Species, p = 0.7, list = F)
training = iris[inTrain,]
testing = iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width, Sepal.Width, colour = Species, data = training)

library(caret)
modFit = train(Species ~ ., method = 'rpart', data = training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = T, main = 'Classification Tree')
text(modFit$finalModel, use.n = T, all = T, cex = 0.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata = testing)
confusionMatrix(testing$Species, predict(modFit, newdata = testing))



# Random Forrests
data(iris); library(ggplot2);
inTrain = createDataPartition(y = iris$Species, p = 0.7, list = F)
training = iris[inTrain,]
testing = iris[-inTrain,]

library(caret)
modFit = train(Species ~ ., data = training, method = 'rf', prox = T)
modFit

getTree(modFit$finalModel, k=2)

irisP = classCenter(training[, c(3,4)], training$Species, modFit$finalModel$proximity)
irisP = as.data.frame(irisP);
irisP$Species = rownames(irisP)
p = qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5, shape = 4, data = irisP)

pred = predict(modFit, testing);
testing$predRight = (pred == testing$Species);
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, colour = predRight, data = testing, main = 'newdata predictions')


# Boosting
library(ISLR); library(caret); library(ggplot2);
data(Wage); 
Wage = subset(Wage, select = -c(logwage))
summary(Wage)

inTrain = createDataPartition(y = Wage$wage, p = 0.7, list = F)
training = Wage[inTrain,]; testing = Wage[-inTrain,];

modFit = train(wage ~ ., method = 'gbm', data = training, verbose = F)
print(modFit)

qplot(predict(modFit, newdata = testing), wage, data = testing)



# Model-based predictions
# Random Forrests
data(iris); library(ggplot2);
inTrain = createDataPartition(y = iris$Species, p = 0.7, list = F)
training = iris[inTrain,]
testing = iris[-inTrain,]

library(caret)
modlda = train(Species ~ ., data = training, method = 'lda')
modnb  = train(Species ~ ., data = training, method = 'nb')

plda = predict(modlda, newdata = testing);
pnb  = predict(modnb, newdata = testing); 

table(plda, pnb)

eqPred = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour = eqPred, data = testing)



# Combining predictors
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))

# Create a building data set and validation set
inBuild = createDataPartition(y=Wage$wage, p=0.7, list=FALSE);
validation = Wage[-inBuild,]; 
buildData  = Wage[inBuild,]

inTrain = createDataPartition(y=buildData$wage, p=0.7, list=FALSE);
training = buildData[inTrain,]; 
testing  = buildData[-inTrain,];

mdl1 = train(wage ~ ., method = 'glm', data = training);
mdl2 = train(wage ~ ., method = 'rf' , data = training, 
             trControl = trainControl(method = 'cv'), number = 3);

pred1 = predict(mdl1, testing);
pred2 = predict(mdl2, testing);
qplot(pred1, pred2, colour = wage, data = testing);

predDF = data.frame(pred1, pred2, wage = testing$wage);
combModFit = train(wage ~ ., method = 'gam', data = predDF);
combPred = predict(combModFit, predDF);

err1 = sqrt(sum((pred1-testing$wage)^2))
err2 = sqrt(sum((pred2-testing$wage)^2))
err3 = sqrt(sum((combPred-testing$wage)^2))

pred1V = predict(mdl1, validation);
pred2V = predict(mdl2, validation);
predVDF = data.frame(pred1 = pred1V, pred2 = pred2V)
combPredV = predict(combModFit, predVDF)

err1v = sqrt(sum((pred1V-validation$wage)^2))
err2v = sqrt(sum((pred2V-validation$wage)^2))
err3v = sqrt(sum((combPredV-validation$wage)^2))





# Forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("MSFT", src="google", from = from.dat, to = to.dat)

mGoog <- to.monthly(MSFT)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

plot(decompose(ts1), xlab = 'Years + 1')

ts1Train = window(ts1, start = 1, end = 5)
ts1Test  = window(ts1, start = 5, end = (7-0.01))
ts1Train 

library(forecast)
plot(ts1Train)
lines(ma(ts1Train, order=3), col = 'red')

ets1 = ets(ts1Train, model = 'MMM')
fcast = forecast(ets1)
plot(fcast);
lines(ts1Test, col = 'red')

accuracy(fcast,ts1Test)


# Unsupervised prediction
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

kMeans1 = kmeans(subset(training, select = -c(Species)), centers = 3)
training$clusters = as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data = training)
  
table(kMeans1$cluster, training$Species)

modFit = train(clusters ~ ., data = subset(training, select = -c(Species)), method = 'rpart')
table(predict(modFit, newdata = training), training$Species)

testClustedPred = predict(modFit, testing)
table(testClustedPred, testing$Species)
