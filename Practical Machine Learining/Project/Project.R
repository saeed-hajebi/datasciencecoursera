setwd('C:/Dropbox/Others/Data Science/datasciencecoursera/Practical Machine Learining/Project')
training = read.csv(file = 'pml-training.csv', sep = ',', header = T)
testing  = read.csv(file = 'pml-testing.csv', sep = ',', header = T) 

#Read data
#trainingURL = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
#testingURL  = 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

#training = read.csv(url(trainingURL), na.strings=c("NA","#DIV/0!",""))
#testing  = read.csv(url(trainingURL), na.strings=c("NA","#DIV/0!",""))

set.seed(184)

library(caret)

inTrain = createDataPartition(training$classe, p = 3/4, list = F)
trainData = training[ inTrain,]
evalData  = training[-inTrain,]


