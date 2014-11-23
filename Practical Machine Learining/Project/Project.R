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

#Mode fitting
trainCtrl = trainControl(method = 'cv', number = 5);

model = train(classe ~ ., 
              data = training, 
              method = 'rf',
              trControl = trainCtrl);

print(model)
print(model$finalModel)

#Test the moedl
tests = predict(model, newdata = testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(tests)
