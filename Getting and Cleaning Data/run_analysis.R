rm(list=ls());
# Download file, save it, and unzip it
if(!file.exists("data")) {dir.create("data")};
fileURL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
file = paste("data",'/',"getdata_projectfiles_UCI HAR Dataset.zip", sep="");
download.file(url = fileURL, file);
unzip(file , exdir = file.path("data/"));

# Read lables
lablesfile = "./data/UCI HAR Dataset/activity_labels.txt"
activityLables = read.table(lablesfile, header = F);
lables = activityLables$V2;

# Read train and test data and merge them
trainfile = "./data/UCI HAR Dataset/train/X_train.txt"
testfile  = "./data/UCI HAR Dataset/test/X_test.txt"
train = read.table(trainfile, header = F);
test = read.table(testfile, header = F);
allData = rbind(train,test);

# Read features and lable the data appropriately
featuresfile = "./data/UCI HAR Dataset/features.txt";
activityFeatures = read.table(featuresfile, header = F);
features = as.vector(activityFeatures$V2);

colnames(allData) = features;

# Keep just mean and std features
MeanAndSdCols = grep("mean()|std()", colnames(allData));
MeanAndSdData = allData[,MeanAndSdCols];

# Re-lable columns 
# Appropriately labels the data set with descriptive variable names. 
ColNames = colnames(MeanAndSdData);
ColNames = gsub('\\(\\)', "", ColNames);
ColNames = gsub('mean', "Mean", ColNames);
ColNames = gsub('std', "STD", ColNames)

## Creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject.
# Set activity IDs
TrainActivityIDs = read.table("./data/UCI HAR Dataset/train/y_train.txt", header=FALSE);
TestActivityIDs  = read.table("./data/UCI HAR Dataset/test/y_test.txt", header=FALSE);
ActivityIDs      = rbind(TrainActivityIDs, TestActivityIDs)$V1;

# Set acitivity lables
ActivityLabels = read.table("./data/UCI HAR Dataset/activity_labels.txt", header=FALSE);
colnames(ActivityLabels) = c("ActivityID", "ActivityName");
LabledData = merge(allData, ActivityLabels);

# Set Subject IDs
TrainSubjects = read.table("./data/UCI HAR Dataset/train/subject_train.txt", header=FALSE);
TestSubjects  = read.table("./data/UCI HAR Dataset/test/subject_test.txt", header=FALSE);
SubjectIDs    = rbind(TrainSubjects, TestSubjects)$V1; 

LabledDataWithSubjectID = cbind(LabledData,SubjectIDs);

# Tidying up the dataset
SecondDataSet = split(LabledDataWithSubjectID[,1:561], 
      list(LabledDataWithSubjectID$ActivityName, LabledDataWithSubjectID$SubjectIDs));

TidyDataset = sapply(SecondDataSet, colMeans)

# Save results
write.table(TidyDataset, file = "Results.txt", row.name = F);
