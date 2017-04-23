## Set working directory
setwd('C:/Users/xxxxx/Desktop/Get & Clean Data Project')

## download and unzip the file
filename <-"getdata_Project_Dateset.zip"

if(!file.exists(filename)){
     fileUrl <-'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
     download.file(fileUrl, filename)
 }

if(!file.exists('UCI HAR Dataset')){
    unzip(filename)
}


## 1. Merge the training and the test sets to create one data set
# Read the training data set from raw files
Features = read.table('./UCI HAR Dataset/features.txt', header=FALSE)
ActivityLabels = read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE)
SubjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt', header=FALSE)
xTrain = read.table('./UCI HAR Dataset/train/x_train.txt', header=FALSE)
yTrain = read.table('./UCI HAR Dataset/train/y_train.txt', header=FALSE)
 
# Assign column names
colnames(ActivityLabels) = c('activityId','activityType')
colnames(SubjectTrain) = "subjectId"
colnames(xTrain) = Features[,2]
colnames(yTrain) = "activityId"

# Create the training data set by combining yTrain, SubjectTrain and xTrain 
TrainingData = cbind(yTrain,SubjectTrain,xTrain)
 
# Read the test data set from raw files 
SubjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)
 
# Assign column names 
colnames(SubjectTest) = "subjectId"
colnames(xTest)       = Features[,2]
colnames(yTest)       = "activityId"

# Create the test data set by combining yTest, SubjectTest and xTest 
TestData = cbind(yTest,SubjectTest,xTest)

# Create the final data set
FinalData = rbind(TrainingData,TestData)


## 2. Extract only the measurements on the mean and standard deviation for each measurement
# Create a vector for the column names to select mean and stdev columns
ColNames  = colnames(FinalData)

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",ColNames) | grepl("subject..",ColNames) | grepl("-mean..",ColNames) & !grepl("-meanFreq..",ColNames) & !grepl("mean..-",ColNames) | grepl("-std..",ColNames) & !grepl("-std()..-",ColNames))

# Subset finalData table based on the logicalVector to keep only desired columns
FinalData = FinalData[logicalVector==TRUE]


## 3. Use descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names

FinalData = merge(FinalData,ActivityLabels,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
ColNames  = colnames(FinalData)


## 4. Appropriately label the data set with descriptive activity names

# Cleaning up the variable names
for (i in 1:length(ColNames)) 
{
     ColNames[i] = gsub("\\()","",ColNames[i])
     ColNames[i] = gsub("-std$","StdDev",ColNames[i])
     ColNames[i] = gsub("-mean","Mean",ColNames[i])
     ColNames[i] = gsub("^(t)","time",ColNames[i])
     ColNames[i] = gsub("^(f)","freq",ColNames[i])
     ColNames[i] = gsub("([Gg]ravity)","Gravity",ColNames[i])
     ColNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",ColNames[i])
     ColNames[i] = gsub("[Gg]yro","Gyro",ColNames[i])
     ColNames[i] = gsub("AccMag","AccMagnitude",ColNames[i])
     ColNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",ColNames[i])
     ColNames[i] = gsub("JerkMag","JerkMagnitude",ColNames[i])
     ColNames[i] = gsub("GyroMag","GyroMagnitude",ColNames[i])
 }

# Reassigning the new descriptive column names to the finalData set
ColNames = colnames(FinalData)


## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table without the activityType column
FinalDataNoActivityType = FinalData[,names(FinalData) != 'activityType']
 
# Summarizing the  table to include just the mean of each variable for each activity and subject
TidyData = aggregate(FinalDataNoActivityType[,names(FinalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=FinalDataNoActivityType$activityId,subjectId = FinalDataNoActivityType$subjectId),mean)
 
# Merging the TidyData with ActivityLabels to include descriptive acitvity names
TidyData = merge(TidyData,ActivityLabels,by='activityId',all.x=TRUE)

# Export the TidyData set 
write.table(TidyData, './TidyData.txt',row.names=TRUE,sep='\t')

