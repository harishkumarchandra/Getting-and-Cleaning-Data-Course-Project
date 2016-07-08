
# Set Working Directory 
# Load  Data to working Directory
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  
 # Read in the data from files 

features = read.table('./features.txt',header=FALSE);  
activityType = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE) 
xTrain = read.table('./train/x_train.txt',header=FALSE)
yTrain = read.table('./train/y_train.txt',header=FALSE)

# Assigin column names 
colnames(activityType) = c('activityId','activityType') 

colnames(subjectTrain) = "subjectId" 

colnames(xTrain) = features[,2] 

colnames(yTrain) = "activityId" 

#merge yTrain, subjectTrain, and xTrain 
trainingData = cbind(yTrain,subjectTrain,xTrain)

#Read  test data 
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
xTest = read.table('./test/x_test.txt',header=FALSE)
yTest = read.table('./test/y_test.txt',header=FALSE)

# Assign column names to the test data 
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId" 
# final test set by merging the xTest, yTest and subjectTest data 

testData = cbind(yTest,subjectTest,xTest)  
  
  
  
#Combine training and test data to create a final data set 
  
finalData = rbind(trainingData,testData); 
#1 vector for the column names from the finalData,# to select the desired mean() & stddev() columns 

colNames = colnames(finalData)
#2 Extract only the measurements on the mean and standard deviation for each measurement
meanstddev = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
finalData = finalData[meanstddev==TRUE]
  
# 3. Use descriptive activity names to name the activities in the data set   
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)
colNames = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(finalData) = colNames
### 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

finalData2= finalData[,names(finalData) != 'activityType']; 
# Summarizing the inalDataNoActivityType table to include just the mean of each variable for each activity and each subject 
tidyData = aggregate(finalData2[,names(finalData2) != c('activityId','subjectId')],by=list(activityId=finalData2$activityId,subjectId = finalData2$subjectId),mean); 
# Merging the tidyData with activityType to include descriptive acitvity names 
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE)
##head(tidyData)

write.table(tidyData, "tidy.txt", row.names = FALSE, quote = FALSE)
