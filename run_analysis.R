require(dplyr)
colList<-c()
colNameList<-c()

#1. read testFile into dataset
testFile<-"UCI HAR Dataset\\test\\X_test.txt"
dataset <- read.table(testFile,header=FALSE)

#2. extract training file and merge with test file
trainFile<-"UCI HAR Dataset\\train\\X_train.txt"
temp_dataset <-read.table(trainFile, header = FALSE)
dataset<-rbind(dataset, temp_dataset)
rm(temp_dataset)

#3. find features with mean or std in their names
featuresFile<-read.table("UCI HAR Dataset\\features.txt", header=FALSE)

for (i in 1:nrow(featuresFile)){
  colName <- tolower(featuresFile[i,2])  
  
  if ((length(grep ("mean",colName))>0) || (length(grep ("std",colName)>0))) {
   colList[length(colList)+1]<-i
   colNameList[length(colNameList)+1]<-colName
  }
}

#3a. extract requested columns and assign column names
dataset <-dataset[,colList]
colnames(dataset)<-colNameList

#4a. create ActivityFile dataset with test activities
ActivityFile<-read.table("UCI HAR Dataset\\test\\Y_test.txt", header=FALSE)

#4b. extract training activities and merge with test activities
temp_dataset <-read.table("UCI HAR Dataset\\train\\Y_train.txt",  header = FALSE)
ActivityFile<-rbind(ActivityFile, temp_dataset)
rm(temp_dataset)

#4c. merge activity dataset with cleaned dataset
dataset$activity<-ifelse(ActivityFile[1]==1, "WALKING", ifelse(ActivityFile[1]==2, "WALKING_UPSTAIRS",ifelse(ActivityFile[1]==3, "WALKING_DOWNSTAIRS",ifelse(ActivityFile[1]==4, "SITTING",ifelse(ActivityFile[1]==5, "STANDING",ifelse(ActivityFile[1]==6, "LAYING","NA"))))))

#5 add subjects to dataset
SubjectFile<-read.table("UCI HAR Dataset\\test\\subject_test.txt", header=FALSE)
temp_dataset <-read.table("UCI HAR Dataset\\train\\subject_train.txt",  header = FALSE)
SubjectFile<-rbind(SubjectFile, temp_dataset)
rm(temp_dataset)

setnames(SubjectFile,"V1","subject")
dataset<-cbind(SubjectFile[1],dataset)

#set the key to be (activity,subject):first convert the dataframe to a data table
dataset<-data.table(dataset)
setkey(dataset,activity,subject)

#melt the dataset into 4 columns: activity,subject, variable and value
dataset<-data.table(melt(dataset,key(dataset),variable.name = "variable"))
#cast it so that subject and activity are the keys and take mean of remaining variables
tidy<-dcast(dataset,subject+activity ~ variable,mean)

write.table(tidy,file="UCI HAR Dataset\\tidy.txt",row.name=FALSE)
