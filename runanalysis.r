 runanalysis <- function{
   #read activity files
   Testing <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/test/y_test.txt", header = F)
   Training <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/train/y_train.txt", header = F)
   #read subject files  
   SubjTrain <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/train/subject_train.txt", header = F)
   SubjTest <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
   #read data files
   FeaTest <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
   FeaTrain <-  read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
   
   #merging files
   TesTrain <- rbind(Testing,Training)
   setnames(TesTrain, "V1","activityNum")
   Subj <- rbind(SubjTest,SubjTrain)
   setnames(Subj,"V1", "subject")
   Feating <- rbind(FeaTrain,FeaTest)
   
   # match varibales to features
   Features<- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/features.txt")
   setnames(Features, names(Features), c("featureNum","featureName")) 
   colnames(Feating)<- Features$featureName
  
   Actlab <- read.table("/Users/mcader/Desktop/JHU Data Science Course/Cleaning Data/UCI HAR Dataset/activity_labels.txt")
   setnames(Actlab, names(Actlab), c("activityNum","activityName"))
   
   #merge columns
   allData <- cbind(TesTrain,Subj)
   Feating <- cbind(allData,Feating)
   
   #extracting only measuremeents on the mean and standard dev for each measurement
   FeatMeanSD <- grep("mean\\(\\)|std||(||))", Features$featureName,value=TRUE)
   FeatMeanSD <- union(c("subject","activityNum"),FeatMeanSD)
   Feating <- subset(Feating, select = FeatMeanSD)
   
   #Using descriptive activity names to name activities in data set
   Feating <- merge(Actlab, Feating, by="activityNum",all.x = TRUE)
   Feating$activityName <- as.character(Feating$activityName)
   Feating$activityName <- as.character(Feating$activityName)
   Master <- aggregate(. ~ subject - activityName, data = Feating, mean)
   Feating <- tbl_df(arrange(Master, Subj, activityName))
   
   #Labeling Data Set
   head(str(Feating),2)
   names(Feating)<- gsub("std()","SD", names(Feating))
   names(Feating)<-gsub("mean()","MEAN", names(Feating))
   names(Feating)<-gsub("^t","time",names(Feating))
   names(Feating)<-gsub("^f","frequency", names(Feating))
   names(Feating)<-gsub("Acc","Accelerometer",names(Feating))
   names(Feating)<-gsub("Gyro","Gyroscope",names(Feating))
   names(Feating)<-gsub("Mag","Magnitude",names(Feating))
   names(Feating)<-gsub("BodyBody","Body",names(Feating))
   
   head(str(Feating),6)
   
   write.table(Feating,"cleanData.txt",row.name= FALSE)
 }
 
 