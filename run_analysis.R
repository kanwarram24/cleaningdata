##Peer-graded Assignment: Getting and Cleaning Data Course Project
run_analysis <- function(){
  
  ##TASKS
  ##Merges the training and the test sets to create one data set.
  ##Extracts only the measurements on the mean and standard deviation for each measurement.
  ##Uses descriptive activity names to name the activities in the data set
  ##Appropriately labels the data set with descriptive variable names.
  ##From the data set in step 4, creates a second, 
  ##independent tidy data set with the average of each variable for each activity and each subject.
  
  
  
  
  ##Import the data
  
  features <- read.table("UCI HAR Dataset/features.txt",col.names = c("n","functions"))
  activities <- read.table("UCI HAR Dataset/activity_labels.txt",col.names =  c("act_id","activity"))
  
  test <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "idt")
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = features$functions)
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt",col.names = "id")
  
  
  train <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "idt")
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt",col.names =  features$functions)
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt",col.names = "id")
  
  

  
  
    
    
    
    
  ## Task 1 Merge the Data
  
  
  
  ##merging the x,y,and subject data
  
  
  names(x_test)<-names(x_train)
  X_merg<- rbind(x_test, x_train)
  Y_merg<-rbind(y_test,y_train)
  data<-rbind(test,train)
  
  
  merg<-cbind(data,Y_merg,X_merg)
  
  ##Task 2 Extracts only the measurements on the mean and standard deviation for each measurement.
  
  Data <- merg %>% select(idt, id, contains("mean"), contains("std"))
  
  
  ##Task 3 Uses descriptive activity names to name the activities in the data set
  
  Data$id <- activities[Data$id,2]
  
  ##Task 4 Appropriately labels the data set with descriptive variable names.
  
  names(Data)[2] = "activity"
  ## All meaningful names already given to the tables and their columns
  
  ##Task 5 From the data set in step 4, creates a second, 
  ##independent tidy data set with the average of each variable for each activity and each subject.
  
  Result <- Data %>%group_by(idt, activity) %>% summarise_all(funs(mean))
  
  
  
  ##return the data  
  write.table(Result, "Result.txt", row.name=FALSE)
  
    
    
}