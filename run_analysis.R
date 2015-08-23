##This R script run_analysis.R does the following: 

#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##Download and read the dataset
download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="dataset.zip",mode="wb")
file.info("dataset.zip")
if (!file.exists("UCI HAR Dataset")) { 
  unzip("dataset.zip") 
}
list.files("UCI HAR Dataset",recursive=TRUE)

##read in activity and features txt files
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE )

features <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)

library(data.table)
setnames(features, names(features), c("Feature_Number", "Feature_Name"))
library(dplyr)
colnames(features)
str(features)

##find features that are average and standard deviation with grep
##pull the names of these rows into features_mean_std.names dataframe
##substitute -mean() and -std() with Mean and Std

features_mean_std<-grep("mean\\(\\)|std\\(\\)",features[,2])
features_mean_std.names <- features[features_mean_std,2]
features_mean_std.names = gsub('-mean', 'Mean', features_mean_std.names)
features_mean_std.names = gsub('-std', 'Std', features_mean_std.names)
features_mean_std.names <- gsub('[-()]', '', features_mean_std.names)

##Read in only need data from xtrain. Read in all Y-train and suject_train. Create training_data dataframe
training_set<-read.table("UCI Har Dataset/train/X_train.txt")[features_mean_std] ##only needed rows
training_labels<-read.table("UCI Har Dataset/train/Y_train.txt")
training_subject<-read.table("UCI Har Dataset/train/subject_train.txt")
training_data<-cbind(training_subject,training_labels,training_set)

##Read in only needed data from xtest. Read in all y-test and subject_test. Create test_data dataframe
test_set<-read.table("UCI Har Dataset/test/X_test.txt")[features_mean_std]
test_labels<-read.table("UCI Har Dataset/test/Y_test.txt")
test_subject<-read.table("UCI Har Dataset/test/subject_test.txt")
test_data<-cbind(test_subject,test_labels,test_set)

## Merge training and test sets
merged_data<-rbind(training_data,test_data)
##Rename columns Subject and Activity
colnames(merged_data) <- c("Subject", "Activity", features_mean_std.names)

##conver to factor Activity and Subject in merged data
merged_data$Activity<-factor(merged_data$Activity,levels=activity_labels[,1],labels=activity_labels[,2])
merged_data$Subject<-as.factor(merged_data$Subject)

library(reshape2)
##melt merged data to long format
merged_data.melted<-melt(merged_data,id = c("Subject","Activity"))
merged_data.melted$value<-as.numeric(merged_data.melted$value)

##calculate the average of each variable for each activity and each subject
merged_data.mean<-dcast(merged_data.melted,Subject + Activity ~ variable, mean)

##write to tidy_data.txt the results
write.table(merged_data.mean, "tidy_data.txt", row.names = FALSE, quote = FALSE)
