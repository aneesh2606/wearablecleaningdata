library(dplyr)
library(plyr)

run_analysis <- function()
{
    x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
    y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
    x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
    y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
    subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
    subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
    features <- read.table('./UCI HAR Dataset/features.txt')
    activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt')
    
    x_data <- rbind(x_train,x_test)
    y_data <- rbind(y_train,y_test)
    
    subject_data <- c(subject_train$V1,subject_test$V1)
    tidy_data <- cbind(y_data$V1,subject_data,x_data)
    
    features_vector <- as.character(features$V2)
    columns <- c("ActivityID", "Subject", features_vector ) ##all columns
    
    names(tidy_data) <- columns
    relevant_features <- features_vector[grepl("std[(]+[)]+|mean[(]+[)]+",features_vector)]  ##get mean and std deviation
    relevant_features <- c( "ActivityID", "Subject", relevant_features )
    tidy_data <- tidy_data[,relevant_features]
    
    activity_labels <- data.frame(activity_labels)
    names(activity_labels) <- c("ActivityID", "Activity")
    tidy_data <- merge( activity_labels, tidy_data, by = "ActivityID")
    tidy_data$ActivityID <- NULL
    
    tidy_data <- ddply(tidy_data, .(Activity, Subject), numcolwise(mean) )
    tidy_data
}
