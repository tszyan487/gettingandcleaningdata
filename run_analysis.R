
# =============================================================================================== #  
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# =============================================================================================== #  

rm(list = ls())

library(readxl)
library(dplyr)
library(xlsx)
library(tidyr)
library(reshape)


workdir <- "C:/Users/ili/Documents/Development/Coursera/Getting and Cleaning Data/Week4/Peer Assignment/UCI HAR Dataset/"
setwd(workdir)

# Measurements
features <- read.table(paste0(workdir,"features.txt",sep=""), header = F)

# Activities
activity <- read.table(paste0(workdir,"activity_labels.txt",sep=""), header = F)
colnames(activity) <- c("Activity_Code", "Activity_Name")

train_num <- c(1:7352)
test_num <- c(1:2947)

# Import Training Set File
#
# ----------------------------------------------------------------------------------------------- #
# Production

train_dir <- "./train/"

# Train IDs
subject_train <- read.table( paste0(train_dir,"subject_train.txt", sep=""), header=F)
colnames(subject_train) <- "Subject_Train"

# Train Data Set
train_set <- read.table(paste0(train_dir,"X_train.txt",sep=""), header = F)
activity_train <- cbind(subject_train,train_num,train_set)

# Train Activities
activity_code <- read.table(paste0(train_dir,"y_train.txt",sep=""), header = F)
colnames(activity_code) <- "Activity_Code"
train_code <- cbind(subject_train,train_num,activity_code)
train_code <- merge(train_code,activity, by = "Activity_Code", all = T)

# Combine Train Data Set and Activities for Each Person
train_activity <- merge(train_code,activity_train, by = c("Subject_Train","train_num"), all = T)
names(train_activity)[1] <- "Subject"
train_activity <- cbind("Train",train_activity)
names(train_activity)[1] <- "Set"

# Combine the Train Data Set with Measurments
# Appropriately labels the data set with descriptive variable names.
names(train_activity)[6:566] <- c(as.vector(features$V2))
train_activity <- train_activity[c(1:2,5:566)]

# ----------------------------------------------------------------------------------------------- #
# Test Directory

test_dir <- "./test/"  

# Test IDs
subject_test <- read.table( paste0(test_dir,"subject_test.txt", sep=""), header=F)
colnames(subject_test) <- "Subject_Test"

# Test Data Set
test_set <- read.table(paste0(test_dir,"X_test.txt",sep=""), header = F)
activity_test <- cbind(subject_test,test_num,test_set)

# Test Activity
activity_code_test <- read.table(paste0(test_dir,"y_test.txt",sep=""), header = F)
colnames(activity_code_test) <- "Activity_Code"
test_code <- cbind(subject_test,test_num,activity_code_test)
test_code <- merge(test_code,activity, by = "Activity_Code", all = T)

# Combine Test Data Set and Activities for Each Person
test_activity <- merge(test_code,activity_test, by = c("Subject_Test","test_num"), all = T)
names(test_activity)[1] <- "Subject"
test_activity <- cbind("Test",test_activity)
names(test_activity)[1] <- "Set"

# Combine the Test Data Set with Measurments
# Appropriately labels the data set with descriptive variable names.
names(test_activity)[6:566] <- c(as.vector(features$V2))
test_activity <- test_activity[c(1:2,5:566)]

# =============================================================================================== #
# Combine the Train and Test Data together
# Uses descriptive activity names to name the activities in the data set

train_test <- rbind(train_activity,test_activity)

# Extracts only measurement contains Mean and Standard Deviation.
measure <- train_test[,grep("mean|std",names(train_test))]

# =============================================================================================== #
# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.

train_test_v2 <- melt(train_test, id=c("Subject","Activity_Name"))

train_test_v2 <- train_test_v2[c(2:5)]

train_test_mean <- cast(train_test_v2, Subject+Activity_Name ~ variable, mean)






