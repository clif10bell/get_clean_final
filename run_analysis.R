## run_analysis.R script for Getting and Cleaning Data Course Project


## Activate desired packages

library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)

# Clear Global Environment
rm(list=ls(all=TRUE))

## Define working directory
directory <- "c:/Clif_R_Training/getclean_project/UCI HAR Dataset/"
setwd(directory)

## STEP 1 - Read and merge the training and test sets

features <- as.data.frame(fread("./features.txt"))
feature_names <- as.vector(features[,2])
train <- fread("./train/X_train.txt", col.names = feature_names)
test <- fread("./test/X_test.txt", col.names = feature_names)
merged_data <- as.data.frame(rbind(train, test, use.names = TRUE))

## STEP 2 - Extract only the measurements on the mean and standard deviation for each measurement.

## Read in the variable names identify which include "mean()" or "std()"  I did
## not end up using this but preserving in case the code is useful in some
## other script

##mean_std_features <- lapply(features[,2], function(x) grep ('mean\\(\\)|std\\(\\)', x))
##mean_std_features <- unlist(mean_std_features)

## Extract the mean and std features from the merged data set

merged_data %>% select(contains("mean()") | contains("std()")) -> merged_data

## STEP 3 Use descriptive activity names to name the activities in the data set
## Get the activity numbers from y_train.txt. Rbind the y_train.txt files for the training and test datasets,
## and add a new variable with a descriptive activity name. Cbind those columns to the merged data.

trainy <- fread("./train/y_train.txt")
testy <- fread("./test/y_test.txt")
merged_activity_list <- as.data.frame(rbind(trainy, testy))
merged_activity_list$V1 <- as.factor(merged_activity_list$V1)
merged_activity_list %>%
  mutate(Activity = case_when(
    V1==1 ~ "Walk",
    V1==2 ~ "Walk_Up",
    V1==3 ~ "Walk_Down",
    V1==4 ~ "Sit",
    V1==5 ~ "Stand",
    V1==6 ~ "Lay"
    )) -> merged_activity_list
merged_data <- cbind(merged_activity_list, merged_data)

## STEP 4 Appropriately label the data set with descriptive variable names.
## The training and test feature variables are already labeled. Just to need to
## relabel the first two columns.

colnames(merged_data)[colnames(merged_data) == 'V1'] <- 'Activity_No'
colnames(merged_data)[colnames(merged_data) == 'Activity'] <- 'Activity_Name'

## STEP 5 From the data set in step 4, creates a second, independent tidy data set with
## the average of each variable for each activity and each subject.

## First read in the subject data from the training and test sets, rbind them
## to each other, and cbind them to the merged_data

subject_train <- fread("./train/subject_train.txt", col.names = "Subject")
subject_test <- fread("./test/subject_test.txt", col.names = "Subject")
all_subject <- rbind(subject_train, subject_test)
merged_data <- cbind(all_subject, merged_data)

## Now calculate the means

merged_data %>% group_by(Subject, Activity_Name) %>% summarize_all(mean) -> Final_Means

## Remove the column of means of activity numbers because it is not meaningful

Final_Means <- Final_Means[,-3]

## Export the tidy dataset to a comma-delimited *.txt file.

write.csv(Final_Means, "./Final_Means.csv", row.names = FALSE)
