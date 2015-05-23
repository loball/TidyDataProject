###############################
## Coursera - Reading and Cleaning Data 
## Course Project Assignment
## 
## Author: Lorenzo Ball
## Date: 5/21/15
## 
## This script takes data from the HCI Machine Learning Data Website, creates
## a tidy data set, and run a average on the columns
###############################

## Load Data.Table Closs
library(data.table)
library(stringr)
library(dplyr)
library(plyr)


#Download the zip file and extract data into working directory
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileurl, 
destfile = "Programming Assignment/getdata_projectfiles_UCI HAR Dataset.zip", 
method = "curl")
 
unzip("Programming Assignment/getdata_projectfiles_UCI HAR Dataset.zip", 
exdir = "Programming Assignment/")

# Set variables
dtTest <- data.table()
dtTrain <- data.table()
dtComplete <- data.table()
dtCompleteSub <- data.table()
dtLabels <- data.table()
dtSbjLbl <- data.table()
dtActLbl <- data.table()

#Create variables for file location  
vTrainSet <- "Programming Assignment/UCI HAR Dataset/train/X_train.txt"
vTestSet <- "Programming Assignment/UCI HAR Dataset/test/X_test.txt"
vLabel <- "Programming Assignment/UCI HAR Dataset/features.txt"
vTrainActLabel <- "Programming Assignment/UCI HAR Dataset/train/Y_train.txt"
vTrainSbjLabel <- "Programming Assignment/UCI HAR Dataset/train/subject_train.txt"
vTestActLabel <- "Programming Assignment/UCI HAR Dataset/test/Y_test.txt"
vTestSbjLabel <- "Programming Assignment/UCI HAR Dataset/test/subject_test.txt"

#Ensure the file exist, then put the text file into a data table
if (file.exists(vTestSet) && file.exists(vTrainSet)) {
        
        dtTrain <- as.data.table(read.table(vTrainSet, strip.white = T))
        dtTest <- as.data.table(read.table(vTestSet, strip.white = T))
        dtLabels <-  as.data.table(read.table(vLabel, strip.white = T))
        
} else{
        print("no file")
        quit()
}

#Put current table labels into an array and apply to the combined 
#data set
vtrLabels <- as.character(dtLabels[, V2])
setnames(dtTrain, 1:561, vtrLabels)
setnames(dtTest, 1:561, vtrLabels)

#Load Subject and Activity for the training data sets 
dtSbjLbl <-  as.data.table(read.table(vTrainSbjLabel, strip.white = T))
dtActLbl <-  as.data.table(read.table(vTrainActLabel, strip.white = T))
m <- cbind(dtSbjLbl,dtActLbl,dtTrain)

#Load Subject and Activity for the testing data sets 
dtSbjLbl <-  as.data.table(read.table(vTestSbjLabel, strip.white = T))
dtActLbl <-  as.data.table(read.table(vTestActLabel, strip.white = T))
p <- cbind(dtSbjLbl,dtActLbl,dtTest)


#Combine the Training and Test Data Set, create row(?) and column labels
#Training Set will be row 1:7352 and Testing will be row 7353:10299
dtComplete <- rbind(m,p)

# Set the column heading for the subject number and activity type 
setnames(dtComplete, 1:2, c("SubjectNumber","ActivityType"))

# Change the labels of the activity type to reflect appropriate labels 
# Question 3 of Assignement: Uses descriptive activity names to name the 
# activities in the data set
dtComplete$ActivityType <- mapvalues(dtComplete$ActivityType, c("1","2","3","4","5","6"), c("WALKING", 
"WALKING_UPSTAIRS","WALKING_DOWNSTAIRS", "SITTING","STANDING","LAYING"))

#Look for the columns that are mean and std to relabel and subset into another
#Data Table
#subset vector
vColSubset <- vector()
vColSubset <- c("SubjectNumber","ActivityType")
vCounter = 3

for (i in 1:ncol(dtComplete)){
        
       vName <- colnames(dtComplete)[i]
        
        if(str_detect(vName, "mean")){
                
                vColSubset[vCounter] <- vName
                vCounter <- vCounter + 1
                
        } else if(str_detect(vName, "std")){
                
                vColSubset[vCounter] <- vName
                vCounter <- vCounter + 1
                
        }
        
}

# Create a subset data set of all the mean and std variables 
# Question 2 of Assignement: Extracts only the measurements on the mean 
# and standard deviation for each measurement. 
dtCompleteSub <- subset(dtComplete,,select=vColSubset)

# Create a tidy data set with the average of mean and std variables
# Question 5 of Assignement: creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
dtComplete.tidy <- group_by(dtCompleteSub, SubjectNumber, ActivityType)
dtSummary.tidy <- ddply(dtComplete.tidy, .(SubjectNumber, ActivityType), colwise(mean))

dtSummary.tidy
write.table(dtSummary.tidy, "cleanDataPrj_TidyData.txt", row.names = F)