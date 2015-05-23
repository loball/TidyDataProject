---
title: "CodeBook"
author: "lorenzoball"
date: "May 23, 2015"
output: html_document
---

This document describes the approach taken to develop the Run Analysis project assignment for the Getting and Cleaning Coursera class.


```{r}
Load the library needed for merge data tables and conducting string 
manipulations 

## Load Data.Table Closs
library(data.table)
library(stringr)
library(dplyr)
```

```{r}
Downloaded files for the analysis fromUCI Machine Learning Repository. 
A description of the data set and the file location can be found at the 
following locations: 

Repository desription:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Link to files: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#Download the zip file and extract data into working directory
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileurl, 
destfile = "Programming Assignment/getdata_projectfiles_UCI HAR Dataset.zip", 
method = "curl")
 
unzip("Programming Assignment/getdata_projectfiles_UCI HAR Dataset.zip", 
exdir = "Programming Assignment/")
```

```{r}
Variables were set for the data objects that will be used in the scripts and 
the location of the files needed for analysis.   

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

```


```{r}
Validate if the testing and training files were downloaded correctly.   If they
were not downloaded correct, and in the correct location, the script will quit.
Within the if conditionally the file is casted into a Data.Table object. Since 
the data set is in a fixed width format, strip.white is used to remove 
any leading or trailing white spaces. 

#Ensure the file exist, then put the text file into a data table
if (file.exists(vTestSet) && file.exists(vTrainSet)) {
        
        dtTrain <- as.data.table(read.table(vTrainSet, strip.white = T))
        dtTest <- as.data.table(read.table(vTestSet, strip.white = T))
        dtLabels <-  as.data.table(read.table(vLabel, strip.white = T))
        
} else{
        print("no file")
        quit()


```

```{r}
Part of the project was to properly label the observations, the activities and
the subjects of the the experiment.  This script leveraged the exsisting
column labels provided in the UCI data files.  The labels can be found at the 
following location: 
"Programming Assignment/UCI HAR Dataset/features.txt"
"Programming Assignment/UCI HAR Dataset/features_info.txt"

The next set of steps:
1. Places the labels from the text file, put them in a vector. 
2. Used the setnames function to change the label names for the testing
and training data tables. 
3. Loaded the data table with the new labels and combined the the data set 
using the cbind function.   

This process added two additional variables to the dataset resulting in 
563 variables or columns
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

The next part of this process was to add the newly created training and testing
data sets  for analyis. 

#Combine the Training and Test Data Set, create row(?) and column labels
#Training Set will be row 1:7352 and Testing will be row 7353:10299
dtComplete <- rbind(m,p)


Lastly, a new label name was created for the activity and subject grouping in
the combine/complete data set  Similar to the process before, leveraged the 
setnames function to change the column names.   Also, using the mapvalue 
function, I updated the numeric values in the Activity Type to a descriptive 
label which was provided in the downloaded dataset. 

# Set the column heading for the subject number and activity type 
setnames(dtComplete, 1:2, c("SubjectNumber","ActivityType"))

# Change the labels of the activity type to reflect appropriate labels 
# Question 3 of Assignement: Uses descriptive activity names to name the 
# activities in the data set
dtComplete$ActivityType <- mapvalues(dtComplete$ActivityType, c("1","2","3","4","5","6"), c("WALKING", 
"WALKING_UPSTAIRS","WALKING_DOWNSTAIRS", "SITTING","STANDING","LAYING"))

```

```{r}
This next operation created a vector to be used to define which column will be 
used in the data set.  The process used a for loop and the stringr library to
evaluate the data.table columns.   The for loop detected if the word "mean" or 
"std" is within a column name.  If it is then, the index of the columen will 
be noted in the vector.   

This vector will be used to create a subset of the combined data table and 
prior to the tidy data set. 


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

```


```{r}
The last set of operations grouped the tidy data set by activity and subject.  
The next step was to run a colwise mean on the columns using the ddply library.


# Create a tidy data set with the average of mean and std variables
# Question 5 of Assignement: creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.
dtComplete.tidy <- group_by(dtCompleteSub, SubjectNumber, ActivityType)
dtSummary.tidy <- ddply(dtComplete.tidy, .(SubjectNumber, ActivityType), colwise(mean))

```


```{r}
Used the write table function to write the tindy data file to the working 
directory

dtSummary.tidy
write.table(dtSummary.tidy, "cleanDataPrj_TidyData.txt", row.names = F)
```

