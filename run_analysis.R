## run_analysis.R
## Setting the scene
library(data.table)
library(dplyr)

wd <- getwd()
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL, file.path(wd, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

x_train <- read.table("./train/X_train.txt", header = FALSE)
y_train <- read.table("./train/y_train.txt", header = FALSE)
subject_train <- read.table("./train/subject_train.txt", header = FALSE)

x_test <- read.table("./test/X_test.txt", header = FALSE)
y_test <- read.table("./test/y_test.txt", header = FALSE)
subject_test <- read.table("./test/subject_test.txt", header = FALSE)

features <- read.table("features.txt", header = FALSE)
features <- as.data.table(features[,2]); names(features) <- "features"
features$duplicate <- duplicated(features); features$appendix <- ifelse(features$duplicate == TRUE, "D", ""); features$paste <- paste(features$features, features$appendix, sep="")
features$dduplicate <- duplicated(features$paste); features$aappendix <- ifelse(features$dduplicate == TRUE, "D", ""); features$ppaste <- paste(features$paste, features$aappendix, sep="")

columns <- as.vector(features$ppaste)

labels <- as.data.table(read.table("activity_labels.txt", header=FALSE))
names(labels) <- c("ID", "Activity")

## Merge the training and the test sets to create one data set.
train_data <- cbind(subject_train, y_train, x_train)
test_data <- cbind(subject_test, y_test, x_test)
data <- as.data.table(rbind(train_data, test_data))

## Extract only the measurements on the mean and standard deviation for each measurement.
names(data) <- c("Subject", "Activity", columns)
data <- select(data, Subject:Activity, matches("mean()"), matches("std()"))

## Use descriptive activity names to name the activities in the data set.
for (i in 1:nrow(labels)){
      data$Activity <- replace(data$Activity, data$Activity == i, as.character(labels[i, Activity]))
}

## Appropriately label the data set with descriptive variable names.
names(data) <- gsub("-", ".", names(data))
names(data) <- gsub(",", ".", names(data))
names(data) <- gsub("\\()", "", names(data))
names(data) <- gsub("\\(", ".", names(data))
names(data) <- gsub("\\)", "", names(data))
names(data) <- gsub("tBody", "timeBody", names(data))
names(data) <- gsub("tGrav", "timeGrav", names(data))
names(data) <- gsub("fBody", "freqBody", names(data))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data2 <- group_by(data, Subject, Activity) %>% summarise_all(mean)

print(data2)
