#   Using data collected from the accelerometers from the Samsung Galaxy S smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt". See README.md for details.

library(dplyr)
library(dataMaid)

# Download the data
myURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
myFile <- "UCI HAR Dataset.zip"

if (!file.exists(myFile)) {
  download.file(myURL, myFile, mode = "wb")
}

myPath <- "UCI HAR Dataset"
if (!file.exists(myPath)) {
  unzip(myFile)
}

# Read the data in
# Training
trainSubject <- read.table(file.path(myPath, "train", "subject_train.txt"))
trainXValues <- read.table(file.path(myPath, "train", "X_train.txt"))
trainYValues <- read.table(file.path(myPath, "train", "y_train.txt"))

# Test
testSubjects <- read.table(file.path(myPath, "test", "subject_test.txt"))
testXValues <- read.table(file.path(myPath, "test", "X_test.txt"))
testYValues <- read.table(file.path(myPath, "test", "y_test.txt"))

# Features
myFeatures <- read.table(file.path(myPath, "features.txt"), as.is = TRUE)

# Activity labels
myActivities <- read.table(file.path(myPath, "activity_labels.txt"))
colnames(myActivities) <- c("activityId", "activityLabel")

# Merge training and test data
myJoinedActivity <- rbind(
  cbind(trainSubject, trainXValues, trainYValues),
  cbind(testSubjects, testXValues, testYValues)
)

# assign column names
colnames(myJoinedActivity) <- c("subject", myFeatures[, 2], "activity")

# Get the mean and standard deviation for each measurement
myColumnsToKeep <- grepl("subject|activity|mean|std", colnames(myJoinedActivity))
myJoinedActivity <- myJoinedActivity[, myColumnsToKeep]

# Relabel the data set with better names
myJoinedActivity$activity <- factor(myJoinedActivity$activity, levels = myActivities[, 1], labels = myActivities[, 2])
myJoinedCols <- colnames(myJoinedActivity)
myJoinedCols <- gsub("[\\(\\)-]", "", myJoinedCols)

myJoinedCols <- gsub("^f", "Frequency", myJoinedCols)
myJoinedCols <- gsub("^t", "Time", myJoinedCols)
myJoinedCols <- gsub("BodyBody", "Body", myJoinedCols)

colnames(myJoinedActivity) <- myJoinedCols

# Export a tidy set with the average of each variable for each activity and subject
myActivityMeans <- myJoinedActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(list(mean))

write.table(myActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)
makeCodebook(myActivityMeans)