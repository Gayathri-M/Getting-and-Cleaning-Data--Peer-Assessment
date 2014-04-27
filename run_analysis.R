# Getting and Cleaning Data - Peer Assessment steps

##-------- Merges the training and the test sets to create one data set.


## get the X coordinates -> Train + Test data sets and merge to DataX

TrainX <- read.table("train/X_train.txt")
TestX <- read.table("test/X_test.txt")
DataX <- rbind(TrainX, TestX)

# str(DataX)

## get the subects -> Train + Test data sets and merge to Subject

TrainSubject <- read.table("train/subject_train.txt")
TestSubject <- read.table("test/subject_test.txt")
Subject <- rbind(TrainSubject, TestSubject)

#str(Subject)

## get the Y coordinates -> Train + Test data sets and merge to DataY

TrainY <- read.table("train/y_train.txt")
TestY <- read.table("test/y_test.txt")
DataY <- rbind(TrainY, TestY)

# str(DataY)

##--------  Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
DataX <- DataX[, indices_of_good_features]
names(DataX) <- features[indices_of_good_features, 2]
names(DataX) <- tolower(gsub("\\(|\\)", "", names(DataX)))


##--------  Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
DataY[,1] = activities[DataY[,1], 2]
names(DataY) <- "activity"

##--------  Appropriately labels the data set with descriptive activity names.

names(Subject) <- "subject"
complete <- cbind(Subject, DataY, DataX)

# complete[10:15,60:68]
write.table(complete, "complete.txt")

##--------  Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(Subject)[,1]
numSubjects = length(unique(Subject)[,1])
numActivities = length(activities[,1])
numCols = dim(complete)[2]
result = complete[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- complete[complete$subject==s & complete$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}

write.table(result, "Activity_average.txt",sep="\t", row.names=FALSE)