library(dplyr)

# Load the datasets and merge data
train_X <- read.table("train/X_train.txt")
train_Y <- read.table("train/Y_train.txt")
train_subjects <- read.table("train/subject_train.txt")
train <- cbind(train_subjects, train_Y, train_X)

test_X <- read.table("test/X_test.txt")
test_Y <- read.table("test/Y_test.txt")
test_subjects <- read.table("test/subject_test.txt")
test <- cbind(test_subjects, test_Y, test_X)

alldata = rbind(train,test)

#labels
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

colnames(alldata) <- c("subject","activity",as.character(features[,2]))

#Extract only the measurements on the mean and standard deviation
columnsToKeep <- grepl("subject|activity|mean|std", colnames(alldata))
alldata <- alldata[, columnsToKeep]

# replace activity values with named factor levels
alldata$activity <- factor(alldata$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# get column names
datacols <- colnames(alldata)

# remove special characters
datacols <- gsub("[\\(\\)-]", "", datacols)

# expand abbreviations and clean up names
datacols <- gsub("^f", "frequencyDomain", datacols)
datacols <- gsub("^t", "timeDomain", datacols)
datacols <- gsub("Acc", "Accelerometer", datacols)
datacols <- gsub("Gyro", "Gyroscope", datacols)
datacols <- gsub("Mag", "Magnitude", datacols)
datacols <- gsub("Freq", "Frequency", datacols)
datacols <- gsub("mean", "Mean", datacols)
datacols <- gsub("std", "StandardDeviation", datacols)
datacols <- gsub("BodyBody", "Body", datacols)
colnames(alldata) <- datacols


result <- alldata %>% group_by(subject, activity)%>% summarise_all(funs(mean))
# output to file "tidy_data.txt"
write.table(result, "tidydata.txt", row.names = FALSE, 
            quote = FALSE)
