
library(dplyr)
#Download and data preparation
data <- "Coursera_DS3_Final.zip"

if (!file.exists(data)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, data, method="curl")
}  

if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

#Naming my data frames

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
xtest <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
ytest <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
ytrain <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#1: Merges the training and the test sets to create one data set.

x <- rbind(xtrain, xtest)
y <- rbind(ytrain, ytest)
subject <- rbind(subjecttrain, subjecttest)
mergeddata <- cbind(subject, y, x)

#2: Extracts only the measurements on the mean and standard deviation for each measurement.

tidy <- mergeddata %>% select(subject, code, contains("mean"), contains("std"))

#3: Uses descriptive activity names to name the activities in the data set.

tidy$code <- activities[tidy$code, 2]

#4: Appropriately labels the data set with descriptive variable names.

names(tidy)[2] = "activity"
names(tidy)<-gsub("Acc", "Accelerometer", names(tidy))
names(tidy)<-gsub("Gyro", "Gyroscope", names(tidy))
names(tidy)<-gsub("BodyBody", "Body", names(tidy))
names(tidy)<-gsub("Mag", "Magnitude", names(tidy))
names(tidy)<-gsub("^t", "Time", names(tidy))
names(tidy)<-gsub("^f", "Frequency", names(tidy))
names(tidy)<-gsub("tBody", "TimeBody", names(tidy))
names(tidy)<-gsub("-mean()", "Mean", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-std()", "STD", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-freq()", "Frequency", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("angle", "Angle", names(tidy))
names(tidy)<-gsub("gravity", "Gravity", names(tidy))

#5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

final <- tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
#save as "finaldata.txt"
write.table(final, "finaldata.txt", row.name=FALSE)
str(final)
final
