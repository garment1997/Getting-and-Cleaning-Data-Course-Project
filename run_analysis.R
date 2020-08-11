#Getting data
dir.create("./data")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- ("./data/data.zip")
download.file(url,destfile)
unzip(zipfile = "./data/data.zip", exdir = "./data")

#Reading data
features <- read.table("./data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merge into 1 df
x <- rbind(x_train,x_test)
y <- rbind(y_train,y_test)
subject <- rbind(subject_train, subject_test)
data <- cbind(subject,x,y)

#Extract only the measurements on the mean and standard deviation for each measurement.
library(dplyr)
data <- data %>%
  select(subject,contains("mean"),contains("std"),code) 

#Use descriptive activity names to name the activities in the data set
data <- merge(data, activities, by="code", all.x = TRUE)
head(data)

#Appropriately labels the data set with descriptive variable names.
names(data)[1] = "activity code"
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))
names(data)<-gsub("tBody", "TimeBody", names(data))
names(data)<-gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
names(data)<-gsub("-std()", "STD", names(data), ignore.case = TRUE)
names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
names(data)<-gsub("angle", "Angle", names(data))
names(data)<-gsub("gravity", "Gravity", names(data))

colnames(data)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data_subjects_mean <- data %>%
  select(-`activity code`) %>%
  group_by(subject, activity) %>%
  summarize_all(mean,na.rm = TRUE) %>%
  arrange(subject, activity)

head(data_subjects_mean)