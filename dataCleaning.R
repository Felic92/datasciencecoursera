#Downloads extracts contents of source into "UCI HAR Dataset"folder in working directory
#temp <- tempfile()
#download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp,method="curl")
#unzip(temp)

#Identifies the subjects[1,30] who performed the activity
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Read in the training data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Read in the testing data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# rbind the X_test data to the X_train data
sensor_data <- rbind(X_test,X_train)

# Read in the feature labels 
feature_labels <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)

# Subset the desired column names from the data frame
feature_names <- feature_labels$V2

# Associate each feature name with its correspoinding column in sensor data 
colnames(sensor_data) <- feature_names

# Subset columns of "sensor_data" to include only measurements related to mean or standard deviation
sensor_data <- sensor_data[,grep("mean|std",colnames(sensor_data))]

# specify patterns and their replacements replace for the feature names
patterns = c("-|\\()","X","Y","Z","mean","std","Acc","Mag","Gyro","Freq")
replacements = c("","Xaxis","Yaxis","Zaxis","Mean","StandardDeviation","Acceleration","Magnitude","Gyroscope","Frequency")

#Loop using gsub to perform actual replacements
for(index in 1:length(patterns)){
    colnames(sensor_data) <- gsub(patterns[index],replacements[index],colnames(sensor_data))
}

# rbind the Y_test to the Y_train data and provide descriptive variable name
activity_data <- rbind(Y_test,Y_train)
colnames(activity_data) <- "activityLabel"

# Specify descriptive activity names to replace numeric activitiy codes
activities = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")

# Loop using subsetting to replace codes with descriptive names
for(index in 1:length(activities)){
    activity_data[activity_data$activityLabel == index,] = activities[index]
}

# rbind the subject_test data to the subject_train data and provide descriptive variable name
subjectId_data <- rbind(subject_test,subject_train)
colnames(subjectId_data) <- "subjectId"

# Free space taken by data which will no longer be used
rm(X_test,X_train,Y_test,Y_train,subject_test,subject_train,feature_labels,feature_names)

# cbind the activity_data, sensor_data, and subjectId_data into one dataset
data_set <- cbind(sensor_data,activity_data,subjectId_data)

# Free space taken by data which will no longer be used
rm(sensor_data,activity_data,subjectId_data)

# Ref: http://www.statmethods.net/management/aggregate.html 
tidyDataset <- aggregate(data_set[,1:79],by=list(data_set$subjectId,data_set$activityLabel),FUN = mean)

library(plyr)
# Correct column names that were changed during aggregate
tidyDataset <- rename(tidyDataset, c("Group.1"="subjectId", "Group.2"="activityLabel"))

# Write tidy dataset to a file and remove it from workspace
write.table(tidyDataset,file="tidyDataset.txt",row.names=FALSE)
rm(tidyDataset)

#To read the data frame into R uncommet the following command 
# tidyData <- read.table("./tidyDataset.txt",header = TRUE)


### ========= End script ========= ###

#Decided to replace the following walls of text with loops to perform the same function
#colnames(sensor_data) <- gsub("-|\\()", "", colnames(sensor_data))
#colnames(sensor_data) <- gsub("X","Xaxis", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Y","Yaxis", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Z","Zaxis", colnames(sensor_data))
#colnames(sensor_data) <- gsub("mean", "Mean", colnames(sensor_data))
#colnames(sensor_data) <- gsub("std", "StandardDeviation", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Acc", "Acceleration", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Mag", "Magnitude", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Gyro", "Gyroscope", colnames(sensor_data))
#colnames(sensor_data) <- gsub("Freq", "Frequency", colnames(sensor_data))

#activity_data[activity_data$activityLabel == 1,] = "WALKING"
#activity_data[activity_data$activityLabel == 2,] = "WALKING_UPSTAIRS"
#activity_data[activity_data$activityLabel == 3,] = "WALKING_DOWNSTAIRS"
#activity_data[activity_data$activityLabel == 4,] = "SITTING"
#activity_data[activity_data$activityLabel == 5,] = "STANDING"
#activity_data[activity_data$activityLabel == 6,] = "LAYING"

### The following strategy was inspired by this post:
# http://www.r-bloggers.com/using-r-quickly-calculating-summary-statistics-from-a-data-frame/

#library(reshape2)
#library(plyr)

# Melt the data to separate id variables from measurments 
#moltenData <- melt(data_set, id.vars = c("subjectId","activityLabel"))

# use the split apply combine approach to summarize the readings
#tidyDataSet <- ddply(moltenData, c("subjectId","activityLabel"), summarise, mean = mean(value))
###
