library(dplyr)
library(tidyr)
library(readr)
library(magrittr)

dataURL<-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
temp<-tempfile()
download.file(dataURL,temp)
unzip(temp,exdir="./")
unlink(temp)
rm(dataURL)

## Read and store data files as data frame 
features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

## Merges the training and the test sets
subject_merge<-rbind(subject_train,subject_test)
x_merge<-rbind(x_train,x_test)
y_merge<-rbind(y_train,y_test)

## Extracts only the measurements on the mean and standard deviation for each measurement.
select_mean_std<- select(x_merge, grep("mean[(][)]|std[(][)]", features[,2]))

## Create one data set
onedataset<-cbind(subject_merge,y_merge,select_mean_std)

## Labels the data set with descriptive variable names.
colnames(onedataset)<-c("subject","activity",
                        grep("mean[(][)]|std[(][)]", features[,2], value =TRUE)
                        )

## Uses descriptive activity names to name the activities in the data set
onedataset<-mutate(onedataset,activity=activity_labels[,2][onedataset[[2]]])
colnames(onedataset)%<>%
      sub("^t","time ", .)%>%
      sub("^f","freq ", .)%>%
      sub("Acc", " acceleration", .)%>%
      sub("Jerk", " jerk", .)%>%
      sub("Gyro", " gyroscope", .)%>%
      sub("Mag", " magnitude", .)%>%
      sub("-mean[(][)]", " signal mean value", .)%>%
      sub("-std[(][)]", " signal standard deviation", .)%>%
      sub("-X", " in the X direction", .)%>%
      sub("-Y", " in the Y direction", .)%>%
      sub("-Z", " in the Z direction", .)

## From the data set, 'onedataset', creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
## A nested 'for' loop is used to iterate over each subject and each activity.
## The average of each variable is calculated and row bind to tidy data set.
tidydata<-data.frame(matrix(nrow = 0, ncol = length(onedataset)))
for (i in unique(onedataset$subject)){
      for (j in unique(onedataset$activity)){
            col_mean<- onedataset %>% 
                  filter(subject==i, activity==j) %>% 
                  select(-subject,-activity) %>% colMeans()
            tidydata<- rbind(tidydata,c('subject'=i,'activity'=j,col_mean))
      }
}
colnames(tidydata)<-colnames(onedataset)
View(tidydata)


## Creates a tidy data text file in working directory
write.table(tidydata,"tidydata.txt", sep="\t",  row.names = FALSE)


