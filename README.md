## Getting-CleaningData_CourseraProject
## INTRODUCTION
The source files are located in the working directory within a folder called “./UCI HAR Dataset/”. There are two folders called: “./UCI HAR Dataset/train” and “./UCI HAR Dataset/test" 

“./UCI HAR Dataset/train” is composed with the following files: subject_train.txt, X_train.txt, y_train.txt.

“./UCI HAR Dataset/test" is composed with the following files: subject_test.txt, X_test.txt, y_test.txt.

subject_train.txt and subject_test.txt Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

y_train.txt and y_test.txt are training and test labels. Its range from 1 to 6 representing the activities performed by subjects. Activities: 1=WALKING, 2=WALKING_UPSTAIRS, 3=WALKING_DOWNSTAIRS, 4=SITTING, 5=STANDING, 6=LAYING.

X_train.txt and X_test.txt are the training and test set with the measurement of variables for each subject. There are 561 variables measured. 

There are two other files called activity_labels.txt with the list of activities and features.txt with the name of the 561 variables measured. 


##FUNCTION
The following function creates a table with the average of each variable for each activity and each subject.
1st: it creates a data frame called output with the names of activities instead of numbers given by table y_train.txt. Then it merges subject_train.txt, output.

run_analysis <- function() {
  
  features <- read.table("./UCI HAR Dataset/features.txt", header = F)
  
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = F)
  
  X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = F)
  
  Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = F)
  
  output <- vector()
  
  for (i in 1:nrow(Y_train)) {
    value_Ytrain <- Y_train[i,1]
    
    if (value_Ytrain == 1) {
      activity_value <- c("WALKING")
      } else {
        if (value_Ytrain == 2) {
          activity_value <- c("WALKING_UPSTAIRS")
        } else {
          if (value_Ytrain == 3) {
            activity_value <- c("WALKING_DOWNSTAIRS")
          } else {
            if(value_Ytrain==4){
              activity_value<-c("SITTING")
            }else{
              if(value_Ytrain==5){
                activity_value<-c("STANDING")
              }else{
                if(value_Ytrain==6){
                  activity_value<-c("LAYING")
                }
              }
            }
          }
        }
      }
    output<-append(output,activity_value)
  }
  
  output<-as.data.frame(matrix(output,nrow(Y_train),1,byrow=T))
  
  colnames(output)<-c("Activity")
  
  colnames(subject_train)<-c("Subject")
  
  output<-cbind(subject_train,output)
}



