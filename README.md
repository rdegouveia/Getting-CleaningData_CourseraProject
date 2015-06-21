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
The following function creates a data frame with the average of each variable for each activity and each subject. Before calculating the average the function:

1st: merges train and test set,  renames columns and activities.

2nd: extracts the measurements on the mean and standard deviation for each measurement

3rd: and then calculate the average of each variable, for each activity and each subject. 

1st: The function merges train and test set as follows:

The function merges the train set. It means the tables: X_train,y_train, replacing y_train with the activities names and replacing column's names of X_train with names from features.txt. 

It creates a data frame called output with the names of activities instead of labels given by table y_train.txt. Then it merges subject_train.txt and output as follows:

<!-- -->

    run_analysis<-function() {
     features<-read.table("./UCI HAR Dataset/features.txt", header=F)
     subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt", header=F)
     X_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header=F)
     Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt",header=F)
     output<-vector()
     
     #It evaluates each line of Y_train then creates a vector called output with the activities names. 
     #If Y_train[i,1]==1 then the activity will be "Walking", if Y_train[i,1]==2 
     #activity will be "Walking_Uptairs", etc.
     
     For (i in 1:nrow(Y_train)){
       value_Ytrain<-Y_train[i,1]
        if (value_Ytrain==1) {
          activity_value<-c("WALKING")
          }else{
            if (value_Ytrain==2){
              activity_value<-c("WALKING_UPSTAIRS")
              }else{
                if (value_Ytrain == 3){
                  activity_value <- c("WALKING_DOWNSTAIRS")
                  }else{
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
      
      #Transform the vector output to a data.frame and add a column with the subjects. The results is a data frame 
      #called output with the name of activities performed by subjects_train.
      
      output<-as.data.frame(matrix(output,nrow(Y_train),1,byrow=T))
      colnames(output)<-c("Activity")
      colnames(subject_train)<-c("Subject")
      output<-cbind(subject_train,output)

It creates subset with column's names of X_train. It creates a subset with the 2nd column of features.txt which are the names of variables measured. Then it replaces the names of the columns of X_train with the names from features.txt. Using setnames from library data.table.

<!-- -->

    library(data.table)
    col_Xtrain<-names(X_train)
    new_name<-features[,2]
    #It evaluates names(X_train)[i] and replace with a new name from features.txt with the subset new_name[i]
    for(i in 1:length(X_train)){
        setnames(X_train,old=as.character(col_Xtrain[i]),new=as.character(new_name[i]))
    }
    #Then it merges X_train and output. The results is a data frame called output 
    #with subjects, variables measured and activities from train set:
    output<-cbind(output,X_train)

Then the function merges the test set. It means the tables: X_test,y_test, replacing y_test with the activities names and replacing column's names of X_test with names from features.txt. Same steps saw above but in this case with files from test set.

<!-- -->

     X_test<-read.table("./UCI HAR Dataset/test/X_test.txt",header=F)
     Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt",header=F)
     subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=F)
     output_1<-vector()
     for(i in 1:nrow(Y_test)){
      value_Ytest<-Y_test[i,1]
      if(value_Ytest==1){
        activity_value<-c("WALKING")
        }else{
          if(value_Ytest==2){
            activity_value<-c("WALKING_UPSTAIRS")
            }else{
              if(value_Ytest==3){
                activity_value<-c("WALKING_DOWNSTAIRS")
                }else{
                  if(value_Ytest==4){
                    activity_value<-c("SITTING")
                    }else{
                      if(value_Ytest==5){
                        activity_value<-c("STANDING")
                        }else{
                           if(value_Ytest==6){
                            activity_value<-c("LAYING")
                            }
                        }
                    }
                 }
            }
        }
     output_1<-append(output_1,activity_value)
    }
     output_1<-as.data.frame(matrix(output_1,nrow(Y_test),1,byrow=T))
     colnames(output_1)<-c("Activity")
     colnames(subject_test)<-c("Subject")
     output_1<-cbind(subject_test,output_1)
     col_Xtest<-names(X_test)
     for(i in 1:length(X_test)){
        setnames(X_test,old=as.character(col_Xtest[i]),new=as.character(new_name[i]))
        }
        
    #The results is a data frame called output_1  with subjects, variables measured and activities from test set:
    output_1<-cbind(output_1,X_test)
    mergedata<-rbind(output,output_1)

Finally the function merges output and output_1. The results is a data frame called mergedata with train and test sets. All columns were renamed with the features.txt file and labels from y_train.txt and y_test.txt files were renamed with the activities names. 

2nd: extracts the measurements on the mean and standard deviation for each measurement

<!-- -->

col_subject<-as.data.frame(mergedata[,1:1])
  col_activity<-as.data.frame(mergedata[,2:2])
  colnames(col_subject)<-c("Subject")
  colnames(col_activity)<-c("Activity")
  df<-subset(mergedata,select=c(grep("mean..",colnames(mergedata),fixed=T,value=T),grep("std..",colnames(mergedata),fixed=T,value=T)))
  df1<-cbind(col_subject,col_activity,df)
