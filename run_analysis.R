#If you run this code in R, please split it in two parts for better functioning. 
run_analysis<-function(){
  features<-read.table("./UCI HAR Dataset/features.txt",header=F)
  subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=F)
  X_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header=F)
  Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt",header=F)
  output<-vector()
  for(i in 1:nrow(Y_train)){
    value_Ytrain<-Y_train[i,1]
    if(value_Ytrain==1){
      activity_value<-c("WALKING")
      }else{
        if(value_Ytrain==2){
          activity_value<-c("WALKING_UPSTAIRS")
        }else{
          if(value_Ytrain==3){
            activity_value<-c("WALKING_DOWNSTAIRS")
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
  output<-as.data.frame(matrix(output,nrow(Y_train),1,byrow=T))
  colnames(output)<-c("Activity")
  colnames(subject_train)<-c("Subject")
  output<-cbind(subject_train,output)
  library(data.table)
  col_Xtrain<-names(X_train)
  new_name<-features[,2]
  for(i in 1:length(X_train)){
    setnames(X_train,old=as.character(col_Xtrain[i]),new=as.character(new_name[i]))
  }
  output<-cbind(output,X_train)
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
  output_1<-cbind(output_1,X_test)
  mergedata<-rbind(output,output_1)
  col_subject<-as.data.frame(mergedata[,1:1])
  col_activity<-as.data.frame(mergedata[,2:2])
  colnames(col_subject)<-c("Subject")
  colnames(col_activity)<-c("Activity")
  df<-subset(mergedata,select=c(grep("mean..",colnames(mergedata),fixed=T,value=T),grep("std..",colnames(mergedata),fixed=T,value=T)))
  df1<-cbind(col_subject,col_activity,df)
  activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header=F)
  activity<-as.data.frame(activity_labels[,2])
  colnames(activity)<-c("Activity")
  output_2<-data.frame()
  output_3<-data.frame()
  for(i in 1:30){
    new_subset<-df1[which(df1$Subject==i),]
    for(j in 1:6){
      subset_byactivity<-activity_labels[j,2]
      data_2average<-new_subset[which(new_subset$Activity==subset_byactivity),]
      data_2average[,2:2]<-NULL
      new_vector<-as.data.frame(lapply(data_2average,mean))
      output_2<-rbind(output_2,new_vector)
    }
    output_3<-rbind(output_3,activity)
  }
  output_2<-cbind(output_3,output_2)
  write.table(output_2,file="course_project.txt",row.name=FALSE)
}

