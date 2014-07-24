  filepath = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  temp <- tempfile()
  download.file(filepath,temp)
  file_ls <- as.character(unzip(temp, list = TRUE)$Name)
  for (i in file_ls) {
    #load test data
    if( grepl("*subject_test.txt",i)) subject_test <- read.table(unz(temp, i));
    if( grepl("*X_test.txt",i)) X_test <- read.table(unz(temp, i));
    if( grepl("*body_acc_z_test.txt",i)) acc_z_test <- read.table(unz(temp, i));
    #load train data
    if( grepl("*y_test.txt",i)) y_test <- read.table(unz(temp, i));
    if( grepl("*subject_train.txt",i)) subject_train <- read.table(unz(temp, i));
    if( grepl("*X_train.txt",i)) X_train <- read.table(unz(temp, i));
    if( grepl("*y_train.txt",i)) y_train  <- read.table(unz(temp, i));
    #load activity names
    if( grepl("*activity_labels.txt",i)) activity_labels <- read.table(unz(temp, i));
    #load feature names
    if( grepl("*features.txt",i)) features <- read.table(unz(temp, i));
  }
  unlink(temp)
  
  headers <- features[,2]
  
  #name columns of test and train features
  names(X_test) <- headers
  names(X_train) <- headers
  
  #select only mean and std headers
  mean_and_std <- grepl("mean\\(\\)|std\\(\\)", headers)
  
  #filter mean and std columns on test and train
  X_test_mean_and_std <- X_test[,mean_and_std]
  X_train_mean_and_std <- X_train[,mean_and_std]
  
  #merge all test and train rows
  subject_all <- rbind(subject_test, subject_train)
  X_all <- rbind(X_test_mean_and_std, X_train_mean_and_std)
  y_all <- rbind(y_test, y_train)
  
  #combine all vectors/data.frames into one data.frame
  mergedf <- cbind(subject_all, y_all, X_all)
  names(mergedf)[1] <- "SubjectID"
  names(mergedf)[2] <- "Activity"
  
  #aggregate by subjectid and activity
  agg <- aggregate(. ~ SubjectID + Activity, data=mergedf, FUN = mean)
  
  #Appropriately labels the data set with descriptive variable names
  agg$Activity <- factor(agg$Activity, labels=activity_labels[,2])
  
  #Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  write.table(agg, file="./tidyData.txt", sep="\t", row.names=FALSE)