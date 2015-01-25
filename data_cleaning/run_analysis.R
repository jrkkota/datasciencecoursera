##Merges the training and the test sets to create one data set.
##Extracts only the measurements on the mean and standard deviation for each measurement. 
##Uses descriptive activity names to name the activities in the data set
##Appropriately labels the data set with descriptive variable names. 
##From the data set in step 4, creates a second, independent tidy data set with 
##the average of each variable for each activity and each subject.

if(require("data.table")){
  print("data.table is available and ready to use...")
} else {
  print("data.table package is unavailable and trying to install data.table...")
  install.packages("data.table")
  if(require(data.table)){
    print("data.table is installed and loaded...")
  } else {
    stop("Unable to install data.table...")
  }
}

if(require("reshape2")){
  print("reshape2 is available and ready to use...")
} else {
  print("reshape2 package is unavailable and trying to install reshape2...")
  install.packages("reshape2")
  if(require(reshape)){
    print("reshape2 is installed and loaded...")
  } else {
    stop("Unable to install reshape2...")
  }
}
require("reshape2")


loadDataSets <- function() {
  #load the dataSets from test and train directories
  #Load three txt files namely subject_test.txt, X_test.txt and 
  #Y_test.txt from the test and train directories
  
  # Load the activity labels
  activity_labels <- read.table("./UCIHARDataset/activity_labels.txt")[,2]
  
  # Load the data column names
  features <- read.table("./UCIHARDataset/features.txt")[,2]
  # the measurements on the mean and standard deviation for each measurement.
  mean_std_features <- grepl("mean|std", features)
  
  subjectTest <- read.table("./UCIHARDataset/test/subject_test.txt")
  
  # Load and process X_test & y_test data.
  X_test <- read.table("./UCIHARDataset/test/X_test.txt")
  y_test <- read.table("./UCIHARDataset/test/y_test.txt")
  names(X_test) = features
  
  # Extract only the measurements on the mean and standard deviation for each measurement.
  X_test = X_test[,mean_std_features]
  
  # Load activity labels
  y_test[,2] = activity_labels[y_test[,1]]
  names(y_test) = c("Activity_ID", "Activity_Label")
  names(subjectTest) = "subject"
  
  # Use cbind to bind the data
  test_data <- cbind(as.data.table(subjectTest), y_test, X_test)
 
  # Load and process X_train & y_train data.
  X_train <- read.table("./UCIHARDataset/train/X_train.txt")
  y_train <- read.table("./UCIHARDataset/train/y_train.txt")
  
  subjectTrain <- read.table("./UCIHARDataset/train/subject_train.txt")
  
  names(X_train) = features
  
  # Extract only the measurements on the mean and standard deviation for each measurement.
  X_train = X_train[,mean_std_features]
  
  # Load activity data
  y_train[,2] = activity_labels[y_train[,1]]
  names(y_train) = c("Activity_ID", "Activity_Label")
  names(subjectTrain) = "subject"
  
  # Bind data
  train_data <- cbind(as.data.table(subjectTrain), y_train, X_train)
  
  # Merge test and train data
  data = rbind(test_data, train_data)
  
  id_labels   = c("subject", "Activity_ID", "Activity_Label")
  data_labels = setdiff(colnames(data), id_labels)
  melt_data      = melt(data, id = id_labels, measure.vars = data_labels)
  
  # Apply mean function to dataset using dcast function
  tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)
  
  write.table(tidy_data, file = "./tidyData.txt", row.names = FALSE)
  
}