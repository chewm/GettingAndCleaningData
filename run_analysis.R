## This code run an analysis on a datasets provided in 
## a zip file(Default file name=HARDataset.zip on current 
## directory). It will merge the two datasets(Test & Train)
## into one dataset. From the merged datasets, it provide
## the average of the selected measurements group by both
## subject & activity.
run_analysis <- function(f="HARDataset.zip") {
  ## check if the file exists
  if(!file.exists(f)) {stop("invalid file")}
  
  ## assigned directory and files references to each
  ## data sets in different files
  xTrSet <- "UCI HAR Dataset/train/X_train.txt"
  yTrLbl <- "UCI HAR Dataset/train/y_train.txt"
  TrSubj <- "UCI HAR Dataset/train/subject_train.txt"
  xTtSet <- "UCI HAR Dataset/test/X_test.txt"
  yTtLbl <- "UCI HAR Dataset/test/y_test.txt"
  TtSubj <- "UCI HAR Dataset/test/subject_test.txt"
  feature_file <- "UCI HAR Dataset/features.txt"
  act_file <- "UCI HAR Dataset/activity_labels.txt"
  
  ## Reading train data sets
  train_set <- read.table(unz(f,xTrSet))
  train_lbl <- read.table(unz(f,yTrLbl))
  train_subj <- read.table(unz(f,TrSubj))
  ## Merge train sets by columns
  train_dt <- cbind(train_subj,train_lbl,train_set)

  ## Reading test data sets
  test_set <- read.table(unz(f,xTtSet))
  test_lbl <- read.table(unz(f,yTtLbl))
  test_subj <- read.table(unz(f,TtSubj))    
  ## Merge test sets by columns
  test_dt <- cbind(test_subj,test_lbl,test_set)
  
  ## Merging both test & training datasets by rows
  dt <- rbind(train_dt,test_dt)

  ## Reading features file to retrieve variables name
  ## for measurement columns
  ftr <- read.table(unz(f,feature_file),colClasses="character")
  
  ## Naming all columns of merged datasets
  names(dt) <- c("subject","activity",ftr[,2])

  
  idx <- c(grep("mean()",names(dt),fixed=TRUE), grep("std()",names(dt),fixed=TRUE))
  idx <- sort(idx)
  meanstd <- dt[,c(1,2,idx)]
  
  ## Uses descriptive activity names to name the activities in the data set
  act <- read.table(unz(f,act_file),colClasses="character")
  ## assign activity as factor with activity labels from file
  meanstd$activity <- factor(meanstd$activity,levels=act[,1],labels=act[,2])
  ## make subject column as factor
  meanstd$subject <- as.factor(meanstd$subject)

  ## summarize the datasets(average on every measurement column) group by
  ## both activity & subject
  tidy_data <- group_by(meanstd, activity, subject) %>% summarise_each(funs(mean))
  
  ## write tidy data results to a file called tidydata.txt
  write.table(tidy_data,file="tidydata.txt",row.names=FALSE)
  
  ## print out the tidy_data on the screen
  print(tidy_data)
}