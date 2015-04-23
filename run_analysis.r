#read in the three text files from the training data
subject_train <- read.table("data\\train\\subject_train.txt")
X_train <- read.table("data\\train\\X_train.txt")
y_train <-read.table("data\\train\\y_train.txt")

#read in the three text files from the test data
subject_test <- read.table("data\\test\\subject_test.txt")
X_test <- read.table("data\\test\\X_test.txt")
y_test <-read.table("data\\test\\y_test.txt")

#get variable / column names
variables <- c(as.character(read.table("data\\features.txt")[,2]))
variables <- gsub("-", "_", variables)
variables <- gsub("\\(\\)", "", variables)
variables <- gsub("\\(", "", variables)
variables <- gsub("\\)", "", variables)
variables <- gsub("BodyBody", "Body", variables)

#merge test + training data
subjects <- rbind(subject_test, subject_train)
X <- rbind(X_test, X_train)
y <- rbind(y_test, y_train)


#fill full dataset
full_data_set <- X
full_data_set <- cbind(full_data_set, y)
full_data_set <- cbind(full_data_set, subjects)

#change column names
colnames(full_data_set) <- c(variables, "activity", "subjects")

#get columns where mean() and std() are occurring in the names
meanstdcols <- c(grep("mean|std", colnames(full_data_set)))

#get subset with above columns
full_data_set_meanstd <- full_data_set[,c(meanstdcols, 562, 563)]
full_data_set_meanstd$activity <- as.character(full_data_set_meanstd$activity)

#transform function to transform activity to sensible name
transformActivity <- function(x) {
  if (x == "1") {
    x <- "WALKING" 
    
  }  
  if (x == "2") {
    x <- "WALKING_UPSTAIRS"
    
  }
  if (x == "3") {
    x <- "WALKING_DOWNSTAIRS"    
    
  }  
  if (x == "4") {
    x <- "SITTING"    
    
  }
  if (x == "5") {
    x <- "STANDING"    
    
  }  
  if (x == "6") {
    x <- "LAYING"    
    
  }
  x
}

full_data_set_meanstd$activity <- sapply(full_data_set_meanstd[,"activity"], transformActivity)
full_data_set_meanstd$activity <- as.factor(full_data_set_meanstd$activity)

#create new dataset 

library(dplyr)

newtable <- data.frame()
for(x in 1:6) {
  temp <- subset(full_data_set_meanstd, as.numeric(activity) == x)
  for (y in meanstdcols) {
    newtable <- rbind(newtable, data.frame(variable =variables[y], mean = mean(temp[,variables[y]]), activity= temp[1, "activity"], subject = NA  ))
  }  
}

for(x in 1:30) {
  temp <- subset(full_data_set_meanstd, subjects == x)
  for (y in meanstdcols) {
    newtable <- rbind(newtable, data.frame(variable =variables[y], mean = mean(temp[,variables[y]]), subject= temp[1, "subjects"], activity =NA ))
  }  
}

write.table(newtable, "tidydataset.txt", row.name=FALSE, )





