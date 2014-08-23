
## The purpose of this script is to generate a tidy data set 
## representing aggregated average values of raw data obtained 
## from the Human Activity Recognition Using Smartphone's Data Set.
## See README.md file for a more detailed description.

## Set working directory (Commented out if using differng directoy struture.)
##setwd("d:\\datascience\\gettingandcleaningdata\\project")

## 1. Merge the training and the test sets to create one data set.

# Start off by loading raw data.

print("Loading X_test.txt")
X_test  <-  read.table("X_test.txt"
                    , header = FALSE, row.names=NULL)

print("Loading X_train.txt")
X_train <-  read.table("X_train.txt"
                    , header = FALSE, row.names=NULL)

print("Loading Y_test.txt")
Y_test  <-  read.table("Y_test.txt"
                    , header = FALSE, row.names=NULL)

print("Loading Y_train.txt")
Y_train <-  read.table("Y_train.txt"
                    , header = FALSE, row.names=NULL)

print("Loading subject_test.txt")
subject_test  <-  read.table("subject_test.txt"
                    , header = FALSE, row.names=NULL)

print("Loading subject_train.txt")
subject_train <-  read.table("subject_train.txt"
                    , header = FALSE, row.names=NULL)

print("Loading features.txt")
features <-  read.table("features.txt"
                             , header = FALSE, row.names=NULL)

print("Loading activity_labels.txt")
activityLabels <-  read.table("activity_labels.txt"
                        , header = FALSE, row.names=NULL)

print("Done loading raw data")

# Combine raw test and training data tables.

print("Processing data")
print("Combine raw data")
allRawData  <-  rbind(X_test,  X_train )
names(allRawData)   <- features$V2

allRawDataY <-  rbind(Y_test,  Y_train )$V1
allRawDataSubject <-  rbind(subject_test,  subject_train )$V1

allRawData$trainingLabels  <-  allRawDataY 
allRawData$subject  <-  allRawDataSubject 


## 2. Extract only the measurements on the mean and standard deviation for 
## each measurement.
print("extracting data")

# Find columns with mean or std in name (or trainingLables).
columnNameSearchStrings <- c("Mean", "mean", "std"
                             , "trainingLabels", "subject")

matchingNames <-  logical(length = 0)
matchingNames[1:length(names(allRawData))] <- FALSE
for(i in columnNameSearchStrings){
  matchingNames <- matchingNames | grepl(i,names(allRawData), perl=TRUE)
}

# Subset only columns with mean or std in name (or trainingLables).
allRawData  <- allRawData[,matchingNames]  


## 3. Use descriptive activity names to name the activities in the data set
print("Use descriptive activity names")
allRawData$trainingLabels <-  activityLabels[allRawData$trainingLabels,2]


## 4. Appropriately label the data set with descriptive variable names. 
print("Make descriptive variables more appropriately labeled")
allNames  <-  names(allRawData)  
for(i in 1:2){
    allNames <- sub("(", "", allNames, ignore.case =FALSE, fixed=TRUE)
    allNames <- sub(")", "", allNames, ignore.case =FALSE, fixed=TRUE)
    allNames <- sub(",", "", allNames, ignore.case =FALSE, fixed=TRUE)
    allNames <- sub("-", "", allNames, ignore.case =FALSE, fixed=TRUE)
}
allNames <- sub("mean", "Mean", allNames, ignore.case =FALSE, fixed=TRUE)
allNames <- sub("std", "Std", allNames, ignore.case =FALSE, fixed=TRUE)
allNames <- sub("gravity", "Gravity", allNames, ignore.case =FALSE, fixed=TRUE)

for(i in 1:length(names(allRawData))){
  if(substr(allNames[i],1,1)=="t" & substr(allNames[i],1,5)!="train"){
    allNames[i] <-(paste("time"
                    ,(substr(allNames[i],start=2,stop=nchar(allNames[i])))
                    , sep = ""))
  }   
  else if(substr(allNames[i],1,1)=="f"){
    allNames[i] <-(paste("frequency"
                         ,(substr(allNames[i],start=2,stop=nchar(allNames[i])))
                         , sep = ""))
  }     
}

names(allRawData)  <-  allNames


## 5. Create a second, independent tidy data set with the average of each 
## variable for each activity and each subject. 


SecondTidyDataSet <- aggregate(allRawData[,1:1:(length(allRawData)-2)]
                 , list(subject=allRawData$subject ,trainingLabels=allRawData$trainingLabels)
                 , mean)

# Can be used generate a small subset for testing and easier viewing  
# SecondTidyDataSet <- aggregate(allRawData[,4:6]
#                 , list(subject=allRawData$subject ,trainingLabels=allRawData$trainingLabels)
#                 , mean)

## Store the tidy data set in a file.
print("Storing Step 5 tidy data (this is the final data set)")
write.table(SecondTidyDataSet, "tidydata.txt", sep="\t", row.name=FALSE) 

print("Done")
