# This will take in multiple data sets and create a tidy dataset.
# It will:
#  1) Merges the training and the test sets to create one data set.
#  2) Extracts only the measurements on the mean and standard deviation for each measurement.
#     (where label like mean or std)
#  3) Use descriptive activity names to name the activities in the data set
#  4) Create the "tidy" data set with the average of each variable for each activity 
#     and each subject. 

readDataSets <- function(path_name, data_type) {

    #Read y File
    filename <- paste(path_name,"/",data_type,"/y_", data_type, ".txt",sep="");
    y_data <- read.table(filename, header=F, col.names=c("ActivityID"))
    
	  #Read Subject File
	  filename <- paste(path_name,"/",data_type,"/subject_", data_type, ".txt",sep="");
    subject_data <- read.table(filename, header=F, col.names=c("SubjectID"))
    
    # read the column names, assuming features.txt is in the working directory
    data_cols <- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
    
    # read the X data file
    filename <- paste(path_name,"/",data_type,"/X_", data_type, ".txt",sep="")
    X_data <- read.table(filename, header=F, col.names=data_cols$MeasureName)
    
    # names of subset columns required
    subset_data_cols <- grep(".*mean\\(\\)|.*std\\(\\)", data_cols$MeasureName)
    
    # subset the X_data since it is large
    data <- X_data[,subset_data_cols]
    
    # append the activity id and subject id columns
    data$ActivityID <- y_data$ActivityID
    data$SubjectID <- subject_data$SubjectID

    # return the data
    return (data)
    
}

# Read the test data
readTestData <- function(path) {
    readDataSets(path, "test")
}

# Read the train data
readTrainData <- function(path) {
    readDataSets(path, "train")
}

# Merge both train and test data sets
# Cleanup column names
mergeData <- function(path) {
    data <- rbind(readTestData(path), readTrainData(path))
    cnames <- colnames(data)
    cnames <- gsub("\\.+mean\\.+", cnames, replacement="Mean")
    cnames <- gsub("\\.+std\\.+",  cnames, replacement="Std")
    colnames(data) <- cnames
    return (data)
}

# Add the activity names as another column
applyActivityLabel <- function(data) {
    #assume activity_labels is in the working directory
	  activity_labels <- read.table("activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
    activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
    labeledData <- merge(data, activity_labels)
    return (labeledData)
}

# Combine training and test data sets and add the activity label as another column
getMergedLabeledData <- function(path) {
    applyActivityLabel(mergeData(path))
}

# Create a tidy data set that has the average of each variable for each activity and each subject.
getTidyData <- function(merged_labeled_data) {
    library(reshape2)
    
    # melt the dataset
    id_vars = c("ActivityID", "ActivityName", "SubjectID")
    measure_vars = setdiff(colnames(merged_labeled_data), id_vars)
    melted_data <- melt(merged_labeled_data, id=id_vars, measure.vars=measure_vars)
    
    # recast 
    dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

# Create the tidy data set and save it on to the named file
createTidyDataFile <- function(path, output) {
    setwd(path)
    tidy_data <- getTidyData(getMergedLabeledData(path))
    write.table(tidy_data, output)
}

print("Creating tidy dataset as tidy.txt...")
createTidyDataFile("/home/sully/r/data/UCI\ HAR\ Dataset","tidy_output.txt")
print("Done.")
