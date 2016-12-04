#### run_analysis.R

#### 	1.	Merges the training and the test sets to create one data set.
####     =================================================

### ------------------------------------------
### READING DATA FROM FILES
### ------------------------------------------

# Read data from 'test' folder
subject_test <- read.table("test/subject_test.txt")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

# Read data from 'train' folder
subject_train <- read.table("train/subject_train.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

## Read 'features'
features <- read.table("features.txt")
## Read 'activity labels'
activity_labels <- read.table("activity_labels.txt")

### ---------------------------------------------------
### ASSIGN COLUMN NAMES TO DATA
### ---------------------------------------------------

## Assign column names for 'test' data
colnames(X_test) <- features[,2]
colnames(y_test) <- "ActivityID"
colnames(subject_test) <- "SubjectID"

## Assign column names for 'train' data
colnames(X_train) <- features[,2]
colnames(y_train) <- "ActivityID"
colnames(subject_train) <- "SubjectID"

## Assign column names to 'activity_labels' data
colnames(activity_labels) <- c('ActivityID', 'ActivityType')

### ------------------------------------------
### MERGE DATA INTO ONE SET
### ------------------------------------------

# Merge 'train' data
train_data <- cbind(y_train, subject_train, X_train)
# Merge 'test' data
test_data <- cbind(y_test, subject_test, X_test)
# Merge train_data and test_data
combined_data <- rbind(train_data, test_data)


#### 	2.	Extracts only the measurements on the mean and standard deviation for each measurement.
####     ==========================================================================

### -----------------------------------------------------
### READ COLNAMES OF combined_data
### -----------------------------------------------------

colNames <- colnames(combined_data)

### ------------------------------------------------------------------------------------
### CREATE VECTOR FOR ID / MEAN / STANDARD DEVIATION
### ------------------------------------------------------------------------------------

mean_standard_dev <- (grepl("ActivityID", colNames) | 
						grepl("SubjectID", colNames) |
						grepl("mean...", colNames) |
						grepl("std....", colNames))

mean_standard_data <- combined_data[, mean_standard_dev == TRUE]

####   3.	Uses descriptive activity names to name the activities in the data set
####   ========================================================

data_activity_names <- merge(mean_standard_data, activity_labels, by = 'ActivityID', all.x = TRUE)

#### 	4.	Appropriately labels the data set with descriptive variable names.
####  ====================================================

# Get column names
colNamesFinal <- colnames(data_activity_names)

# Replace column names with appropriate labels
for (i in 1:length(colNamesFinal)){
	
	colNamesFinal[i] = gsub("\\()", "", colNamesFinal[i])
	colNamesFinal[i] = gsub("-std$", "StdDev", colNamesFinal[i])
	colNamesFinal[i] = gsub("-mean", "Mean", colNamesFinal[i])
	colNamesFinal[i] = gsub("^(t)", "time", colNamesFinal[i])
	colNamesFinal[i] = gsub("^(f)", "freq", colNamesFinal[i])
	colNamesFinal[i] = gsub("([Gg]ravity)", "Gravity", colNamesFinal[i])
	colNamesFinal[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", colNamesFinal[i])
	colNamesFinal[i] = gsub("[Gg]yro", "Gryo", colNamesFinal[i])
	colNamesFinal[i] = gsub("AccMag", "AccMagnitude", colNamesFinal[i])
	colNamesFinal[i] = gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", colNamesFinal[i])
	colNamesFinal[i] = gsub("JerkMag", "JerkMagnitude", colNamesFinal[i])
	colNamesFinal[i] = gsub("GyroMag", "GyroMagnitude", colNamesFinal[i])
	
	}

# Assign new column names back to data	
colnames(data_activity_names) = colNamesFinal
	
#### 	5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
####   =============================================================================================================

tidy_data_set <- aggregate(. ~SubjectID + ActivityID, data_activity_names, mean)
tidy_data_set <- tidy_data_set[order(tidy_data_set$SubjectID, tidy_data_set$ActivityID), ]

#### ---------------------------------------------
#### WRITE tidy_data_set TO tidy_data_set.txt FILE
#### ---------------------------------------------

write.table(tidy_data_set, "tidy_data_set.txt", row.name = FALSE)
