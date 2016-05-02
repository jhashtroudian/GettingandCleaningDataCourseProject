#!/usr/bin/R
#run_analysis.R
# A script that
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# a utility function from R help chartr
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#make sure all needed packages are installed (httr, plyr)
run_analysis <- function() {
	# do the requires
	require(httr)
	require(plyr)
	require(tidyr)

	# print("testing run_analysis.R") #making sure sourcing runs this script


	# Download and unzip https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
	# into temppath
	temppath <- "c:/rtemp"
	setwd(temppath)


	# check for existence of the necessary iles
	if (!file.exists("FUC_HAR.zip")) {
		print("Zip file not downloaded yet")
		download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "FUC_HAR.zip")
		# get rid of the spaces and shorten filename
	}
	if (!file.exists("FUC_HAR.zip")) {
		# we could not download so abort
		print("we could not download so abort")
		return(-1)
	}
	# zip file exists - has it been unzipped?
	if (!file.exists("UCI HAR Dataset")) {
		unzip("FUC_HAR.zip")
		print("completed unzipping")
	}
	if (!file.exists("UCI HAR Dataset")) {
		# we could not unzip so abort
		print("we could not unzip so abort")
		return(-1)
	}
	
	#print("testing run_analysis.R file downloaded and unzipped")
	# file downloaded and unzipped
	
	# now  read the files in first the lables
	featureNames <- read.table("./UCI HAR Dataset/features.txt")
	activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
	# and next the actual activities for test and training
	testActivites <- read.table("./UCI HAR Dataset/test/y_test.txt")
	trainingActivites <- read.table("./UCI HAR Dataset/train/y_train.txt")
	# merge activites
	totActivities <- rbind(testActivites, trainingActivites)
	# note all merges hae to be in same order (test then training)
	# clen up unneeded data
	rm(testActivites, trainingActivites)
	
	#read subject information
	subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
	subjectTraining <- read.table("./UCI HAR Dataset/train/subject_train.txt")
	totSubjectData <- rbind(subjectTest,subjectTraining)
	
	# clen up unneeded data
	rm(subjectTest, subjectTraining)
	
	# add header lable for subjects
	names(totSubjectData) <- "Subjects"

	
	# next the data files training and test
	trainingData <- read.table("./UCI HAR Dataset/train/X_train.txt")
	testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
	
	# merge the test and training data sets to create one data set
	totData <- rbind(testData, trainingData)
	# 1. totOnlyMeanSd <- totData[,meanSdPos]
	
	
	totDataTab <- tbl_df(totData)
	
	# clen up unneeded data
	rm(testData, trainingData)
	
	# clean feature names by removing "()"
	featureNames[,2] <- gsub("\\(|\\)", "", featureNames[,2])
	# and replacing the - with space
	featureNames[,2] <- gsub("-", " ", featureNames[,2])
	# add lables to merged data
	names(totData) = featureNames[, 2]
	
	#keep only the mean and standard deviation
	# first grep on mean and std
	meanSdPos <- grep("mean|std", featureNames[, 2])
	totOnlyMeanSd <- totData[,meanSdPos]
	# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
	
	# now use descriptive activity names to name the activites in the data set
	activities[,2] <- gsub("_", " ", activities[,2])
	activities[,2] <- capwords(tolower(activities[,2]))
	
	# change activities list from numbers to lables
	totActivities[,1] = activities[totActivities[,1], 2]
	
	names(totActivities) <- "Activity"
	# 3. Uses descriptive activity names to name the activities in the data set
	
	# now merge the columns to get oe tidy file:
	finalTidyMerged <- cbind(totSubjectData, totActivities, totOnlyMeanSd)
	# 4. Appropriately labels the data set with descriptive variable names.
	
	# now do a summary table of means of data per person peractivity
	finalPersonActivitySummaryTable <- ddply(finalTidyMerged, .(Subjects, Activity), numcolwise(mean))
	# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	
	#finally write the results to disk as tables
	write.table(finalTidyMerged, "final_.tTidy_merged.txt", row.name=FALSE)
	# Note this intermediate file is not needed for the assignment.
	write.table(finalPersonActivitySummaryTable, "final_runPersonActivity_summary_table.txt", row.name=FALSE)
	
	# ultimately c;ean the whole environment
	rm(list = ls())
	

}


