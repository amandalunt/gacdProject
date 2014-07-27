

run_analysis <- function(){
	
	
	##fileURL <- 
	##"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	
	##download.file(fileURL, destfile="humanActivityData.zip", method="curl")
	
	##unzip("humanActivityData.zip")	
	testdataLocation<-"./UCI HAR Dataset/test/X_test.txt"
	testlabelLocation<-"./UCI HAR Dataset/test/y_test.txt"
	testsubjectLocation<-"./UCI HAR Dataset/test/subject_test.txt"
	traindataLocation<-"./UCI HAR Dataset/train/X_train.txt"
	trainlabelLocation<-"./UCI HAR Dataset/train/y_train.txt"
	trainsubjectLocation<-"./UCI HAR Dataset/train/subject_train.txt"
	
	activitiesLocation<-"./UCI HAR Dataset/activity_labels.txt"
	
	# meaningful names
	featuresLocation<-"./UCI HAR Dataset/features.txt"
	
	#load separate datasets into R tables
	testD<-read.table(testdataLocation)
	testDlab<-read.table(testlabelLocation)
	testDsubj<-read.table(testsubjectLocation)
	trainD<-read.table(traindataLocation)
	trainDlab<-read.table(trainlabelLocation)
	trainDsubj<-read.table(trainsubjectLocation)
	features<-read.table(featuresLocation)
	activity_names<-read.table(activitiesLocation)
	
	numfeatures<-nrow(features)
	
	# change features to character format and put in variable colnamer
	colnamer<-as.character(features[,2])
	
	# create new character vector of activity names
	activities<-as.character(activity_names[,2])
	
	
	# piece the puzzle together: merge the train and test data
	
	datachunk<-rbind(trainD,testD)
	
	#name the columns
	for(i in 1:numfeatures) {
		
		names(datachunk)[i] <- colnamer[i]
		
	}
	
	#extract only mean and standard deviation measurements for each measurement
	#measurements have 13 entries, (XYZ candidates have 3 in row to 39 entries)
	# mean and std are the first 2 (6) entries
	#tBodyAcc-XYZ 1-40			1:6
	#tGravityAcc-XYZ 41-80 		41:46
	#tBodyAccJerk-XYZ 81-120	81:86
	#tBodyGyro-XYZ 121-160		121:126
	#tBodyGyroJerk-XYZ 161-200	161:166
	#tBodyAccMag 201-213		201:202
	#tGravityAccMag 214-226		214:215
	#tBodyAccJerkMag 227-239	227:228
	#tBodyGyroMag 240-252		240:241
	#tBodyGyroJerkMag 253-265	253:254
	#fBodyAcc-XYZ 266-302 + bandsEnergy 303-344		266:271
	#fBodyAccJerk-XYZ 345-381 + bE 382-423			345:350
	#fBodyGyro-XYZ 424-460 + bE 461-502				424:428
	#fBodyAccMag 503-515							503:504
	#fBodyAccJerkMag 516-528						516:517
	#fBodyGyroMag 529-541							529:530
	#fBodyGyroJerkMag 542-554 plus 7 angle() - tot 561	542:543
	
	mean_std<-datachunk[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:428,503:504,516:517,529:530,542:543)]
	
	# merge the train and test activities
	labelchunk<-rbind(trainDlab,testDlab)
	
	numlabels<-nrow(labelchunk)
	newlabs<-matrix("character", nrow = numlabels, ncol = 1)
	
	# merge the train and test subjects
	subjchunk<-rbind(trainDsubj,testDsubj)
	
	# create new column vector with activity names rather than numbers
	for(i in 1:numlabels){
		
		toreplace<-labelchunk[i,1]
		newlabs[i,1]<-activities[toreplace]
	}
	
	combined<-cbind(mean_std,newlabs)
	combined<-cbind(subjchunk,combined)
	
	# name the newlabs column
	colnames(combined)[colnames(combined)=="newlabs"]<-"Activity"
	colnames(combined)[colnames(combined)=="V1"]<-"Subject"
	
	# group the different measurements according to activity and subject
	comboMelt<-melt(combined,id=c("Activity","Subject"),measure.vars=c(2:66))
	
	# change type of Subject to factor
	comboMelt$Subject<-as.factor(comboMelt$Subject)
	
	# create an independent data set with average of each variable for each activity
	castAvg<-dcast(comboMelt, Activity + Subject ~ variable, mean)
	
	# save new data set as a tab delimited text file
	write.table(castAvg,"./tidy_data_set.txt",sep="\t")
	
		
	
}