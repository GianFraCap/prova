Creation of Script "run_analysis.R"
#INPUT: "URL" URL pointing the training and test sets
STEP 1:
A) Download files for analysis and load appropriate libraries
        setInternet2(use = TRUE)
        if(!file.exists("./data")){dir.create("./data")}
        fileUrl <- URL
        download.file(fileUrl,destfile="./data/Dataset.zip")
        unzip("./data/Dataset.zip",exdir = "./data")		
        setwd("./data/UCI HAR Dataset")
		library(dplyr)
		library(reshape2)
B) Prepare the table files from test and training	and merge them (subject, X and y)	#
		subject_test <- read.table("./test/subject_test.txt", header=FALSE)
        subject_train <- read.table("./train/subject_train.txt", header=FALSE)
		#
		X_test <- read.table("./test/X_test.txt", header=FALSE)			# > dim(X_test) --> [1] 2947  561	
		y_test <- read.table("./test/y_test.txt", header=FALSE)			# > dim(y_test) --> [1] 2947    1
		#
		X_train <- read.table("./train/X_train.txt", header=FALSE)		# > dim(X_train) --> [1] 7352  561
		y_train <- read.table("./train/y_train.txt", header=FALSE)		# > dim(y_train) --> [1] 7352    1
		mergedTrain <- cbind(y_train,X_train)
		mergedTest <- cbind(y_test,X_test)
C)  "feature.txt" file shows 561 columns. Only the one referring to as mean and standard deviation (std) will be kept for the future analysis
		mergedDataset <- rbind(mergedTrain,mergedTest)
		####  END OF STEP 1 ###
STEP 2
		#> head(mergedDataset[,1:5])
		 # V1      V1.1          V2         V3         V4
		#1  5 0.2885845 -0.02029417 -0.1329051 -0.9952786
		#2  5 0.2784188 -0.01641057 -0.1235202 -0.9982453
		####
A)	Prepare for the subset selection starting from the features list
		features <- read.table("./features.txt", header=FALSE)	
		firstr <- data.frame("V1"="N","V2"="Act")	# Create a single-row DataFrame (DF), the 2nd column will be used for rbind with features list --> Z
		Z <- rbind(firstr,features)
		feat_list <- Z[,2]		
B)	Select mean and std variables
		nmean <- grep("mean()",feat_list,fixed=FALSE)
		nstd <- grep("std()",feat_list,fixed=FALSE)
		subsetcol <- append(nmean,nstd)
		subsetsortcol <- sort(subsetcol)
		###
	Thus obtaining the columns of interest
	#> sort(subsetcol)
		#[1]   2   3   4   5   6   7  42  43  44  45  46  47  82  83  84  85  86  87 122 123 124
		#[22] 125 126 127 162 163 164 165 166 167 202 203 215 216 228 229 241 242 254 255 267 268
		#[43] 269 270 2
		###
C)	Append "1" to the subset of indexes to take into account the "Activity" for Step 3
		subsetsortcol <- append(1,subsetsortcol)	
	Then obtaining the subset DF (mergedsubset)
	mergedsubset <- mergedDataset[,subsetsortcol]	
		####  END OF STEP 2 ###

STEP 3
A)	Join the Activity labels to "mergedDataset" to produce "joindtable"
		Activitylab <- read.table("./activity_labels.txt", header=FALSE)
		joindtable <- inner_join(mergedsubset,Activitylab,by = "V1")		# Creo l'ultima colonna con le Activity Label --> occorrerà spostare questa come prima ed eliminare l'attuale prima
		####  END OF STEP 3 ###

STEP 4
A)	Initially labelling with features exact names to produce "Joindtfinal"
		VarNames <- Z[subsetsortcol,2]	
		names(joindtable2) <- Z[subsetsortcol,2]
		joindtable[,1] <- joindtable[,81]
B)	Examining last columns I assume "fBodyBody"-like columns are typos and are removed
		Joindtfinal <- joindtable[,1:71]		
		#####
		#> head(Joindtfinal[,1:5])
		#	   Act tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X
		#1 STANDING         0.2885845       -0.02029417        -0.1329051       -0.9952786
		#2 STANDING         0.2784188       -0.01641057        -0.1235202       -0.9982453
		#3 STANDING         0.2796531       -0.01946716        -0.1134617       -0.9953796
		#####
C) 	Create descriptive features name, es:
		names(Joindtfinal) <- gsub("Act","Activity Name",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyAcc-","Time Domain Linear Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("fBodyAcc-","Frequency Domain Linear Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tGravityAcc-","Time Domain Gravity Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyGyro-","Time Domain Angular Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyAccJerk-","Time Domain Jerk Linear Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyGyroJerk-","Time Domain Jerk Angular Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyGyroMag-","Time Domain Angular Acceleration Magnitude- ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("tBodyGyroJerkMag-","Time Domain Jerk Angular Acceleration Magnitude- ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("fBodyAccJerk-","Frequency Domain Jerk Linear Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("fBodyGyro-","Frequency Domain Angular Acceleration - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("Time Domain Jerk Linear Acceleration Magnitude - ", "Time Domain Jerk Linear Acceleration -",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		names(Joindtfinal) <- gsub("fBodyAccMag-","Frequency Domain Linear Acceleration Magnitude - ",names(Joindtfinal),ignore.case=FALSE,fixed=TRUE)
		# Qui termina definitivamente lo Step 4
D)	!!!! Realize I forgot to "rbind" subject tables from test and train. Now do that (Possible because no rows re-arrangiaments are done)

		mergedsubject <- rbind(subject_train,subject_test)		
		names(mergedsubject) <- c("SubjectID")
		#
		DFStep5bis <- cbind(mergedsubject,Joindtfinal)
		####  END OF STEP 4 ###
STEP 5
		DFStep5meltbis <- melt(DFStep5bis,id=c("Activity Name","SubjectID"))
		#
		DFStep5melt <- melt(Joindtfinal,id=c("Activity Name"))
		DFStep5meltDcast <- dcast(DFStep5melt, "Activity Name" ~ variable,mean)		# questo produce 1 sola riga con la media per ciascuna colonna: ERRORE
		####  END OF STEP 5 ###	
	
		#ALTERNATIVA NON RISOLUTIVA PENSATA POST PROGETTO
		DFmelt <- melt(DFStep5bis,id=c("SubjectID","Activity Name"))
		subjdcast <- dcast(DFmelt, SubjectID ~ variable,mean)		# questo produce una riga per ogni subject (quindi 30), ma spariscono le "Acitivity"
		subjdcast <- dcast(DFmelt, SubjectID + "Activity Name"~ variable,mean) # questo produce una riga per ogni subject (quindi 30), le "Acitivity" sono NA
		
	