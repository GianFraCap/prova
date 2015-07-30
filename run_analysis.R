run_analysis <- function(URL) {
        #"URL" URL pointing the training and test sets
        setInternet2(use = TRUE)
        if(!file.exists("./data")){dir.create("./data")}
        fileUrl <- URL
        download.file(fileUrl,destfile="./data/Dataset.zip")
        unzip("./data/Dataset.zip",exdir = "./data")        	#fare unzip
        setwd("./data/UCI HAR Dataset")
        library(dplyr)
        library(reshape2)
        #
        subject_test <- read.table("./test/subject_test.txt", header=FALSE)
        subject_train <- read.table("./train/subject_train.txt", header=FALSE)
        #
        X_test <- read.table("./test/X_test.txt", header=FALSE)			# > dim(X_test) --> [1] 2947  561	quindi 2947 righe e 561 colonne
        y_test <- read.table("./test/y_test.txt", header=FALSE)			# > dim(y_test) --> [1] 2947    1
        #
        X_train <- read.table("./train/X_train.txt", header=FALSE)		# > dim(X_train) --> [1] 7352  561
        y_train <- read.table("./train/y_train.txt", header=FALSE)		# > dim(y_train) --> [1] 7352    1
        mergedTrain <- cbind(y_train,X_train)
        mergedTest <- cbind(y_test,X_test)
        # Il file "feature.txt" mostra che delle 561 colonne solo alcune trattano di media e deviazione standard (std). 
        # N.B. dal file "feature_info.txt" si capisce che le 561 osservazioni sono il frutto di 17 variabili stimate per i 33 segnali (-XYZ sta per segnali X, Y, Z)
        mergedDataset <- rbind(mergedTrain,mergedTest)		#Dataset finale. Qui finisce lo Step 1
        ####
        #> head(mergedDataset[,1:5])
        # V1      V1.1          V2         V3         V4
        #1  5 0.2885845 -0.02029417 -0.1329051 -0.9952786
        #2  5 0.2784188 -0.01641057 -0.1235202 -0.9982453
        ####
        features <- read.table("./features.txt", header=FALSE)	#contiene l'elenco delle variabili del Dataset completo, di cui prendere il subset di colonne con mean() e std()
        
        firstr <- data.frame("V1"="N","V2"="Act")	# creo un DF di una sola riga e due colonne per usare la seconda colonna da unire con rbind a feat_list. Il warning non è un problema
        Z <- rbind(firstr,features)
        feat_list <- Z[,2]		#seleziono la sola colonna del DF features contenente le variabili misurate e la colonna aggiunta "Act" per farne i nomi delle colonne del "mergedDataset"
        #	ora stabilisco quali colonne del mergedDataset contengono il termine mean() e std()
        nmean <- grep("mean()",feat_list,fixed=FALSE)
        nstd <- grep("std()",feat_list,fixed=FALSE)
        #### motivare l'esclusione nel codebook come l'indicazione nel forum "https://class.coursera.org/getdata-030/forum/thread?thread_id=86#comment-244"
        subsetcol <- append(nmean,nstd)
        subsetsortcol <- sort(subsetcol)
        ###
        #> sort(subsetcol)
        #[1]   2   3   4   5   6   7  42  43  44  45  46  47  82  83  84  85  86  87 122 123 124
        #[22] 125 126 127 162 163 164 165 166 167 202 203 215 216 228 229 241 242 254 255 267 268
        #[43] 269 270 2
        ###
        subsetsortcol <- append(1,subsetsortcol)	#aggiungo "1" per includere anche la colonna di activity
        mergedsubset <- mergedDataset[,subsetsortcol]		# Qui finisce lo Step 2
        #
        Activitylab <- read.table("./activity_labels.txt", header=FALSE)
        joindtable <- inner_join(mergedsubset,Activitylab,by = "V1")		# Creo l'ultima colonna con le Activity Label --> occorrerà spostare questa come prima ed eliminare l'attuale prima
        # Qui finisce lo Step 3
        VarNames <- Z[subsetsortcol,2]	#subset della tab col nome delle variabili, ma manca l'ultima (activity name)
        names(joindtable2) <- Z[subsetsortcol,2]
        joindtable[,1] <- joindtable[,81]
        # "fBodyBody" named columns are assumed to be Typos and are excluded by the final DF
        Joindtfinal <- joindtable[,1:71]		
        # Qui finisce la prima parte dello Step 4. Eliminiamo le ultime colonne perchè assumiamo siano typos
        # quelle contenenti "fBodyBody"
        #
        # Terminiamo lo Step 4 dando descrittività ai nomi delle colonne 
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
        # End - Step 4
        DFStep5melt <- melt(Joindtfinal,id=c("Activity Name"))
        DFStep5meltDcast <- dcast(DFStep5melt, "Activity Name" ~ variable,mean)
        # End - Step 5
}