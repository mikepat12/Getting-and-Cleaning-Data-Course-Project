# Assignment: Getting and Cleaning Data Course Project
library(data.table)
library(tidyverse)
library(reshape2)

# Reading in the data
setwd("~/Downloads/UCI HAR Dataset")
    # First are the activity labels and features. Only care for mean and standard deviation. Get rid of everything else
activitylabels <- fread("activity_labels.txt")
colnames(activitylabels) <- c("Category Label", "Activity Name")

features <- fread("features.txt")
colnames(features) <- c("Index", "Features")    # Included column names    
?grep
featuresMeanSTD <- grep("(mean|std)\\(\\)", features[, Features])   # only care for mean and std. Need to get parentheses after those words too
MeanSTD <- features[featuresMeanSTD, Features]
MeanSTD <- gsub('[()]', '', MeanSTD)    # Sub out parentheses in MeanSTD for no space. 


    # training set
setwd("~/Downloads/UCI HAR Dataset/train")
subject_train <- fread("subject_train.txt")
colnames(subject_train) <- c("SubjectID")

x_trainset <- fread("UCI HAR Dataset/train/X_train.txt")[, featuresMeanSTD, with = F] # Allows us to filter w/featuresMeanSTD
data.table::setnames(x_trainset, colnames(x_trainset), MeanSTD) # Apply column names from MeanSTD. Dataset was already filtered above and this allowed me to correctly label
head(x_trainset)
dim(x_trainset)

y_trainset <- fread("y_train.txt")   # These are the test labels
colnames(y_trainset) <- c("Label")   # Change column name
head(y_trainset)

Trainingset <- cbind(subject_train, y_trainset, x_trainset)    # using cbind to bind columns into 1 dataset
head(Trainingset)
dim(Trainingset)


    # test set
setwd("~/Downloads/UCI HAR Dataset/test")
subject_test <- fread("subject_test.txt")
colnames(subject_test) <- c("SubjectID")

x_testset <- fread("X_test.txt")[, featuresMeanSTD, with = F] # Allows us to filter w/featuresMeanSTD
data.table::setnames(x_testset, colnames(x_testset), MeanSTD) # Apply column names from MeanSTD. Dataset was already filtered above and this allowed me to correctly label

y_testset <- fread("y_test.txt")
colnames(y_testset) <- c("Label")

Testset <- cbind(subject_test, y_testset, x_testset)    # using cbind to bind columns into 1 dataset

# Now we will merge the training and testing datasets
Mergeset <- merge(Trainingset, Testset, by.x = "SubjectID", by.y = "SubjectID", all = T)
dim(Mergeset)
head(Mergeset)

intersect(names(Trainingset), names(Testset))   # Removing duplicate columns
mergeset2 = merge(Trainingset, Testset, all = T)
dim(mergeset2)
str(mergeset2)

# Appropriately labeling the dataset with descriptive variable names
mergeset2[["Activity"]] <- factor(mergeset2[, Label]
                                 , levels = activitylabels[["Category Label"]]
                                 , labels = activitylabels[["Activity Name"]]) 
# Taking label variables from both sets and applying as a factor variable. Convert category label to activity name. Store in activity
# Then take subject ID and change to factor variable
mergeset2[["SubjectID"]] <- as.factor(mergeset2[, SubjectID])


# Melt dataframe to only include id and activity
mergeset2 <- reshape2::melt(data = mergeset2, id = c("SubjectID", "Activity"))
str(mergeset2)
head(mergeset2)

# Now use dcast to summarize by measure. Able to get mean for each activity based on subject id
?dcast
mergeset2 <- reshape2::dcast(data = mergeset2, SubjectID + Activity ~ variable, fun.aggregate = mean)



# Independent tidy data set with the average of each variable for each activity and each subject. Save as txt or csv
setwd("~/Downloads")
write.csv(mergeset2, "tidydata.csv")
write.table(mergeset2, "tidydata1.txt", row.name = FALSE)

