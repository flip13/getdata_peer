setwd("~/coursera/getdata_peer")
trainx2 = NULL
testx2 = NULL
x2names = NULL
i=0

#pull together the parts of each dataset
features = read.table("UCI HAR Dataset/features.txt")
trainsubject = read.table("UCI HAR Dataset/train/subject_train.txt")
trainy = read.table("UCI HAR Dataset/train/y_train.txt")
trainx = read.table("UCI HAR Dataset/train/X_train.txt")
testsubject = read.table("UCI HAR Dataset/test/subject_test.txt")
testy = read.table("UCI HAR Dataset/test/y_test.txt")
testx = read.table("UCI HAR Dataset/test/X_test.txt")

#name each of the variables for question 4.

colnames(trainsubject) = "Subject"
colnames(testsubject) = "Subject"
colnames(trainy) = "Labels"
colnames(testy) = "Labels"
colnames(trainx) = features$V2
colnames(testx) = features$V2

#seperate out the data for mean and standard deviation of each measurement for question 2.

for(i in 1:nrow(features)) {
    if(grepl("mean", names(testx)[i], ignore.case=T | grepl("std", names(testx)[i], ignore.case=T)) ) {
      testx2 = cbind(testx2, testx[,i])
      trainx2 = cbind(trainx2, trainx[,i])
      x2names = rbind(x2names, names(testx)[i])
    }
}
colnames(testx2) = x2names
colnames(trainx2) = x2names
train = cbind(trainsubject, trainy, trainx2)
test = cbind(testsubject, testy, testx2)

#merge the test and training datasets for question 1.

data = rbind(train, test)

#put labels in for each activity for question 3.

data$Labels = as.character(data$Labels)
label = read.table("UCI HAR Dataset/activity_labels.txt")
label$V2 = as.character(label$V2)
for(j in 1:nrow(data)) {
    if(data$Labels[j] == "1") {
        data$Labels[j] = label[1,2]
    }
    else if(data$Labels[j] == "2") {
        data$Labels[j] = label[2,2]
    }
    else if(data$Labels[j] == "3") {
        data$Labels[j] = label[3,2]
    }
    else if(data$Labels[j] == "4") {
        data$Labels[j] = label[4,2]
    }
    else if(data$Labels[j] == "5") {
        data$Labels[j] = label[5,2]
    }
    else data$Labels[j] = label[6,2]
}

#find the mean of each observation column for each subject and write the data for question 5.
varmeans = NULL
data$Labels = as.factor(data$Labels)
data$Subject = as.integer(data$Subject)
for(l in 1:30) {
    temp1 = subset(data, Subject == l)
    temp1$Labels = as.factor(temp1$Labels)
    temp3 = as.data.frame(levels(data$Labels))
    for(k in 3:ncol(temp1)){
        temp2 = tapply(temp1[,k], temp1$Labels, mean)
        temp2 = as.data.frame(temp2)
        temp3 = cbind(temp3, temp2)
    }
    submean = cbind(l, temp3)
    varmeans = rbind(varmeans, submean)
}
rownames(varmeans)= c(1:nrow(varmeans))
colnames(varmeans) = names(data)
write.table(varmeans, file="variable_means.txt", row.name=FALSE)
