
setwd (***)	#The work directory

#Name of the variables:
var.name <- read.csv("UCI HAR Dataset/features.txt", sep = "", header = FALSE)[2]

#Activities:
activ <- read.csv("UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE)

#Read all archives and merge in one:

for (set in c("test", "train")){	
	
	#Two differents dirs, one for test and other for training
	dir<-paste("UCI HAR Dataset/",sep="",set)

	#Each dir has three txt archives that contain de data
	#The first has one column and contains the subject data
	#The seconda has 561 columns and contains the experiment data
	#The third has one column and contains the activities data
	arc<-list.files(dir,pattern="txt",full.names=TRUE,recursive=FALSE)
	
	#For testing:
	#for (i in 1:length(arc)){
	#	data<-read.csv(arc[i])
	#	print(dim(data))	
	#}
	
	#Merge the three in one by column
	for (i in 1:length(arc)){
		if (i==1){
			data<-read.csv(arc[i],sep="",head=FALSE)
		} else {
			data<-cbind(data,read.csv(arc[i],sep="",head=FALSE))
		}
		#print(dim(data))		#For testing 
	}

	#Before to merge both sets, it is necesary to rename de names of column
	#The first column corresponds with the first archive of the folder, wich is the subject data. It is called "ID".
	#Tha last column corresponds with the third archive of the folder, wich is the activities data.It is called "Activity".
	#The others correspond with the experiment data. It is called like it is indicated in the "activity_labels.txt", named in the program like "activ".
	names(data)<-c("ID",as.character(var.name[,1]),"Activity")

	#And differentiate the set:
	data<-cbind(data,set)
	

	#At the end, merge by row the data from each set
	if (set=="test") {
		data.test<-data
		rm(data)
	} else {
		data<-rbind(data.test,data)
		rm(data.test)
	}
}

#Only we want the variables to reference measurements:
data<-data[,grepl("(std|mean|ID|Activity|Set)", names(data), ignore.case = TRUE)]


#Now, it includes the name of activities:
data<-merge(data, activ, by.x = "Activity", by.y = "V1")

#It renames the last column with the name of activities:
names(data)[length(data)]<-"Activity.Name"

#Saving the data
write.table(data, 
  "data.txt", sep=" ",
   col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")


#SUMMARIZE
#For summarize, the data will be convert to a tbl
library(dplyr) 
tbl.data<-as.tbl(data)

#The average of each feature by Activity and subject
mean.data.activ<-select(tbl.data,-set) %>%
			group_by(Activity.Name,ID) %>%
			summarise_all(mean)

#Saving the result
write.table(as.data.frame(mean.data.activ), 
  "Mean by activity and subject.txt", sep=" ",
   col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")



