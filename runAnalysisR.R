
setwd ("C:/Users/Almudena/Documents/Data Science/03.Getting and Cleaning Data/Proyecto Final")

#Name of the variables:
var.name <- read.csv("UCI HAR Dataset/features.txt", sep = "", header = FALSE)[2]

#Activities:
activ <- read.csv("UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE)

#Read all archives and merge in one:

for (set in c("test", "train")){	
	
	#Two differents dirs, one for test and other for training
	dir<-paste("UCI HAR Dataset/",sep="",set)

	#Each dir has three txt archives then contain de data
	arc<-list.files(dir,pattern="txt",full.names=TRUE,recursive=FALSE)
	
	#For testing the program:
	#for (i in 1:length(arc)){
	#	data<-read.csv(arc[i])
	#	print(dim(data))	
	#}
	
	#This for merge the three in one by column
	for (i in 1:length(arc)){
		if (i==1){
			data<-read.csv(arc[i],sep="",head=FALSE)
		} else {
			data<-cbind(data,read.csv(arc[i],sep="",head=FALSE))
		}
		#print(dim(data))		#For testing the program 
	}

	#Before to merge both sets, it is necesary to rename de names of column
	names(data)<-c("ID",as.character(var.name[,1]),"Activity")

	#And distintc the set:
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



#SUMMARIZE
#For summarize, the data will be convert to a tbl
library(dplyr) 
tbl.data<-as.tbl(data)


mean.data.set<-tbl.data %>%
			group_by(set) %>%
			summarise_all(mean)

write.table(as.data.frame(mean.data.set), 
  "Mean by set.txt", sep=" ",
   col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")

mean.data.activ<-tbl.data %>%
			group_by(Activity.Name,ID) %>%
			summarise_all(mean)

write.table(as.data.frame(mean.data.activ), 
  "Mean by activity and subject.txt", sep=" ",
   col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")



