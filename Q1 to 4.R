#Read CSV

Sanu <- read.csv("Exam.csv", TRUE,",")

#Calculate Mean and Stadard Deviations

AttndMean <- sum(Sanu$Attendance)/length (Sanu$Attendance)
AttndMeanper <- paste(round(AttndMean
  *100,0),"%",sep="")
Markmean <-sum(Sanu$EndTermMarks)/length(Sanu$EndTermMarks)
stddevAttnd <- sqrt(((sum((Sanu$Attendance-AttndMean)^2)/length (Sanu$Attendance))))
stddevMark <- sqrt(((sum((Sanu$EndTermMarks-Markmean)^2)/length(Sanu$EndTermMarks))))
Zattnd <-(Sanu$Attendance-AttndMean)/stddevAttnd
Zmark <-(Sanu$EndTermMarks-Markmean)/stddevMark
AttndMean
AttndMeanper
Markmean
stddevAttnd
stddevMark

#Zscore for each values

Zattnd
Zmark

#Encoding

Dummy <- as.data.frame(model.matrix(~0+Sanu$Gender, data=Sanu),Sanu$Name)
Sanu1 <-subset(Sanu,select=c(Name,Attendance,EndTermMarks))
Coding <-cbind(Sanu1,Dummy)
Coding

#Export data after coding to csv format

write.csv(Coding,file="Coding.csv",row.names = FALSE)

#Split data to Trainging and Test

Sample <-sample(2,nrow(Coding),replace=TRUE,prob=c(0.70,0.30))
Training <-Coding[Sample==1,]
Training
Test <-Coding[Sample==2,]
Test

#export data to csv format

write.csv(Training,file="Training.csv",row.names = FALSE)
write.csv(Test,file="Test.csv",row.names = FALSE)

