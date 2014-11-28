source("GLS-STEP.R")
check.packages()
setwd("MyTraces FOLDER")  # Set the My traces Folder Directory path
file.names<-system("ls",intern=TRUE)   # This should get the names of the trace files in the folder. 
for(i in 1:length(file.names)){
	RNA<-read.table(file.names[i],sep="\t",header=TRUE)
	w<-c(seq(10,90,by=10),seq(100,1000,by=25))           
	Y<-RNA[,1]
	times<-RNA[,2]   
	chpt0<-get.zstat(y,times,w)
	chpt1<-ChPt(chpt0)
	plot.step(chpt0,chpt1,pdfname=paste("RNAFIG-",i,".pdf",sep=""))
	RNA.RESULT1<-get.results(chpt1)
	write.table(RNA.RESULT1,file=paste("RNA-RESULT-",i,".txt",sep=""),sep="\t",header=TRUE)
	myModel<-get.model(chpt1,type="bic")
	save(myModel,file=paste("myModel-",i,".Rdata",sep=""))  
# This is saves as a R file .Rdata, you can load it into R by   load("myModel-1.Rdata")
}