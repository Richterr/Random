library(dplyr)


folderName= "./sampleData/"


datafiles <- paste(folderName, list.files(folderName), sep="/")

Dye1List <- c(1,5,8)
Dye2List <- c(2,4,7)
Dye3List <- c(3,6)


getFileByIndex <- function(folderName, list, suffix){
    
    datafiles <- paste(folderName, list, sep="/")
    datafiles <- paste0(datafiles, suffix)
    
}
    
Dye1FileList <- getFileByIndex(folderName,Dye1List, ".txt")



Dye2List <- c(2,4,7)
Dye3List <- c(3,6)

    
    

# write log ---------------------------------------------------------------
 
logf <- paste0("log", list.files(folderName),sep="")
logf <- paste(folderName, logf ,sep="/")

for (i in 1:n){   
    # read in log 
    log <- readLines(datafiles[[i]], 15)
    log <- gsub(",","",log)
    # write log 
    write.csv(log,logf[[i]] ,row.names=FALSE)
}





n <- length(files)



for (i in 1:n){
    
    data <- read.csv(files[[i]], skip=15, sep="\t")
    
    # df <- data.frame(files[[i]]=data)
    
}



# read in flur data
temp.data <- read.csv("./control2_2.csv", skip=15)
temp.data <- data.frame(temp.data)

# read in log 
log <- readLines("./control2_2.csv", 15)
log <- gsub(",","",log)

# write log 
write.csv(log, "./log.txt",row.names=FALSE)

# arrange flur data
data <- select(temp.data,Time.sec.,Intensity)

# rename 
names(data) <- c("Time(sec)", "Intensity")

# remove temp data
rm(temp.data)

# check data
head(data)

# output data
write.csv(data, "./mydata.csv", row.names=FALSE)
