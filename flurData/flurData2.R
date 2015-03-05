

cleanfolder <- function(folderName){
    
    repath <-file.path(folderName, "re")
    if (file.exists(repath)){
        
        unlink(repath, recursive=TRUE)
        
    }
}






# find the individual wavelength by log scanning


wavelengthScan <- function(datafiles){
    
    df <- data.frame(wave=read.csv(datafiles[[1]], skip=6, nrow=1, sep="\t")[[2]])    
    
    for (i in 2: length(datafiles)){
        print(i)
        df2 <- data.frame(wave=read.csv(datafiles[[i]], skip=6, nrow=1, sep="\t")[[2]])
        df <- rbind(df,df2)
        
        
    }
   df$wave <- as.factor(df$wave)
   df$fileName <- datafiles
   df
}

combineFluorData <- function (folderName, addTimeSecs){
    
    datafiles <- paste(folderName, list.files(folderName), sep="/")
    datafiles
    
    
    df <- wavelengthScan(datafiles)
    
    
    dir.create(file.path(folderName, "re"), showWarnings = FALSE)
    
    for (w in levels(df$wave)){
        
        # select the files for each dye 
        filelist <- df[df$wave==w,] 
        # print(w)
        
        # initilize a data frame for one dye indensity 
        dt <- data.frame(Time.sec.=numeric(0), Intensity=numeric(0))
        
        
        # combine data from one dye from different files into one data frame 
        for (i in 1: nrow(filelist)){
            
            tempdt <- read.csv(filelist$fileName[[i]], 
                               skip=15, sep="\t")
            tempdt$Time.sec. <- tempdt$Time.sec. + (i-1)*addTimeSecs
            
            dt <- rbind(dt, tempdt[,2:3])
            # print(dt) 
        }
        
        
        # creat a file 
        filedir <- paste(folderName,"re",sep="/")
        fileName <- paste(substr(w, start=4, stop=6),"nm.csv",sep="")
        logName  <-  paste("log", substr(w, start=4, stop=6),"nm.csv",sep="")
        
        # save dataframe to the file
        write.csv(dt, 
                  paste(filedir,fileName,sep="//"), 
                  row.names=FALSE)
        
        # read in log 
        log <- readLines(filelist$fileName[[1]], 15)
        log <- gsub(",","",log)
        # write log 
        write.csv(log,
                  paste(filedir,logName,sep="//"),,
                  row.names=FALSE)
        
    }

}


