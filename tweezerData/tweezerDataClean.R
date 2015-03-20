# "Thu Mar 19 09:52:59 2015"
# this scipt is mean to clean UV-vis tweezer experiment data 
# the UV-vis tweezer experiemnts aims to measure the single molecular events of light-driven nanomotor

library(dplyr)
source("./tweezerDataCleanFunc.R")

file <- "D:/PostDoc/PostDoc2013/tweezer/2015-02-25-ND4-BP-H672/BP-ND4-UV4m-Vis1m-bead11.txt"
expDetails <- "BP-ND4-UV1m-Vis1m"
originalData <- file
df <-TweezerExtenLoad(file)



## plot out the data
par(mfrow=c(2,1))
plot(df$Time_sec, df$Extension_nm, ylim = c(0,1000))
plot(df$Time_sec, df$MagnetsZ_mm)



## filter out the illogical data
df <- df[which(df$Extension_nm<500 & df$Extension_nm>100),]
plot(df$Time_sec,df$Extension_nm, ylim=c(0,500))




# detect cycles
# select proper deltaT to get the right cycles
deltaT <- 20
UVfilter <- 40
cycle <- TweezerCycleCountByTime(df, deltaT)




# check cycle detection
par(mfrow=c(3,1))

plot(df$Time_sec,cycle)
title(main = list(expDetails, cex = 1.5,
                  col = "red", font = 3))
plot(df$Time_sec, df$Extension_nm)

plot(df$Time_sec, df$MagnetsZ_mm)



# if OK, plot out as record
dev.print(png, file = sub(".txt","_plotcycle.png",file), width = 1024, height = 768)
df$cycle <- cycle


# select a force to compare
measure_Z <- -2




# combine data together
df_new <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list(ExtensionAverByCycle(df, measure_Z=measure_Z), 
                      VisDurationPerCycle(df), 
                      UVDurationPerCycle(df, filter=UVfilter)))




# addin all information--------------------------------------- 
df_new$expDetails <- expDetails 
df_new$originalData <- originalData
df_new$measure_Z <- measure_Z
df_new$forceMode <- paste(as.character(unique(df$MagnetsZ_mm)),collapse="")


# analysis 
df_new$stepSize <- df_new$meanExtension-c(df_new$meanExtension[1],df_new$meanExtension[1:(nrow(df_new)-1)] )
df_new$stepSize_cumsum<-cumsum(df_new$stepSize)
df_new$Visduration_cumsum<-cumsum(df_new$Visduration)
df_new$UVduration_cumsum <- cumsum(df_new$UVduration)


par(mfrow=c(3,1))
df_new$UVduration_cumsum

plot(stepSize_cumsum ~ UVduration_cumsum, data=df_new)    
plot(stepSize_cumsum ~ Visduration_cumsum, data=df_new)   
plot(stepSize_cumsum ~ stepSize_cumsum, data=df_new)  

write.csv(df_new,sub(".txt","_clean.txt",file), row.names=F)



# combine all the clean data------------------------------
cleanDatalist <- list.files("D:/PostDoc/PostDoc2013/tweezer/",pattern="*_clean.txt", recursive=TRUE)
cleanDatalist <- paste0("D:/PostDoc/PostDoc2013/tweezer/",cleanDatalist)

cleanDfAll <- read.csv(cleanDatalist[[1]], header=T)
for (i in 2: length(cleanDatalist)) {
    
    cleanDf <- read.csv(cleanDatalist[[i]], header=T)
    if (ncol(cleanDf)<ncol(cleanDfAll)){
        next
    }
    
    cleanDfAll <- rbind(cleanDfAll,cleanDf)
    
}

# how many experiments
unique(cleanDfAll$originalData)
write.csv(cleanDfAll,"D:/PostDoc/PostDoc2013/tweezer/ND4_clean.txt", row.names=F)   
    
    


