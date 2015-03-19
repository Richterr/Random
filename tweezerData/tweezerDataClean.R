# "Thu Mar 19 09:52:59 2015"
# this scipt is mean to clean UV-vis tweezer experiment data 
# the UV-vis tweezer experiemnts aims to measure the single molecular events of light-driven nanomotor

library(dplyr)
source("./tweezerDataCleanFunc.R")



file <- "D:/PostDoc/PostDoc2013/tweezer/2015-02-25-ND4-BP-H672/BP-ND4-UV4m-Vis1m-bead11.txt"

# fill in the experiment detials
expDetails <- "BP-ND4-UV4m-Vis1m"
originalData <- file
df <-TweezerExtenLoad(file)



## plot out the data
par(mfrow=c(2,1))
plot(df$Time_sec, df$Extension_nm)
plot(df$Time_sec, df$MagnetsZ_mm)



## filter out the illogical data
df <- df[which(df$Extension_nm<400 & df$Extension_nm>0),]
plot(df$Time_sec,df$Extension_nm)




# detect cycles
# select proper deltaT to get the right cycles
deltaT <- 40
UVfilter <- 30
cycle <- TweezerCycleCountByTime(df, deltaT)




# check cycle detection
par(mfrow=c(1,1))
title(main = list(expDetails, cex = 1.5,
                  col = "red", font = 3))
plot(df$Time_sec,cycle)

plot(df$Time_sec, df$Extension_nm, ylim=range(df$Extension_nm))

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
plot(stepSize_cumsum ~ UVduration_cumsum, data=df_new)    
plot(stepSize_cumsum ~ Visduration_cumsum, data=df_new)   
plot(stepSize_cumsum ~ stepSize_cumsum, data=df_new)  


write.csv(df_new,sub(".txt","_clean.txt",file), row.names=F)

