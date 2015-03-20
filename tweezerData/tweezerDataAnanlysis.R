
# ND4 data analysis-------------------------
cleanDfAll <- read.csv("D:/PostDoc/PostDoc2013/tweezer/ND4_clean.txt", header=T)
cleanDfAll$originalData
df_new <- cleanDfAll[cleanDfAll$originalData=="D:/PostDoc/PostDoc2013/tweezer/2015-02-25-ND4-BP-H672/BP-ND4-UV4m-Vis1m-bead11.txt",]

plot(stepSize_cumsum ~ UVduration_cumsum, data=df_new)    
plot(stepSize_cumsum ~ Visduration_cumsum, data=df_new)   
plot(stepSize_cumsum ~ stepSize_cumsum, data=df_new) 


bp <- cleanDfAll[grepl("^B",cleanDfAll$expDetails),]
fp <- cleanDfAll[grepl("^F",cleanDfAll$expDetails),]


unique(cleanDfAll$forceMode)
unique(bp$originalData)
unique(fp$originalData)


# Questions 1: ---------------------------
# is there any difference between Backward pulling and foward pulling???
# don't consider force mode

# compare by cycle 
plot(bp$cycle,bp$stepSize_cumsum,col=4,ylim=c(-50,50),xlim=c(0,12),pch = 1, xlab="",ylab="")
par(new=T)
plot(fp$cycle,fp$stepSize_cumsum,col=2,ylim=c(-50,50),xlim=c(0,12),pch = 1)

title(main="blue: back pulling, red: foward pulling")

# compare by UV

plot(bp$UVduration_cumsum,bp$stepSize_cumsum,col=4,ylim=c(-80,50),xlim=c(0,3500),pch = 1, xlab="",ylab="")
par(new=T)
plot(fp$UVduration_cumsum,fp$stepSize_cumsum,col=2,ylim=c(-80,50),,xlim=c(0,3500),pch = 1)

title(main="blue: back pulling, red: foward pulling")


plot(bp$UVduration_cumsum,bp$stepSize_cumsum,col=4,ylim=c(-80,50),xlim=c(0,600),pch = 1, xlab="",ylab="")
par(new=T)
plot(fp$UVduration_cumsum,fp$stepSize_cumsum,col=2,ylim=c(-80,50),,xlim=c(0,600),pch = 1)
title(main="blue: back pulling, red: foward pulling")


# compare by Vis

plot(bp$Visduration_cumsum,bp$stepSize_cumsum,col=4,ylim=c(-50,50),xlim=c(0,2000),pch = 1, xlab="",ylab="")
par(new=T)
plot(fp$Visduration_cumsum,fp$stepSize_cumsum,col=2,ylim=c(-50,50),,xlim=c(0,2000),pch = 1)

title(main="blue: back pulling, red: foward pulling")


# consider force mode




    # compare by cycle 


    # compare by UV
    # compare by Vis