
# library(dplyr)
source("flurData2.R")

# change parameters here ---------------------------------------------------------------
# delet the re fold before using 

#This is your UV duration in sec, please modify accordingly
addTimeSecs <- 900




















# don't change ------------------------------------------------------------------------


folderName <-  "./sampleData/"
cleanfolder(folderName)
combineFluorData(folderName, addTimeSecs)
