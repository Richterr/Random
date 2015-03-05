library(dplyr)
source("flurData2.R")

# change parameters here ---------------------------------------------------------------
# delet the re fold before using 
folderName= "./sampleData/"
addTimeSecs <- 1800




# don't change ------------------------------------------------------------------------
combineFluorData(folderName, addTimeSecs)