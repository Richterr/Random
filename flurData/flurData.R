library(dplyr)



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
