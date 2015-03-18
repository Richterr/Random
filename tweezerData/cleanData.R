source("tweezerStepFunc.R")

file <- "./sample -detail.txt"
df <- read.csv(file, head=T)


# add cycle-----------------------
# adjust deltaT to get reasiable cycles 

# wrong one 
deltaT <- 10
cycle <- TweezerCycleCountByTime(df, 1)
plot(cycle, df$Time_sec)


# rigtht one 
deltaT <- 50
cycle <- TweezerCycleCountByTime(df, 50)
plot(cycle, df$Time_sec)

df$cycle <- cycle




# Uv exposure time per cycle--------------------- 

# time sequence 
UVDurationPerCycle <- function(df, filter=60){
    timeSeq <- df$Time_sec
    start <- timeSeq[1]
    end <- timeSeq[length(timeSeq)]
    deltat <- c(timeSeq,end)-c(start,timeSeq)
    UVDurationPerCycle<- data.frame(cycle=1: length(deltat[deltat>filter]),UVduration =deltat[deltat>filter])

    UVDurationPerCycle
    
}

UVDurationPerCycle(df)


# vis exposure time per Cycle-----------------------

# df contains cycle index and time sequence 
VisDurationPerCycle <- function(df){
    
    totalCycle <- df$cycle[[nrow(df)]]
    
    visDurationPerCycle <- data.frame(cycle=0, Visduration=0)
    
    for (i in 1:totalCycle){
        # extract visible signal at each cycle
        vis_i <- df$Time_sec[which(df$cycle==i)]
        
        # substract end time to starting time to get time inverval
        visDuration <- vis_i[[length(vis_i)]]-vis_i[[length(1)]]
        visDurationPerCycle<-rbind(visDurationPerCycle,c(cycle=i,Visduration=visDuration))
    }
    
    visDurationPerCycle[-1,]
}




cumsum(VisDurationPerCycle(df)$duration)


# mean of extension --------------------------------------------------

# average only the data points at mesurement Z at each cycle

plot(df$cycle,df$Extension_nm)



ExtensionAverByCycle <- function(df, measure_Z){
    
    totalCycle <- df$cycle[[nrow(df)]]
    std <- function(x) sd(x)/sqrt(length(x))
    
    # init
    ExtensionAverByCycle <- data.frame(cycle=0, meanExtension=0, stdExtension=0)
    
    for (i in 1:totalCycle){
        
        # extract extension signal of each cycle at measuremnt Z
        exten_i <- df$Extension_nm[which(df$MagnetsZ_mm==measure_Z & df$cycle==i)]
        exten_i <- na.omit(exten_i)
        print(length(exten_i))
        # average
        mean_exten <- mean(exten_i)
        # standard error of mean
        std_exten <- std(exten_i)
        ExtensionAverByCycle<-rbind(ExtensionAverByCycle,
                                    c(cycle=i, meanExtension=mean_exten, stdExtension=std_exten))
    }
    
    ExtensionAverByCycle[-1,]
}


df_new <- Reduce(function(x, y) merge(x, y, all=TRUE), 
       list(ExtensionAverByCycle(df, measure_Z=-2), 
            VisDurationPerCycle(df), 
            UVDurationPerCycle(df)))

df_new






