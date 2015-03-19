


# extract subset of information ---------------------------------------------
# Only keep Time_sec, MagnetsZ_mm and "Extension_nm" for data analysis


TweezerExtenLoad <- function(file){
    
    # the function will return a data with three columns
    # Time_sec,MagnetsZ_mm,Extension_nm
    
    data <- read.csv(file, skip=5, sep="\t")
    names(data) <- c("Time_sec", "MagnetsZ_mm", "Extension_nm", 
                     "dx_nm", "dy_nm", "z0_nm", "x0_y_n", "y0", "Piezo_pstn", "turns") 
    exten <- select(data, c(Time_sec,MagnetsZ_mm,Extension_nm))
    
}





# add cycle-----------------------
# ## how to define cycle in UV- vis tweezer experiment------------------------------------------
## vis-UV-vis-UV-vis
## 111111122222223333

## extract cycles by extension signal
## during the UV time, the tracing software of tweezer doesn't record 
## the extension signal of beads. So the time of record become discontinued. 
## the function TweezerCycleCountByTime finds the discoutinution of time 
## and the cycle counter increase by 1. 


## how to do the extension measurement in a constant force
## first, define a force that you want to measure
## measureZ is the vertical position away from the glass surface of magnatics at one particular experiment setup
## which may denote the measurement force

## all the extension comparison should be done 
## at the same measurement force to get a fair conclusion  


## when the light switch from UV to visible, 
## the bead trace software will automatically start to refocus microscop by piezo. 
## but sometime, the trace software is not able to detect the bead. We need to manuelly do it 
## In term of measurment, during the time of refocus, either auto or manually, noise signal will be recorded. 
## and the length of these signals are uncontrolable, which will cause inaccuracy in our final analysis
## the auto focus position often takes 10-20s, which is 17%- 30% of the whole data in a minute experiment
## but the manu focus might take very long time. 

## one way to alleviate the problem is that only account for the last portion of data 
## which is roughly to be measured under forcusing status. 

## another way is to 
## manully mark the time when the microscope is reforcused, for example, shift the magnetic a little bit.
## the manually mark method may be simply change the magnetic position a little and start timing the experiments 
## This may also have problem becuase of elongating the light exposure potencially 
## but we don't use this in our previous experiment.
# adjust deltaT to get reasiable cycles 


TweezerCycleCountByTime <- function(df, deltaT){
    
    cyc <- rep(1,nrow(df)) 
    
    for (i in 10: nrow(df)) {
        
        if (df$Time_sec[[i]]>df$Time_sec[[i-1]]+deltaT){   
            cyc[[i]]<- cyc[[i-1]]+1               
        } else {
            cyc[[i]]<- cyc[[i-1]]  
        }
        
    }
    cyc
    
} 



# Uv exposure time per cycle--------------------- 
# UVDurationPerCycle(df)

# time sequence 
UVDurationPerCycle <- function(df, filter=60){
    
    timeSeq <- df$Time_sec
    start <- timeSeq[1]
    end <- timeSeq[length(timeSeq)]
    deltat <- c(timeSeq,end)-c(start,timeSeq)
    UVDurationPerCycle<- data.frame(cycle=1: length(deltat[which(deltat>filter)]),
                                    UVduration =deltat[which(deltat>filter)])
    
    UVDurationPerCycle
    
}


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



# mean of extension --------------------------------------------------

# average only the data points at mesurement Z at each cycle

# plot(df$cycle,df$Extension_nm)



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







