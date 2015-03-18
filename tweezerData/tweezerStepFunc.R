library(dplyr)

measureZ <- -2
RestZ <- -5


# TweezerExtenLoad --------------------------------------------------------
TweezerExtenLoad <- function(file){
    
    # the function will return a data with three columns
    # Time_sec,MagnetsZ_mm,Extension_nm
    
    data <- read.csv(file, skip=5, sep="\t")
    names(data) <- c("Time_sec", "MagnetsZ_mm", "Extension_nm", 
                     "dx_nm", "dy_nm", "z0_nm", "x0_y_n", "y0", "Piezo_pstn", "turns") 
    exten <- select(data, c(Time_sec,MagnetsZ_mm,Extension_nm))
    
}


# add experiment details & output csv with experiment details--------------------------------------------------------
TweezerAddDetail <- function(file, expDetail) {     
    df <- TweezerExtenLoad(file)
    df$id <- file
    df$expDetail <- expDetail
    names(df) <- c("Time_sec", "Extension_nm", 
                     "id", "exp-detail") 
    TweezerWritecsv(df, sub(".txt","", file), "-detail.txt")
    
}


## how to define cycle in UV- vis tweezer experiment------------------------------------------
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



TweezerCycleCountByMag <- function(Mod=1, exten){   
    # the function consider the position change of magnets as cycle initiator    
    # exten data must contain Magnets position as one column 
    
    
    # initializing
    cyc <- rep(1,nrow(exten)) 

    # Mod=1, the function consider position value increase as initiator an add cycles
    if (Mod==1){         
        for (i in 2: nrow(exten)) {  
            if (exten$MagnetsZ_mm[[i]]> exten$MagnetsZ_mm[[i-1]]){
                cyc[[i]]<- cyc[[i-1]]+1   
            } else {                
                cyc[[i]]<- cyc[[i-1]]  
            }            
        }        
    }
    cyc
}



TweezerCycleCountByNA <- function(exten){
    # TweezerCycleCountByNA count cycles by detecting continous NAs 
    cyc <- rep(1,nrow(exten))   
    for (i in 10: nrow(exten)) {
        
        if (is.na(exten$Extension_nm[[i]]) & !is.na(exten$Extension_nm[[i-9]])){            
            cyc[[i]]<- cyc[[i-1]]+1               
        } else {
            
            
            cyc[[i]]<- cyc[[i-1]]  
        }
     
    }
    cyc
 
}





TweezerCycleCountByTime <- function(exten, deltaT){
       
    cyc <- rep(1,nrow(exten)) 
    
    for (i in 10: nrow(exten)) {
        
        if (exten$Time_sec[[i]]>exten$Time_sec[[i-1]]+deltaT){   
            cyc[[i]]<- cyc[[i-1]]+1               
        } else {
            cyc[[i]]<- cyc[[i-1]]  
        }
        
    }
    cyc
    
} 



TweezerStepping <- function(exten){
        # summary the steps by calculatring the mean, sd and sem within each cycles    
        step <- { exten %>%
                      group_by(cycle, MagnetsZ_mm) %>%
                      summarise(
                          mean= mean(Extension_nm,na.rm = TRUE),
                          sd= sd(Extension_nm,na.rm = TRUE),
                          count = n()
                      ) %>%
                      mutate(sem=sd/sqrt(count))
                  
        }
    
}

TweezerSteppingTrace <- function(exten, step){
    # exten should contain cycle conlumn
    
    trace <- rep(0, nrow(exten))    
    for (i in 1: nrow(exten)){        
        trace[[i]] <- step$mean[[exten$cycle[[i]]]]        
    }
    trace
    
}


TweezerSteppingDelta <- function(step, measureZ){  
    
    # change is the difference between each step at a paticula magnets position            
    change <- {        
        step %>%
            filter(MagnetsZ_mm == measureZ) %>%
            select(cycle, mean, sem)              
    }   
    
    delta <- rep(0, nrow(change)) 
    for (i in 2:nrow(change)){
        
        delta[[i]] <- change$mean[[i]]-change$mean[[i-1]]
        
    }
    delta
        
}


# TweezerWritecsv ---------------------------------------------------------

TweezerWritecsv <- function(df, file, suffix){
    write.csv(df, paste(file, suffix),row.names=FALSE)
  
}

# # write.csv(step, paste(file, "-step.csv"))
# # write.csv(change, paste(file, "-change.csv"))
# 
# plot(step[step$MagnetsZ_mm==measureZ,]$mean, pch=3)
# head(step)
# 
# 
# 
# # when the Magnets position is at the measurement position
# 
# plot(change$delta)


