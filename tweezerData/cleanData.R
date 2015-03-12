source("tweezerStepFunc.R")

file <- "./sample -detail.txt"
df <- read.csv(file, head=T)
names(df)
head(df)

exten<-as.data.frame(df$Extension_nm)

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




# vis exposure time-----------------------
# vis accumulation time is the (current time)-(UV exposure time)

plot(df$Time_sec)
df$visAccu<-cumsum(df$Time_sec)
plot(cycle, df$visAccu/3600)


TweezerCycleStarterIndex <- function(){
    
    
    cycleStarter <- rep(0, nrow(cycle))
    # the first place that cycle number is higher than last one 
    
    for (i in 2:nrow(df$cycle)){
        
        if (cycle[[i]]>cycle[[i-1]] & cycle[[i]]==cycle[[i-1]] ){
            
            cycleStarter[[i]] <- 1
        }
        
    }
    cycleStarter[[1]] <-1
    
    cycleStarter
}
TweezerCycleStarterIndex(as.data.frame(cycle))

