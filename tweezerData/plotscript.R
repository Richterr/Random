

source("tweezerStepFunc.R")


dat <- TweezerExtenLoad("./sample.txt")
dat$cycle<-TweezerCycleCountByTime(dat, 5)

step <- TweezerStepping(dat)
dat$trace<-TweezerSteppingTrace(dat, step)


names(dat)
plot(dat$Time_sec, dat$Extension_nm, 
     ylim=c(0,500), 
     xlim=c(0,6000),
     main="UV2mins-BP")

lines(dat$Time_sec,dat$trace,col="red")

for (i in 2: nrow(dat)){
    
    if(dat$cycle[[i]]>dat$cycle[[i-1]]){
        text(dat$Time_sec[[i]]-50, dat$trace[[i]], round(dat$trace[[i]],1), cex=0.8)
    }
    
}




