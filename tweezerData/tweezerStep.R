library(dplyr)

# loading data ------------------------------------------------------------

file="./dig5-bead10-1.txt"

data <- read.csv(file, skip=5, sep="\t")
str(data)
names(data) <- c("Time_sec", "MagnetsZ_mm", "Extension_nm", 
                 "dx_nm", "dy_nm", "z0_nm", "x0_y_n", "y0", "Piezo_pstn", "turns")
str(data)


# calculate extension step ----------------------------------------

measureZ <- -2
RestZ <- -5

exten <- select(data, c(Time_sec,MagnetsZ_mm,Extension_nm))


# key in cycles
exten$cycle <- rep(1,nrow(exten))

for (i in 2: nrow(exten)) {
        
        if (exten$MagnetsZ_mm[[i]]> exten$MagnetsZ_mm[[i-1]]){
                exten$cycle[[i]]<- exten$cycle[[i-1]]+1   
        } else {
                
                
                exten$cycle[[i]]<- exten$cycle[[i-1]]  
        }
        
        
}


step <- { exten %>%
                  group_by(cycle, MagnetsZ_mm) %>%
                  summarise(
                            mean= mean(Extension_nm,na.rm = TRUE),
                            sd= sd(Extension_nm,na.rm = TRUE)
                  )
 
}

plot(step[step$MagnetsZ_mm==measureZ,]$mean, pch=3)
head(step)
change <- {
        step %>%
        filter(MagnetsZ_mm == measureZ) %>%
        select(cycle, mean, sd)        
        
}
change$delta <- rep(0, nrow(change))

for (i in 2:nrow(change)){
        change$delta[[i]] <- change$mean[[i]]-change$mean[[i-1]]
        
}

plot(change$delta)

write.csv(step, paste(file, "-step.csv"))
write.csv(change, paste(file, "-change.csv"))
