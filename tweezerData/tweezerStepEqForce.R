library(dplyr)

# loading data ------------------------------------------------------------

file="./bead51.txt"

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

# cycle counter 1:

for (i in 10: nrow(exten)) {
        
        if (is.na(exten$Extension_nm[[i]]) & !is.na(exten$Extension_nm[[i-9]])){
                
                exten$cycle[[i]]<- exten$cycle[[i-1]]+1   
        
        } else {
                
                
                exten$cycle[[i]]<- exten$cycle[[i-1]]  
        }
        
        
}



step <- { exten %>%
                  group_by(cycle, MagnetsZ_mm) %>%
                  summarise(
                          mean= mean(Extension_nm,na.rm = TRUE),
                          sd= sd(Extension_nm,na.rm = TRUE),
                          count = n()
                  ) %>%
                  mutate(sde=sd/sqrt(count))
          
}

plot(step[step$MagnetsZ_mm==measureZ,]$mean, pch=3)
head(step)
change <- {
        step %>%
                filter(MagnetsZ_mm == measureZ) %>%
                select(cycle, mean, sde)        
        
}
change$delta <- rep(0, nrow(change))

for (i in 2:nrow(change)){
        change$delta[[i]] <- change$mean[[i]]-change$mean[[i-1]]
        
}

plot(change$delta)

write.csv(step, paste(file, "-step.csv"))
write.csv(change, paste(file, "-change.csv"))
