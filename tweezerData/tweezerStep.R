library(dplyr)

# loading data ------------------------------------------------------------

file="./dig1/dig1-bead2-1.txt"

data <- read.csv(file, skip=5, sep="\t")
str(data)
names(data) <- c("Time_sec", "MagnetsZ_mm", "Extension_nm", 
                 "dx_nm", "dy_nm", "z0_nm", "x0_y_n", "y0", "Piezo_pstn", "turns")
str(data)


# calculate extension step ----------------------------------------

measureZ <- -2
RestZ <- -5

exten <- select(data, c(Time_sec,MagnetsZ_mm,Extension_nm))


# cycle counters using magnetic position

# initialize cycles at all rows to be 1 
exten$cycle <- rep(1,nrow(exten))


for (i in 2: nrow(exten)) {
        
        # if the magnets position is larger than the prious one
        # a position change of magnet is detected and cycle counter raise by 1 
        # otherwise, record the same cycle
        if (exten$MagnetsZ_mm[[i]]> exten$MagnetsZ_mm[[i-1]]){
                exten$cycle[[i]]<- exten$cycle[[i-1]]+1   
        } else {
  
                exten$cycle[[i]]<- exten$cycle[[i-1]]  
        }
   
}


# summary the steps by calculatring the mean, sd and sem within each cycles

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


# change is the difference between each step 
# when the Magnets position is at the measurement position

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
