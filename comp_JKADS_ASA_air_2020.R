#quick script to plot air data from ASA comparison - JKADS5099 vs JKADS5100
#data was grabbed on 5-Mar-2020

library(latticeExtra)
library(openair)

#this is where I saved the files 
dir <- "C:/Users/gue02h/cloudstor/N2O_comparison/data"

setwd(dir)


#there as 'fixed width" files - assuming they are the same as for s5075?

file_widths <- c(6,5,10,13,13,5,10,10,12,10,10,1,9,1,9,1,7,6,13,10,1,9,1,9,1,7,6,13,10,1,9,1,9,1,7,6,13)
col_cl <- c("character","character","factor","factor","factor","factor", "numeric", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric")
data <- read.fwf(file = "rvi_n2o_co_all", widths= file_widths, header = FALSE, skip = 2, na.strings = "nan", strip.white=T,
                    colClasses = col_cl)
#reading these fixed widths files is SLOW! 
#clean it up a little
dat <- data[,c(3:11,13,15:20,22,24:28)]
#create a date array
date <- as.POSIXct(paste(data$V1, data$V2), format = "%y%m%d %H%M", tz = "UTC")
df <- cbind(date,dat)
name_cols <- c("date", "type", "sample", "standard", "port", "cavity_temp", "cavity_press","cavity_press_stdev", 
               "h2o", "co_C", "co_wet", "co_dry", "co_flag", "co_stdev", "co_N", "co_target_error", 
               "n2o_C", "n2o_wet", "n2o_dry", "n2o_flag", "n2o_stdev", "n2o_N", "n2o_target_error")
names(df) <- name_cols

#assign to proper name 
#s5100 <- df
#s5099 <- df
s5100 <- df

#s5099$instrument <-"JKADS5099"
s5100$instrument <- "JKADS5100"



###try dplyr stuff here 

library(dplyr)
jkads <- bind_rows(s5100, s5099) #this coerces classes
jkads2 <- rbind(s5100, s5099) #this does not 

jkads_by_instrument <- group_by(jkads, instrument)
summarise(jkads_by_instrument, 
          mean_n2o_C = mean(n2o_C, na.rm = T),
          sd_n2o_C = sd(n2o_C, na.rm = T),
          mean_co_C = mean(co_C, na.rm = T),
         std_co_C = sd(co_C, na.rm = T))
#but this includes all data - NOT matched by date, 
#so this does not really work in this context 
#BUT I can do wide, then use gather - this will give me want I need

#select only "air" data 

s5100_air <- subset(s5100, sample %in% "air_inlet")
s5099_air <- subset(s5099, sample %in% "ambient_air")
plot(n2o_C ~date, data = s5100_air)
points(n2o_C ~date, data = s5099_air, col = "red")

#merge by date 
JKADS_air <- merge(s5099_air, s5100_air, suffixes = c("_s5099", "_s5100"), by = "date")
#calc
JKADS_air <- within(JKADS_air, n2o_C_diff <- n2o_C_s5099 - n2o_C_s5100)
JKADS_air <- within(JKADS_air, co_C_diff <- co_C_s5099 - co_C_s5100)

plot(n2o_C_diff ~date, data = JKADS_air)
mean(JKADS_air$n2o_C_diff)
sd(JKADS_air$n2o_C_diff)
plot(co_C_diff ~date, data = JKADS_air)
mean(JKADS_air$co_C_diff)
sd(JKADS_air$co_C_diff)
#this looks bad, but this is with the built in water corrections 
mean(JKADS_air$n2o_C_s5099)
mean(JKADS_air$n2o_C_s5100)
sd(JKADS_air$n2o_C_s5099)
sd(JKADS_air$n2o_C_s5100)
mean(JKADS_air$co_C_s5099)
mean(JKADS_air$co_C_s5100)

#look at diff just for Nafion period 
JKADS_dry <- subset(JKADS_air, date > "2020-03-04 06:00")

plot(n2o_C_diff ~date, data = JKADS_dry)
plot(co_C_diff ~date, data = JKADS_dry)
plot(n2o_C_diff ~n2o_C_s5099, data = JKADS_dry)
plot(co_C_diff ~co_C_s5100, data = JKADS_dry)

mean(JKADS_dry$n2o_C_diff)
sd(JKADS_dry$n2o_C_diff)
mean(JKADS_dry$co_C_diff)
sd(JKADS_dry$co_C_diff)
#this is similarly bad 

plot(n2o_C_diff ~n2o_C_s5099, data = JKADS_air)
plot(co_C_diff ~co_C_s5100, data = JKADS_air)
plot(n2o_C_diff ~n2o_stdev_s5099, data = JKADS_air)
plot(co_C_diff ~co_stdev_s5099, data = JKADS_air)

#all the above is on 1-minute basis 
# try it on 5 min averages? 

library(openair)
JKADS_air_5min <- timeAverage(JKADS_air, avg = "5 min")
plot(n2o_C_diff ~date, data = JKADS_air)
plot(co_C_diff ~date, data = JKADS_air)

#BTW my df is not tidy - I should have an instrument columns, and rbind instead 
#need to learn how to calculate diffs across the two, for a specific point in time
#apparently there is a way to do this with gather()? 
#although Stephen said have a column per instrument may be "philosophically" correct
JKADS_air_low_var_co <- subset(JKADS_air, co_stdev_s5099 <100)
mean(JKADS_air_low_var_co$co_C_diff ) # I wish to learn a better way to do this 
JKADS_air_low_var_n2o <- subset(JKADS_air, n2o_stdev_s5099 <10)
mean(JKADS_air_low_var_n2o$n2o_C_diff )


summary(JKADS_air$h2o_s5100)
summary(JKADS_air$h2o_s5099)
