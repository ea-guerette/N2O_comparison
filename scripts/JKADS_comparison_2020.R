# new cleaned up script to compare JKADS while they are both at ASA, 2020
# want to use this to show Picarro how bad S5100 is compared to S5099

#will need to sort out CO results - 
#water test results 
#CO2 dependence of N2O 

#for water dependence, need to read in everything (CO wet, dry, N2O wet, dry) - reuse code I wrote for the CGO unit 

library(tidyverse)

instruments <- c("JKADS5100", "JKADS5099")
df_names <- c("s5100", "s5099")
paths <- c("data/rvi_n2o_co_all" , "data/jkads5099_n2o_co_all")
tanks <- list()
water <- list()

file_widths <- c(6,5,10,13,13,5,10,10,12,10,10,1,9,1,9,1,7,6,13,10,1,9,1,9,1,7,6,13,10,1,9,1,9,1,7,6,13)
col_cl <- c("character","character","factor","factor","factor","factor", "numeric", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric",
            "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric", "numeric","numeric")

for (i in 1:length(instruments)){
data <- read.fwf(file = paths[i], widths= file_widths, header = FALSE, skip = 2, na.strings = "nan", strip.white=T,
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

df$instrument <- instruments[i]

water[[i]] <- df %>% filter(type == "h2otest")
tanks[[i]] <- df %>% filter(type %in% "tank")

 
#assign to proper name 
assign(df_names[i], df)
}

#the steps above take a while - maybe better not to include them in the document itself? 
water <-bind_rows(water)
tanks <- bind_rows(tanks)
#not doing this with air because I need to merge by date to enable a comparison 

save(water, file = "rdata/h2otest_data.RData")
save(tanks, file = "rdata/tank_data.RData")

save(s5100, file = "rdata/JKADS5100_data")
save(s5099, file = "rdata/JKADS5099_data")


