#This is code to compare PDD, ECD, SHIM, RVI picarro results - accuracy test 

#use tools we are learning in the course: 
library(tidyverse)

#need to read in data files: 

#read in PDD summary report - extract C_HT
pdd_n2o_tank <- read_fwf("data/summary_report_pdd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(147,153), "sd" = c(156,162), "n" = c(163,168)),
                    skip = 4, trim_ws = TRUE ) %>% mutate(instrument = "PDD")

#do the same for the ECD summary report but extract C_A 
ecd_n2o_tank <- read_fwf("data/summary_report_ecd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(169,179), "sd" = c(180,187), "n" = c(188,191)),
                    skip = 4, trim_ws = TRUE )  %>% mutate(instrument = "ECD")


#read in RVI tank data, using the tools taught in the course: 
#pull out only the info you need for the N2O comparison - extract N2O_C only
rvi_n2o_tank <- read_fwf("data/rvi_n2o_co_all", 
                    fwf_cols("date"= c(1,6), "time" = c(8,11), "type" =c(12,22), "UAN" = c(23,35), "N2O_C" = c(152, 161), "flag" = c(162,162) ) , 
                    skip = 2, trim_ws = TRUE) %>% 
  filter(type == "tank", flag %in% NA)  %>% 
  group_by(UAN, date) %>% summarise(N2O = mean(N2O_C), sd = sd(N2O_C, na.rm = T)) %>% mutate(instrument = "JKADS5100")
#this is good, but how can I summarise the character columns as well?

#create UAN list to filter SHIM results with 
UAN_list <- str_sub(levels(as.factor(pdd_n2o_tank$UAN)), start= 4 )

tank_results <- bind_rows(pdd_n2o_tank, ecd_n2o_tank,rvi_n2o_tank)

#read in SHIM data for N2O 
#shim_n2o_tank <- read_table2(file = "data/csa_02D0_event.n2o", skip =25,  col_names =  T,   
                  # na = "-999.990", comment = "") %>% 
                  #filter(uan %in% UAN_list) %>%  select(id, uan ,formula, value, inst)  %>% #, flag)
                  #unite(formula, inst, col = "name", sep = "_")
            

#shim_co2_tank <- read_table2(file = "data/csa_02D0_event.co2", skip =25,  col_names =  T,  
                #  na = "-999.990", comment = "")  %>% 
                #   filter(uan %in% UAN_list)  %>%  select(id, uan ,formula, value, inst) %>% #, flag)
                # unite(formula, inst, col = "name", sep = "_")

#shim_tank <- bind_rows(shim_co2_tank, shim_n2o_tank) %>% spread(key = name, value = value) %>% mutate(uan = paste0("UAN", uan))

#Use shim_data, select UANs of interest, group by UAN, calculate weighted mean and some error value (try weighted sd)
shim_tanks <- filter(shim_data, UAN %in% UAN_list) %>% group_by(UAN) %>% mutate(sum_N2O = N2O*n, sum_sd = sd *n) %>% summarise(n_S1 = sum(n, na.rm = T), N2O_S1 = sum(sum_N2O, na.rm = T)/sum(n, na.rm = T), sd_S1 = sum(sum_sd, na.rm = T)/sum(n, na.rm = T))

c3_tanks <- filter(c3_data, UAN %in% UAN_list) %>% group_by(UAN) %>% 
  mutate(sum_co2 = `CO2 ar mixing ratio`* `CO2 # aliquots`, sum_ch4 = `CH4 ht mixing ratio` *`CH4 # aliquots`) %>% 
  summarise(CH4 = sum(sum_ch4, na.rm = T)/sum(`CH4 # aliquots`, na.rm = T),CO2 = sum(sum_co2, na.rm = T)/sum(`CO2 # aliquots`, na.rm = T))
db_tanks <- full_join(c3_tanks, shim_tanks) %>% mutate(UAN = as.character(UAN)) %>% mutate(UAN = paste0("UAN", UAN))

tanks <- left_join(db_tanks, tank_results)  %>% mutate(diff = N2O - N2O_S1)

library(lattice)

xyplot(N2O ~N2O_S1, groups = instrument, data = tanks, 
       pch = 16, auto.key = T)
#rvi reads high! and we are missing tanks + need to flag some data 

xyplot(diff ~N2O_S1, groups = instrument, data = tanks, 
       pch = 16, auto.key = T)
xyplot(diff ~CO2_C3, groups = instrument, data = tanks, 
       pch = 16, auto.key = T, 
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(h = c(0, 0.1, -0.1), lty = c(2, 3,3))}
       )

#explore ggplot2 - I think they have handy error bar features 

#make a wide dataset 

test <- spread(tanks, key = instrument, value = N2O ) #hmmm need to spread more than just N2O values (also sd, etc. )
#maybe need to gather first, then spread