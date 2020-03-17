#This is code to compare PDD, ECD, SHIM, RVI picarro results - accuracy test 

#use tools we are learning in the course: 
library(tidyverse)

#need to read in data files: 

#read in PDD summary report - extract C_HT
pdd_n2o <- read_fwf("data/summary_report_pdd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(147,153), "sd" = c(156,162), "n" = c(163,168)),
                    skip = 4, trim_ws = TRUE ) 

#do the same for the ECD summary report but extract C_A 
ecd_n2o <- read_fwf("data/summary_report_ecd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(169,179), "sd" = c(180,187), "n" = c(188,191)),
                    skip = 4, trim_ws = TRUE ) 


#read in RVI tank data, using the tools taught in the course: 
#pull out only the info you need for the N2O comparison - extract N2O_C only
rvi_n2o <- read_fwf("data/rvi_n2o_co_all", 
                    fwf_cols("date"= c(1,6), "time" = c(8,11), "type" =c(12,22), "UAN" = c(23,35), "N2O_C" = c(152, 161), "flag" = c(162,162) ) , 
                    skip = 2, trim_ws = TRUE) %>% 
  filter(type == "tank", flag %in% NA)  %>% 
  group_by(UAN) %>% summarise(N2O = mean(N2O_C), sd = sd(N2O_C, na.rm = T))
#this is good, but how can I summarise the character columns as well?

#create UAN list to filter SHIM results with 
UAN_list <- levels(as.factor(pdd_n2o$UAN))



