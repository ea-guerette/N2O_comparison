#This is code to compare PDD, ECD, SHIM, RVI picarro results - accuracy test 

#use tools we are learning in the course: 
library(tidyverse)

#need to read in data files: 

#read in PDD summary report - extract C_HT
pdd_n2o <- read_fwf("data/summary_report_pdd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(147,153), "sd" = c(156,162), "n" = c(163,168)),
                    skip = 4, trim_ws = T ) 

#do the same for the ECD summary report but extract C_A 
ecd_n2o <- read_fwf("data/summary_report_ecd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(169,179), "sd" = c(180,187), "n" = c(188,191)),
                    skip = 4, trim_ws = T ) 







#create UAN list to filter SHIM results with 
UAN_list <- levels(as.factor(pdd_n2o$UAN))


