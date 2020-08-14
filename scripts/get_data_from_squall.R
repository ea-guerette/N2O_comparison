#Updating this SQUALL script to remove some of the repetition across the project 
#will be highest-level script to connect to database and define functions: 
#get_ family of functions (with ability to select UANs and flags 
#get_all returning a list of all three tables, can select UANs and flags   
#get_uans - goes across all 3 tables and returns a df containing data for selected UANs - useful for anything - yes, good base for rest of code, will need tweaking but OK
#get_Picarro_standards (build on get_uans(), summarising and adding formatting) - TODO
#get_Picarro_drifts (in development) - TODO 

library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)



#establish connection to database 
con <- dbConnect(odbc::odbc(), 
                 .connection_string = 'driver={SQL Server};server=squall-cdc.it.csiro.au;database=gaslab;trusted_connection=true')

#dbDisconnect(con)

#define functions 
#UAN_list <- c("20120148", "20150061", "20160337", "20190214", "20190543", "20100532", "20100778", "20160484", "999479")

#grab all 'good' data from SHIM-1 table, based on heights 
get_s1 <- function(uan = c(), flag = 0) {
  q_shim <- tbl(con, "SHIM-1")  %>% 
  select(- c(`Decimal Year`,  `N2O ar mixing ratio`, `N2O ar mixing ratio sd`)) %>%  #`N2O flag`,
    filter(`N2O flag` %in% flag) 
 
   if(is.null(uan)){
    q_shim <-  q_shim 
  } else{
    q_shim <- q_shim   %>% 
        filter(UAN %in% uan)
  }
  
  shim_data <- collect(q_shim)  %>% rename(file = `File #`, date = `Date Started`) #, n = `N2O # aliquots`, N2O = `N2O ht mixing ratio`, sd = `N2O ht mixing ratio sd`) %>% 
  #mutate(instrument = "S1")
  shim_data
}

#grab all 'good' data from CARLE-3 table, CO2 based on areas, CH4 based on heights 
get_c3 <- function(uan = c(), flag = 0) {
  q_c3 <- tbl(con, "CARLE-3")  %>% 
  select(- c(`Decimal Year`, `CH4 ar mixing ratio`, `CH4 ar mixing ratio sd`, `CO2 ht mixing ratio`,`CO2 ht mixing ratio sd`)) %>%  #`CH4 flag`, `CO2 flag`
  filter(`CO2 flag` %in% flag  ) #& `CH4 flag` %in% flag #not needed, data matched per flag in database 
  if(is.null(uan)){
    q_c3 <-  q_c3 
  } else{
    q_c3 <- q_c3   %>% 
      filter(UAN %in% uan)
  }
  
  c3_data <- collect(q_c3) %>% rename(file = `File #`, date = `Date Started`)
  c3_data
} 

#grab all 'good' data from RGA3-1, based on heights for both CO and H2
get_r1 <- function(uan = c(), flag = 0) {
  q_r1 <- tbl(con, "RGA3-1")  %>% 
    select(- c(`Decimal Year`, `H2 ar mixing ratio`, `H2 ar mixing ratio sd`, `CO ar mixing ratio`,`CO ar mixing ratio sd`)) %>% #`H2 flag`, `CO flag`
    filter(`CO flag` %in% flag ) 
  
  if(is.null(uan)){
    q_r1 <-  q_r1 
  } else{
    q_r1 <- q_r1   %>% 
      filter(UAN %in% uan)
  }
  r1_data <- collect(q_r1) %>% rename(file = `File #`, date = `Date Started`)
  r1_data
}

# create a get_all function  
get_all <- function(uan = c(), flag = 0) {
  s1 <- get_s1(uan, flag)
  c3 <- get_c3(uan, flag)
  r1 <- get_r1(uan, flag)
  return(list(s1,c3,r1))
}

## get_uans 
#similar to get_all, but needs a list of UANs and formats into a df 
get_uans <- function(uan = c(), flag = 0) {
  if(is.null(uan)) {return(print("Need to specify a list of UANs"))}
  else{
  s1 <- get_s1(uan, flag)
  c3 <- get_c3(uan, flag)
  r1 <- get_r1(uan, flag)
 df <- full_join(c3,r1, by = c("UAN", "date", "file")) 
 df <- full_join(df, s1, by = c("UAN", "date", "file")) %>% mutate(UAN = as.character(UAN))
 return(df)}
}



