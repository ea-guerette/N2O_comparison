#this is an example taken from https://db.rstudio.com/getting-started/database-queries/ 
#You can write your code in dplyr syntax, and dplyr will translate your code into SQL. 
#There are several benefits to writing queries in dplyr syntax: you can keep the same consistent 
#language both for R objects and database tables, no knowledge of SQL or the specific SQL variant is required, 
#and you can take advantage of the fact that dplyr uses lazy evaluation. dplyr syntax is easy to read, 
#but you can always inspect the SQL translation with the show_query() function.

#In this example, we will query bank data in an Oracle database. 
#We connect to the database by using the DBI and odbc packages. 
#This specific connection requires a database driver and a data source name (DSN) 
#that have both been configured by the system administrator. 
#Your connection might use another method.
#install.packages("odbc")
#install.packages("DBI")
#install.packages("dbplyr")

library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)
#sort(unique(odbcListDrivers()[[1]]))

#con <- dbConnect(odbc(),
#                 Driver = "ODBC Driver 17 for SQL Server",
#                 Server = "squall-cdc.it.csiro.au",
#                 Database = "gaslab",
#                 UID = "NEXUS\\gue02h",
#                 PWD = rstudioapi::askForPassword("Enter password"),
#                 Port = 1433)
#the above DOES NOT WORK :/ 

#this works: 
con <- dbConnect(odbc::odbc(), 
                 .connection_string = 'driver={SQL Server};server=squall-cdc.it.csiro.au;database=gaslab;trusted_connection=true')
#this also works:
#con <- dbConnect(odbc::odbc(), "gaslab") #this is using a DSN - had to set it up in Windows 

#using odbc() 
dbListTables(con) #this lists all tables in the database
#data <- dbReadTable(con, "SHIM-1")
#as_tibble(data)

#example usring dplyr or dbplyr?
#q1 <- tbl(con, "bank") %>%
#  group_by(month_idx, year, month) %>%
#  summarise(
#    subscribe = sum(ifelse(term_deposit == "yes", 1, 0)),
#    total = n())
#show_query(q1)

q_shim <- tbl(con, "SHIM-1")  %>% ###aha this actually works, the list thing was throwing me off
  filter(`N2O flag` == 0) %>% 
  select(- c(`Decimal Year`, `N2O flag`, `N2O ar mixing ratio`, `N2O ar mixing ratio sd`))  
show_query(q_shim)

shim_data <- collect(q_shim)  %>% rename(file = `File #`, date = `Date Started`, n = `N2O # aliquots`, N2O = `N2O ht mixing ratio`, sd = `N2O ht mixing ratio sd`) %>% 
  mutate(instrument = "S1")


q_c3 <- tbl(con, "CARLE-3")  %>% 
  filter(`CO2 flag` == 0) %>% 
  select(- c(`Decimal Year`, `CH4 flag`,`CH4 ar mixing ratio`, `CH4 ar mixing ratio sd`, `CO2 flag`,`CO2 ht mixing ratio`,`CO2 ht mixing ratio sd`))
  
c3_data <- collect(q_c3) %>% rename(file = `File #`, date = `Date Started`)
#will rename more stuff once I know what I am doing

q_r1 <- tbl(con, "RGA3-1")  %>% 
  filter(`CO flag` == 0) %>% 
  select(- c(`Decimal Year`, `H2 flag`,`H2 ar mixing ratio`, `H2 ar mixing ratio sd`, `CO flag`,`CO ar mixing ratio`,`CO ar mixing ratio sd`))

r1_data <- collect(q_r1) %>% rename(file = `File #`, date = `Date Started`)
#will rename more stuff once I know what I am doing