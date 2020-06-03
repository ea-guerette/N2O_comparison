##Code now copied over from Data_school_report.Rmd


#This is code to compare N2O PDD, ECD, SHIM, RVI picarro results - accuracy test, precision, linearity?, stability of response over time  
#this will need updating with latest data at some point 

source("scripts/get_data_from_squall.R") #need to check this works - also, need to be on VPN if offsite 
#use tools we are learning in the course: 
library(tidyverse)


#need to read in data files: 

#read in PDD summary report - extract C_HT
pdd_n2o_tank <- read_fwf("data/summary_report_pdd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(147,153), "sd" = c(156,162), "n" = c(163,168)),
                    skip = 4, trim_ws = TRUE ) %>% mutate(instrument = "PDD")

#do the same for the ECD summary report but extract C_A 
ecd_n2o_tank <- read_fwf("data/summary_report_ecd",  fwf_cols("UAN" = c(1,20), "date" = c(40,45), "N2O" = c(169,179), "sd" = c(180,187), "n" = c(188,191)),
                    skip = 4, trim_ws = TRUE )  %>% mutate(instrument = "ECD")


#read in JKADS5100 tank data: - no good so leave out of this 
#pull out only the info you need for the N2O comparison - extract N2O_C only
#s5100_n2o_tank <- read_fwf("data/rvi_n2o_co_all", 
#                    fwf_cols("date"= c(1,6), "time" = c(8,11), "type" =c(12,22), "UAN" = c(23,35), "N2O_C" = c(152, 161), "flag" = c(162,162), "N2O_dry" = c(173,181) ) , 
#                    skip = 2, trim_ws = TRUE) %>% 
#  filter(type == "tank", flag %in% NA, str_starts(UAN, "UAN"))  %>% 
#  group_by(UAN, date) %>% summarise(n = n(), N2O = mean(N2O_C), sd = sd(N2O_C, na.rm = T)) %>% mutate(instrument = "JKADS5100")


#read in data from JDAKS5099: 
s5099_n2o_tank <- read_fwf("data/jkads5099_n2o_co_all", 
                           fwf_cols("date"= c(1,6), "type" =c(12,22), "UAN" = c(23,35), "N2O_C" = c(152, 161), "flag" = c(162,162) ) , 
                           skip = 2, trim_ws = TRUE) %>% 
  filter(type == "tank", flag %in% NA, str_starts(UAN, "UAN"))  %>% 
  group_by(UAN, date) %>% summarise( n = n(), N2O = mean(N2O_C), sd = sd(N2O_C, na.rm = T)) %>% mutate(instrument = "JKADS5099")

#create UAN list to filter SHIM results with 

tank_results <- bind_rows(pdd_n2o_tank, ecd_n2o_tank,s5099_n2o_tank) #,s5100_n2o_tank)



UAN_list <- str_sub(levels(as.factor(tank_results$UAN)), start= 4 )

#read in SHIM data for N2O
#Use shim_data, select UANs of interest, group by UAN, calculate weighted mean and some error value (try weighted sd)
squall_tanks <- get_uans(UAN_list) %>% group_by(UAN) %>% 
  mutate(sum_N2O = `N2O ht mixing ratio`*`N2O # aliquots`, sum_sd = `N2O ht mixing ratio sd` *`N2O # aliquots`, 
         sum_co2 = `CO2 ar mixing ratio`* `CO2 # aliquots`, 
         sum_ch4 = `CH4 ht mixing ratio` *`CH4 # aliquots`) %>%
  summarise(    CH4 = sum(sum_ch4, na.rm = T)/sum(`CH4 # aliquots`, na.rm = T), 
                CO2 = sum(sum_co2, na.rm =       T)/sum(`CO2 # aliquots`, na.rm = T),
                n_S1 = sum(`N2O # aliquots`, na.rm = T), N2O_S1 = sum(sum_N2O, na.rm = T)/sum(`N2O # aliquots`, na.rm = T), 
                sd_S1 = sum(sum_sd, na.rm = T)/sum(`N2O # aliquots`, na.rm = T)) %>% 
  mutate(UAN = paste0("UAN", UAN))

squall_tanks$CO2[which(is.na(squall_tanks$CO2))] <- 0.98
#need to manually add a value for CO2 for UAN999479, 
#unless I change my query to select everything, and keep other flags when there are no flag ==0 
#this is what SQUALL does I think in the online UAN query 
#on the todo /nice to have list?

tanks <- left_join(squall_tanks, tank_results, by = "UAN")  %>% mutate(diff = N2O - N2O_S1)

squall_ln <- rename(squall_tanks, n = n_S1, sd = sd_S1, N2O = N2O_S1) %>% mutate(instrument = "SHIMADZU") %>% 
  select(-c(CO2, CH4))
tanks_ln <- bind_rows(squall_ln, tank_results) %>% select(-date)

#sd of tank measurements on each instrument 

tanks_ln  %>%
  ggplot(aes(x = sd)) +
  geom_histogram(col = "white") +
  facet_wrap(~instrument)+ #, scales = "free_x") +
  labs(x = "standard deviation") +
  theme_bw()
#another option:
tanks_ln  %>%
  ggplot(aes(x = sd)) +
  geom_histogram(col ="white") +
  facet_grid(~instrument)+ #, scales = "free_x") +
  labs(x = "standard deviation")+
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"))
    

#not sure the is an effect here, leave this out 
tanks_ln  %>% filter(n>2) %>% 
  ggplot(aes(y = sd, x = N2O)) +
  geom_point() + 
  facet_wrap(~instrument)

#explore accuracy
tanks %>% 
  ggplot(aes(x = N2O, y = diff, colour = instrument)) + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_colour_discrete(name = "New instrument", labels = c("ECD", "JKADS5099", "PDD")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("N"[2] * "O (ppm)")) +
  geom_hline(yintercept = 0)
#or 
tanks %>% 
  ggplot(aes(x = N2O, y = diff, colour = CO2, size = sd)) + 
  geom_point(alpha = 0.5) +
  theme_bw() +
  #scale_colour_discrete(name = "New instrument", labels = c("ECD", "JKADS5099", "PDD")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("N"[2] * "O (ppm)")) +
  geom_hline(yintercept = 0)+
  facet_grid(~instrument)


tanks %>% 
  ggplot(aes(x = CO2, y = diff, colour = instrument)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_colour_discrete(name = "New instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm")
#or 
tanks %>% 
  ggplot(aes(x = CO2, y = diff, colour = N2O,  size = sd)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_bw() +
  #scale_colour_discrete(name = "New instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", col = "dark grey") +
  facet_grid(~instrument)


#or
library(openair)

scatterPlot(tanks, x = "CO2", y = "diff", z = "N2O", type = "instrument", 
            cols = "Blues", pch = 16, cex = 0.5, linear = T, 
            layout = c(3,1), fontsize = 13,  
            ref.y = list(h = 0, lty = 5), main = "",
            xlab = expression("CO" [2] * " (ppm)"), 
            ylab =  expression("N" [2] *"O difference (ppb) (SHIM - new)") )
#because adding equation on ggplot does not look straightfoward 

myBlues <- brewer.pal(9, name = "Blues")
myBlues <- myBlues[4:9]
scatterPlot(tanks, x = "N2O", y = "diff", z = "sd", type = "instrument", 
            cols = myBlues, main = "", layout = c(3,1), alpha = 0.8,
            ref.y = list(h = 0, lty = 5), key.footer = "std. dev.",  
            xlab = expression("N" [2] * "O (ppb)"), 
            ylab =  expression("N" [2] *"O difference (ppb) (SHIM - new)") )
#ideally, fill would be the colour, and col would be a dark colour, so even the pale dots are visible,
#but this is tricky/impossible with scatterPlot 
#I need to code my own continuous colour wrapper function for xyplot... could base it on scatterPlot
      
 







mutate(tanks, lower = (0 - sd_S1), upper = (0 + sd_S1)) %>% filter(instrument !="JKADS5100") %>% 
  ggplot(aes(x = N2O, y = diff, colour = instrument)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("N"[2] * "O (ppm)")) +
  geom_hline(yintercept = 0)

#ECD reads a little low? 
#there is a hint of dependence on N2O concentration - Shim read high at higher values? 
#or my cals don't cover the high end well? (Actually, my cals go higher than the SHIM's)

mutate(tanks, lower = (0 - sd_S1), upper = (0 + sd_S1)) %>% filter(instrument !="JKADS5100") %>% 
  ggplot(aes(x = CO2, y = diff, colour = instrument)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_colour_discrete(name = "New instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0)

#the CO2 dependence explains way more of the scatter 

#try histograms 
tanks %>% 
ggplot(aes(x = sd_S1)) +geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)), colour = "black" ) +
  theme_minimal()

tanks %>% #filter(instrument %in% c("ECD", "PDD")) %>% 
  ggplot(aes(x = sd)) +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)), colour = "black" ) +
  facet_wrap(~instrument, scales = "free_x") +
  theme_bw()


shim_data %>%  filter(n >3) %>% 
ggplot(aes(x=date, y = sd)) + geom_point(colour = "red") +
#  geom_line(aes(y=rollmean(sd, 500, na.pad=TRUE))) +
  theme_bw()
#this is good - for GASLAB report, not DS poster 
#shows steady decline since ~2010
#would be nicer if I could overplot the yearly average - or a rolling mean/median?
#try zoo::rollmean()

shim_data %>% filter(n >3) %>% mutate(year = str_sub(date, start =1, end = 4)) %>% group_by(year) %>% summarise(mean_sd = mean(sd), median_sd = median(sd)) %>% 
  ggplot(aes(x=as.numeric(year), y = median_sd)) + geom_point(colour = "red") +
  theme_bw() + 
  labs(x = "year", 
       y = "sd")

#combine both plots 
mean_shim_sd <- shim_data %>% filter(n >3) %>% mutate(year = str_sub(date, start =1, end = 4)) %>% group_by(year) %>% summarise(mean_sd = mean(sd)) %>% 
  mutate(date = as.POSIXct(year, format= "%Y"))


shim_data %>%  filter(n >3) %>% 
  ggplot(aes(x=date, y = sd)) + geom_point(colour = "black", alpha = 0.5) +
  geom_line(mean_shim_sd, mapping = aes(x = date, y = mean_sd), colour = "red") +
  theme_bw()+ ylim(c(0,2))



hi_sds <- filter(shim_data, sd >1)

arrange(shim_data, date)

#todo: explore ggplot2 - I think they have handy error bar features 
#todo: also check options with lattice 









filter(tanks, instrument == "PDD" & date == "200212") %>% 
ggplot(aes(x = CO2, y = diff, colour = instrument)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0)


filter(tanks, instrument == "PDD") %>% 
  ggplot(aes(x = CO2, y = diff, colour = date)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0)

filter(tanks, instrument == "JKADS5099") %>% 
  ggplot(aes(x = CO2, y = diff, colour = N2O_S1)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm", size = 1, colour = "blue")


summary(lm(diff ~ CO2, data = subset(tanks, instrument == "JKADS5099")))
summary(lm(diff ~ CO2, data = subset(tanks, instrument == "ECD")))
summary(lm(diff ~ CO2, data = subset(tanks, instrument == "PDD")))

filter(tanks, instrument == "ECD") %>% 
  ggplot(aes(x = CO2, y = diff, colour = N2O_S1)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm", size = 1, colour = "blue")

#apply a rough correction of -0.0033*CO2 to SHIM - this says that 0 ppm is the 'right value'
corr_slope = 0.003557
corr_int = -1.12
tanks <- mutate(tanks,N2O_S1_corr = N2O_S1 - corr_slope*CO2, diff_corr = N2O - N2O_S1_corr)
tanks <- mutate(tanks,N2O_S1_corr2 = N2O_S1 - corr_int - corr_slope*CO2,  diff_corr2 = N2O - N2O_S1_corr2)

filter(tanks, instrument == "ECD") %>% 
  ggplot(aes(x = CO2, y = diff_corr2, colour = N2O_S1)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm", size = 1, colour = "blue")

filter(tanks, instrument == "JKADS5099") %>% 
  ggplot(aes(x = CO2, y = diff_corr2, colour = N2O_S1)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm", size = 1, colour = "blue")

summary(lm(diff_corr ~ CO2, data = subset(tanks, instrument == "JKADS5099")))
summary(lm(diff_corr ~ CO2, data = subset(tanks, instrument == "ECD")))
summary(lm(diff_corr ~ CO2, data = subset(tanks, instrument == "PDD")))


mutate(tanks, lower = (0 - sd_S1), upper = (0 + sd_S1)) %>% filter(instrument !="JKADS5100") %>% 
  ggplot(aes(x = CO2, y = diff_corr2, colour = instrument)) + 
   geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0)


tanks <- mutate(tanks, rev_diff = N2O_S1 - N2O)

filter(tanks, instrument == "JKADS5099") %>% 
ggplot(aes(x = CO2, y = rev_diff, colour = N2O_S1)) + 
  # geom_errorbar(aes(ymin = lower, ymax = upper ), width = 5, colour = "grey") + 
  geom_point(alpha = 0.5) +
  theme_minimal() +
  #scale_colour_discrete(name = "Instrument", labels = c("ECD", "JKADS5099", "PDD", "SHIM")) +
  labs(y = expression("N"[2] * "O difference (New instrument - SHIM) (ppb) "), x = expression("CO"[2] * " (ppm)")) +
  geom_hline(yintercept = 0) + 
  geom_smooth(method = "lm", size = 1, colour = "blue")

summary(lm(rev_diff ~CO2, data = ecd))
summary(lm(rev_diff ~ CO2, data = subset(tanks, instrument == "JKADS5099")))

#make a wide dataset 
test <- spread(tanks, key = instrument, value = N2O ) #hmmm need to spread more than just N2O values (also sd, etc. )
#maybe need to gather first, then spread



#####################

library(lattice)

xyplot(N2O ~N2O_S1, groups = instrument, data = tanks, 
       pch = 16, auto.key = T)
#rvi reads high! and we are missing tanks + need to flag some data 
mean_sd <- mean(tanks$sd_S1, na.rm = T)

xyplot(diff ~N2O_S1, groups = instrument, data = tanks, 
       pch = 16, auto.key = T,
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(h = c(0, mean_sd, -mean_sd), lty = c(2, 3,3))})



xyplot(diff ~CO2, groups = instrument, data = tanks, 
       pch = 16, auto.key = T, 
       panel = function(x,y,...){
         panel.xyplot(x,y, ...)
         panel.abline(h = c(0, mean_sd, -mean_sd), lty = c(2, 3,3))}
)

test <- mutate(tanks, lower = (0 - sd_S1), upper = (0 + sd_S1))
xyplot(diff ~CO2, groups = instrument, data = test, 
       pch = 16, auto.key = T, 
       panel = function(x,y,...){
         panel.arrows(x,y0=0,x, test$upper, length = 0.04, angle = 90, col = "grey30")
         panel.arrows(x,y0=0,x, test$lower, length = 0.04, angle = 90, col = "grey30")
         panel.abline(h = c(0), col = "grey30")
         panel.xyplot(x,y, ...)
       }
)