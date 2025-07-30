##########
# Data merging
#
##########

rm(list=ls())
library(ggpubr)
library(readxl)
library(tidyverse)
library(lubridate)
library(viridis)


Clean_BL_dat <- read_excel("data/raw/Huseth_data/Cleaned_BlacklightData_2018-03-15.xlsx")
WI_dat <- read.csv("data/raw/Huseth_data/WI-DATCP_BlacklightData.csv")

Clean_BL_dat <- Clean_BL_dat %>%
  mutate_at(vars(9:31), as.numeric) 

Clean_BL_dat[9:31][is.na(Clean_BL_dat[9:31])] <- 0

Clean_BL_dat$date <- as.Date(Clean_BL_dat$date, format="%m/%d/%Y")
Clean_BL_dat$doy <- yday(Clean_BL_dat$date )

# WI clean up
str(WI_dat)

WI_dat <- WI_dat %>%
  mutate_at(vars(13:34), as.numeric) 

WI_dat[13:34][is.na(WI_dat[13:34])] <- 0

WI_dat$dateStart <- as.Date(WI_dat$dateStart, format="%m/%d/%Y")
WI_dat$dateEnd <- as.Date(WI_dat$dateEnd, format="%m/%d/%Y")

WI_dat$date <- WI_dat$dateStart + floor((WI_dat$dateEnd-WI_dat$dateStart)/2) #im just taking the mid point date here

WI_dat <- WI_dat %>% rowwise() %>% 
  mutate(CEW_total = sum(CEW, CEWp,na.rm = TRUE)) 

WI_dat$loc

test <- ((WI_dat %>% group_by(factor(year),factor(loc)) %>% summarize(count_num = sum(CEWp, na.rm = TRUE)) %>%
       pivot_wider(names_from = `factor(loc)`,values_from = count_num)))

view(test)

names(WI_dat)
CEW_dat <- WI_dat %>% select(year,loc,DOY, CEW, CEWp,CEW_total) %>% pivot_longer(cols=4:6,names_to = 'Trap',values_to = 'count')
summary(CEW_dat)


ggplot(CEW_dat,aes(x=(year),y=count,color=Trap)) + geom_smooth()




WI_dat$month <- month(WI_dat$date)

view(WI_dat %>% group_by(year,month) %>%
  summarise(CEWp = sum(CEWp)) %>% 
  pivot_wider(names_from = month, values_from = CEWp))


# Okay combine both dataframes together
WI_dat2 <- WI_dat %>% rename(location = loc, doy=DOY) %>% select(1:6,11,13:35)

Clean_BL_dat2 <- Clean_BL_dat %>% select(1:31)

names(Clean_BL_dat2)
view(WI_dat2)


combined_dat <- bind_rows(WI_dat2,Clean_BL_dat2) # this includes ALL species which are not in ALL states. I am just going to filter out for just Noctuidae


names(combined_dat)

combined_dat <- combined_dat %>% select(1:7,"date","AL","TAW","BCW","CabL","CelL","CEW","CEWp","DCW","FAW",
                                        "FL","GCW","SCW","VCW","WBC")

combined_dat[8:21][is.na(combined_dat[8:21])] <- 0  #Caution, you lose NAs from states that dont have records 

combined_dat <- combined_dat %>% select(1:7,"date","TAW","CEW","CEWp","FAW") %>% filter(date >= "1981-01-01")

combined_dat$Latitude <- combined_dat$latitude
combined_dat$Longitude <- combined_dat$longitude

combined_dat$unix <- as.numeric(as.POSIXct(combined_dat$date)) * 1000

sampled_dat <- combined_dat %>% sample_n(size=500)

summary(combined_dat)
write.csv(combined_dat,file="combined_dat.csv")
write.csv(sampled_dat,file="sampled_dat.csv")

summary(month(combined_dat$date))



