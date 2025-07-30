#####
# Cleaning AZ Data
#
#####
rm(list=ls())
library(anytime)
library(lubridate)
#1998 - 2002

yuma_valley_1 <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,1:3]

yuma_valley_1 <- yuma_valley_1 %>%
  select(1,3) %>%
  mutate(Location = rep("Yuma_Valley_1",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Yuma Valley -1`))),
         CEW = ...3) %>%
  select(3:5)






yuma_valley_2 <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,5:7]

yuma_valley_2 <- yuma_valley_2 %>%
  select(1,3)  %>%
  mutate(Location = rep("Yuma_Valley_2",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Yuma Valley -2`))),
         CEW = ...7) %>%
  select(3:5)


gila_valley <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,9:11]

gila_valley <- gila_valley %>%
  select(1,3)  %>%
  mutate(Location = rep("Gila_Valley",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Gila Valley`))),
         CEW = ...11) %>%
  select(3:5)

dome_valley <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,13:15]

dome_valley <- dome_valley %>%
  select(1,3) %>%
  mutate(Location = rep("Dome_Valley",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Dome Valley`))),
         CEW = ...15) %>%
  select(3:5)

combined_dat <- rbind(yuma_valley_1,yuma_valley_2,gila_valley, dome_valley) %>% drop_na(CEW) %>%
  filter(CEW != ".") %>%
  mutate(CEW=as.numeric(CEW))

summ
###2013 - 2021 

dat_13_21 <- as_tibble(read_excel("data/raw/AZ_data/AILRC_Trap Counts_H. zea_Yuma AZ 2013-2021 (1).xlsx",skip=2)) %>% 
  tidyr::fill(`2013-14`,.direction=c("down")) %>%
  mutate(Year = replace_na(`2013-14`,"2013-2014")) %>%
  select(2,4:19,23) %>% drop_na(Date)


weird_cols <- dat_13_21[1:17,] %>%
  mutate(Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", Date))))

other_cols <- dat_13_21[18:177,] %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30"))
names(dat_13_21)

dat_13_21 <- rbind(weird_cols,other_cols) %>%
  mutate_at(c(2:17),as.character) %>%
  pivot_longer(cols=2:17,names_to = "Location",values_to = "CEW") %>%
  filter(CEW != "-") %>%
  mutate(CEW = as.numeric(CEW)) %>% select(1,3,4)


Az_combined_dat <- rbind(dat_13_21, combined_dat)
