#####
#  PA
#   Data
#####
rm(list-ls())

library(XML)
library(xml2)
library(readxl)
library(tidyverse)
library(measurements)
library(lubridate)
library(rnaturalearth)
library(ggpubr)
library(sp)
library(sf)
library("rnaturalearth")

#here is all the XML shit

cew_06 <- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2006.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value) %>% mutate( Year =
  rep(2006,nrow(.)))

cew_05<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2005.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value) %>% mutate( Year =
  rep(2005,nrow(.)))

cew_04<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2004.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value) %>% mutate( Year =
  rep(2004,nrow(.)))

cew_03<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2003.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value) %>% mutate( Year =
  rep(2003,nrow(.)))

cew_02<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2002.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value) %>% mutate( Year =
  rep(2002,nrow(.)))

cew_01<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2001.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value)  %>% mutate( Year =
  rep(2001,nrow(.)))

cew_00<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew2000.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value)  %>% mutate( Year =
  rep(2000,nrow(.)))

cew_99<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew1999.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value)  %>% mutate( Year =
  rep(1999,nrow(.)))

cew_98<- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/catchcew1998.xml") %>%
  pivot_longer(cols=starts_with("farm")) %>% drop_na(value)  %>% 
  mutate( Year = rep(1998,nrow(.)))

coords <- xmlToDataFrame("data/raw/Pest_watch_data/PW_old/FarmsCoords2.xml") %>%
  dplyr::select(1,5,6,State)


Y98_06 <- rbind(cew_06,cew_05,cew_05,cew_03,cew_02,cew_01,cew_00,cew_98,cew_99) %>%
  separate(name, 
           into = c("text", "ID"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) %>%
  left_join(coords,by="ID") %>% dplyr::select(1,2,4:8,State) %>%
  mutate(
    Month = as.integer(Month),
    Day = as.integer(Day),
    ID = as.factor(ID),
    CEW = as.integer(value),
    Year = as.integer(Year),
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude) * -1
  ) %>%
 # drop_na(Latitude) %>%
  mutate(Date = as.Date(paste(Year,Month,Day,sep="-"))) %>%
  dplyr::select(3,6:10)

print((Y98_06 %>% filter(is.na(Latitude)) %>% group_by(ID) %>% summarize(total = n()))$ID) # these are the records that could not be matched with the farm coordinates

summary(Y98_06)


# Now the 2007-2021 data

y2007_2021 <- as_tibble(read.csv("data/raw/Pest_watch_data/cew_all_2007_2021.csv")) %>%
  dplyr::select(1,3:4,7,9,11) %>%
  mutate(
    ID = as.factor(farmid),
    Latitude = lat,
    Longitude = lon,
    State = as.character(st),
    Date = as.Date(catchdate),
    CEW = as.integer(catch)) %>%
  dplyr::select(7:12)

combined_dat <- rbind(Y98_06,y2007_2021) %>%
  rename(Location = "ID")

combined_dat$State <- state.name[match(as.factor(combined_dat$State),state.abb)]



### add Iowa Data

IA <- read_xlsx("data/raw/IA_data/CEW Data_Iowa_2020 (1).xlsx") %>%
  mutate(Location = paste(`Location Name`,County, sep="_"),
         Date = as.Date(`Observation Date`),
         CEW = `CEW Counts`,
         State = rep("Iowa",nrow(.))) %>%
  dplyr::select(3:4,7:10)
         



#checking to confirm spatial distribution

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")
name(states)


WI_NC_MS_NY_TN_LA <- read.csv("data/raw/National_data_combined/WI_NC_MS_NY_TN_LA_Jul12.csv")  
WI_NC_MS_NY_TN_LA$Date <- as.Date(WI_NC_MS_NY_TN_LA$Date)

National_dat_July13 <- WI_NC_MS_NY_TN_LA %>% dplyr::select(1:6) %>%
  rbind(combined_dat) %>% rbind(IA) %>% filter(Location != "4963") %>%
  filter(Longitude < -60) %>% filter(Latitude > 20)# %>%
  #distinct(across(2:5))


summary(as.Date(combined_dat$Date))

ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-108,-65), ylim = c(25,49), expand = FALSE)+
  geom_point(data = (National_dat_July13), aes(x=Longitude,y=Latitude)) +
  theme_pubr() + theme(legend.position = "none")

write.csv(National_dat_July13,file="data/raw/National_data_combined/National_dat_July13.csv")


summary(National_dat_July13)
