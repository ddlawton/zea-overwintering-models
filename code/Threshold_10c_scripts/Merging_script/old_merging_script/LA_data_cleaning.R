#####
# Cleaning LA Data
#
#####
rm(list=ls())

library(readxl)
library(tidyverse)
library(measurements)
library(lubridate)
library(rnaturalearth)
library(ggpubr)
library(sp)
library(sf)
library("rnaturalearth")
library(data.table)

#Function to find week mid-point
midweek <- function(year, month, month_week) {
  # first day of the month
  date <- paste(year, month, "1st") %>% lubridate::ymd()
  # return middle date of the month week
  date + 7 * month_week - 4
}

dat <- as_tibble(read_excel("data/raw/LA_data/Macon Ridge H. Zea Data.xlsx",skip=2))


names(dat) <- paste(names(dat), dat[1, ], sep = "_")
dat[-1,]
names(dat)
dat <- (dat %>% rename(Month = ...1_NA, Week = ...2_NA))[2:22,] %>%
  fill(Month,.direction="down") 


dat2 <- dat %>% tidyr::pivot_longer(cols = c(ends_with("_Trap 1"), ends_with("_Trap 2")),
                      names_to = c("Year", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Year = gsub("\\...*","",Year)) %>%
  pivot_longer(cols=4:5,names_to = "Trap",values_to = "count") %>% drop_na(count) %>%
  mutate(
    Latitude = case_when(
      Trap == "Trap 1" ~ 32.139902,
      Trap == "Trap 2" ~ 32.136427
    ),
    Longitude = case_when(
      Trap == "Trap 1" ~ -91.697927,
      Trap == "Trap 2" ~ -91.701078
    ),
    State = rep("Louisiana",nrow(.)),
    Week = gsub("wk ","",Week),
    midpoint = midweek(as.integer(Year), as.character(Month), as.integer(Week))) %>%
  mutate(Location = fct_recode(Trap, "LA_Trap_1" = "Trap 1","LA_Trap_2" = "Trap 2"),
         Date = as.Date(midpoint),
         CEW = replace_na(as.numeric(count),0)) %>%
  select(Location, Date, CEW, State, Latitude,Longitude)



#now combining together with the other state data

WI_NC_MS_NY_TN <- as.tibble(read.csv("data/raw/National_data_combined/WI_NC_MS_NY_TN_Jul9.csv")) %>%
  mutate(Date = as.Date(Date),
         CEW = as.numeric(CEW),
         CEW = replace_na(CEW,0)) 



WI_NC_MS_NY_TN_LA <- bind_rows(WI_NC_MS_NY_TN,dat2) 

WI_NC_MS_NY_TN_LA$Date <- as.Date(WI_NC_MS_NY_TN_LA$Date)

summary(WI_NC_MS_NY_TN_LA)

#checking to confirm spatial distribution

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")
name(states)

ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-94,-70), ylim = c(30,47), expand = FALSE)+
  geom_point(data = (WI_NC_MS_NY_TN_LA), aes(x=Longitude,y=Latitude)) +
  theme_pubr() + theme(legend.position = "none")



WI_NC_MS_NY_TN_LA <- WI_NC_MS_NY_TN_LA %>% mutate(
  latitude = Latitude,
  longitude = Longitude,
  unix = as.numeric(anytime(Date))*1000) %>%
  drop_na(latitude)

write_csv(WI_NC_MS_NY_TN_LA, file = "data/raw/National_data_combined/WI_NC_MS_NY_TN_LA_Jul12.csv")  
