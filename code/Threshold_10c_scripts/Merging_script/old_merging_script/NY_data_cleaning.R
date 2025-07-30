#####
# Cleaning NY Data
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


dat <- read_excel("data/raw/NY_data/NYSIPM trap data 1994-2020 (1).xlsx",sheet = 1) %>% drop_na(Lat) %>%
  rowid_to_column(var="ID")

#figuring out coords

weird_coords <- dat %>% filter(!grepl('\"N', Lat)) %>% filter(!grepl('\'', Lat))
other_coords <- dat %>% filter(grepl('\"N', Lat)) %>% filter(grepl('\'', Lat))

weird_coords$Lat <- stringi::stri_sub(weird_coords$Lat,1,7)
weird_coords$Long <- as.numeric(stringi::stri_sub(weird_coords$Long,1,7)) * -1

other_coords$Lat <- gsub(" ", "", other_coords$Lat, fixed = TRUE)
other_coords$Lat <- gsub("°", "d", other_coords$Lat, fixed = TRUE)

other_coords$Long <- gsub(" ", "", other_coords$Long, fixed = TRUE)
other_coords$Long <- gsub("°", "d", other_coords$Long, fixed = TRUE)

other_coords$Lat <- as.numeric(char2dms(other_coords$Lat))
other_coords$Long <- as.numeric(char2dms(other_coords$Long))


coords_combined <- rbind(weird_coords,other_coords)

leftover_dat <- dat %>% anti_join(coords_combined,by="ID")

leftover_dat$Lat <- gsub("\'\'", "\"", leftover_dat$Lat, fixed = TRUE)
leftover_dat$Lat <- gsub("° ", "d", leftover_dat$Lat, fixed = TRUE)

leftover_dat$Long <- gsub("\'\'", "\"", leftover_dat$Long, fixed = TRUE)
leftover_dat$Long <- gsub("° ", "d", leftover_dat$Long, fixed = TRUE)

leftover_dat$Lat <- as.numeric(char2dms(leftover_dat$Lat))
leftover_dat$Long <- as.numeric(char2dms(leftover_dat$Long))

coords_combined <- rbind(weird_coords,other_coords,leftover_dat)


coords_combined <- coords_combined %>% select(Site, Lat, Long, Date,CEW) %>%
  rename(Location = "Site", Latitude = "Lat", Longitude = 'Long') %>%
  mutate(CEW = replace_na(CEW,0),
         State = rep("New York",nrow(.)))

coords_combined$CEW <- (as.numeric(coords_combined$CEW))
coords_combined$Latitude <- (as.numeric(coords_combined$Latitude))
coords_combined$Longitude <- (as.numeric(coords_combined$Longitude))

summary(coords_combined)
#now combining together with the other state data

WI_NC_MS <- as.tibble(read.csv("data/raw/National_data_combined/WI_NC_MS_NY_Jul8.csv")) %>%
  mutate(Date = as.Date(Date),
         CEW = as.numeric(CEW),
         CEW = replace_na(CEW,0)) %>%
  filter(State != "New York")



WI_NC_MS_NY <- bind_rows(WI_NC_MS,coords_combined) 

WI_NC_MS_NY$Date <- as.Date(WI_NC_MS_NY$Date)

WI_NC_MS_NY %>% filter(State == "New York")

summary(WI_NC_MS_NY)

#checking to confirm spatial distribution

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")
name(states)

ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-94,-70), ylim = c(30,47), expand = FALSE)+
  geom_point(data = (WI_NC_MS_NY), aes(x=Longitude,y=Latitude)) +
  theme_pubr() + theme(legend.position = "none")


write_csv(WI_NC_MS_NY, file = "data/raw/National_data_combined/WI_NC_MS_NY_Jul9.csv")
