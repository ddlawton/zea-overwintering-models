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
library(fuzzyjoin)

# Going year by year to combine it all

coords <-  read_excel("data/raw/TN_data/CAPS GPS and Counties - Stewart.xlsx") %>% fill(c(`Site #`,Location)) %>%
  select(County, Area, Location) %>% distinct()

#2004 Dat


dat_2004 <- read_excel("data/raw/TN_data/Moth Trapping04.xls",sheet = 2)[40:60,]

names(dat_2004) <- dat_2004 %>% slice(1) %>% unlist()

dat_2004_2 <- dat_2004 %>% slice(-1) %>% pivot_longer(cols= 2:14,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = gsub("[()]", "", Area),
         Area = fct_recode(Area, "WTREC" = "South"),
         Area = fct_recode(Area, "Brownsville" = "Brownville"),
         Area = fct_recode(Area, "19 W" = "19"),
         Area = fct_recode(Area, "Mt. Carmel" = "Mt."),
         Area = fct_recode(Area, "Maury City" = "Maury"),
         Area = fct_recode(Area, "Ag Center" = "Ag")) %>%
  left_join(coords,by=c('County','Area')) #this is where I have to leave off. Unsure about missing coords

# 2005 Dat

### No 2005 data

# 2006 Data

dat_2006 <- read_excel("data/raw/TN_data/Moth Trap Data06 Entered.xls",sheet = 1)[27:47,1:19]

names(dat_2006) <- dat_2006 %>% slice(1) %>% unlist()

dat_2006_2 <- dat_2006 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:19,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep="-",into=c("County","Area")) %>% 
  mutate(County = gsub(" ", "", County), Area = gsub(" ", "", Area),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), Area = case_when(
           County == 'Tipton' ~ "W",
           County == 'Hardeman' ~ "Bolivar",
           County == 'Fayette' ~ "Whiteville",
           County == 'Madison' ~ "North",
           County == 'Carroll' ~ "W",
           TRUE ~ as.character(Area)),
         County = fct_recode(County,"Lauderdale" = "Lauder")) %>% 
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.


# 2007 data


dat_2007 <- read_excel("data/raw/TN_data/Moth Traps 07.xls",sheet = 1)[26:45,1:17]


names(dat_2007) <- dat_2007 %>% slice(1) %>% unlist()

dat_2007_2 <- dat_2007 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:17,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep="-",into=c("County","Area")) %>% 
  mutate(County = gsub(" ", "", County), Area = gsub(" ", "", Area),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), Area = case_when(
                             County == 'Tipton' ~ "W",
                             County == 'Hardeman' ~ "Bolivar",
                             County == 'Fayette' ~ "Whiteville",
                             County == 'Madison' ~ "North",
                             County == 'Carroll' ~ "W",
                             County == 'Dyer' ~ "B",
                             TRUE ~ as.character(Area)),
         County = fct_recode(County,"Lauderdale" = "Lauder")) %>% 
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.


# 2008 data


dat_2008 <- read_excel("data/raw/TN_data/Moth Traps 08.xls",sheet = 1)[26:45,1:17]


names(dat_2008) <- dat_2008 %>% slice(1) %>% unlist()

dat_2008_2 <- dat_2008 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:17,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep="-",into=c("County","Area")) %>% 
  mutate(County = gsub(" ", "", County), Area = gsub(" ", "", Area),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), Area = case_when(
                             County == 'Tipton' ~ "W",
                             County == 'Hardeman' ~ "Bolivar",
                             County == 'Fayette' ~ "Whiteville",
                             County == 'Madison' ~ "North",
                             County == 'Carroll' ~ "W",
                             County == 'Dyer' ~ "B",
                             TRUE ~ as.character(Area)),
         County = fct_recode(County,"Lauderdale" = "Lauder")) %>% 
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

# 2009 data


dat_2009 <- read_excel("data/raw/TN_data/Moth Traps 09.xlsx",sheet = 1)[26:45,1:17]


names(dat_2009) <- dat_2009 %>% slice(1) %>% unlist()

dat_2009_2 <- dat_2009 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:17,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep="-",into=c("County","Area")) %>% 
  mutate(County = gsub(" ", "", County), Area = gsub(" ", "", Area),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), Area = case_when(
                             County == 'Tipton' ~ "W",
                             County == 'Hardeman' ~ "Bolivar",
                             County == 'Fayette' ~ "Whiteville",
                             County == 'Madison' ~ "North",
                             County == 'Carroll' ~ "W",
                             County == 'Dyer' ~ "B",
                             TRUE ~ as.character(Area)),
         County = fct_recode(County,"Lauderdale" = "Lauder")) %>% 
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.


# 2010 data


dat_2010 <- read_excel("data/raw/TN_data/Moth Trap Data 2010.xlsx",sheet = 1)[27:46,1:18]


names(dat_2010) <- dat_2010 %>% slice(1) %>% unlist()

dat_2010_2 <- dat_2010 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:18,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(County,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = gsub("[()]", "", Area)) %>%
  mutate(County = gsub(" ", "", County), Area = gsub(" ", "", Area),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), Area = case_when(
                             County == 'Tipton' ~ "W",
                             County == 'Hardeman' ~ "Bolivar",
                             County == 'Fayette' ~ "Whiteville",
                             County == 'Madison' ~ "North",
                             County == 'Carroll' ~ "W",
                             County == 'Dyer' ~ "B",
                             County == 'Crocket' ~ "B",
                             TRUE ~ as.character(Area))) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2011 data


dat_2011 <- read_excel("data/raw/TN_data/2011 MothTrappingData.xlsx",sheet = "Moths")[3:22,1:15]


names(dat_2011) <- dat_2011 %>% slice(1) %>% unlist()

dat_2011_2 <- dat_2011 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2012 data

dat_2012 <- read_excel("data/raw/TN_data/2012 MothTrapping Data.xls",sheet = "Moths")[3:22,1:15]


names(dat_2012) <- dat_2012 %>% slice(1) %>% unlist()

dat_2012_2 <- dat_2012 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2013 data

dat_2013 <- read_excel("data/raw/TN_data/2013 Moth Trapping Data.xls",sheet = "Moths")[3:22,1:15]


names(dat_2013) <- dat_2013 %>% slice(1) %>% unlist()

dat_2013_2 <- dat_2013 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.


#2013 data

dat_2013 <- read_excel("data/raw/TN_data/2013 Moth Trapping Data.xls",sheet = "Moths")[3:22,1:15]


names(dat_2013) <- dat_2013 %>% slice(1) %>% unlist()

dat_2013_2 <- dat_2013 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.
# 2014 data

### missing data

# 2015 Data

dat_2015 <- read_excel("data/raw/TN_data/2015 Moth Trapping Summary.xls",sheet = "Moths")[3:22,1:15]


names(dat_2015) <- dat_2015 %>% slice(1) %>% unlist()

dat_2015_2 <- dat_2015 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

# 2016 Data

dat_2016 <- read_excel("data/raw/TN_data/2016 Moth Trapping Summary.xls",sheet = "Moths")[3:22,1:15]


names(dat_2016) <- dat_2016 %>% slice(1) %>% unlist()

dat_2016_2 <- dat_2016 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2017 data

dat_2017 <- read_excel("data/raw/TN_data/2017 Moth Trapping Summary.xls",sheet = "Moths")[3:22,1:15]


names(dat_2017) <- dat_2017 %>% slice(1) %>% unlist()

dat_2017_2 <- dat_2017 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2018 data

dat_2018 <- read_excel("data/raw/TN_data/2018MothTrappingData.xls",sheet = "Moths")[3:22,1:15]


names(dat_2018) <- dat_2018 %>% slice(1) %>% unlist()

dat_2018_2 <- dat_2018 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2019 data

dat_2019 <- read_excel("data/raw/TN_data/2019MothTrappingData wGraphs.xls",sheet = "Moths")[3:22,1:15]


names(dat_2019) <- dat_2019 %>% slice(1) %>% unlist()

dat_2019_2 <- dat_2019 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.

#2020 data

dat_2020 <- read_excel("data/raw/TN_data/2020 Moth Trapping Data.xls",sheet = "Moths")[3:22,1:15]


names(dat_2020) <- dat_2020 %>% slice(1) %>% unlist()

dat_2020_2 <- dat_2020 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area")) 


#NOW COMBINED THIS SHIT SHOW

combined_dat <- rbind(dat_2004_2,dat_2006_2,dat_2007_2,dat_2008_2,dat_2009_2,dat_2010_2,dat_2011_2,dat_2012_2,
     dat_2013_2,dat_2015_2,dat_2016_2,dat_2017_2,dat_2018_2,dat_2019_2,dat_2020_2)
     
view(combined_dat %>% filter(is.na(Location)) %>% group_by(County, Area) %>%
  dplyr::summarize(count = n()))

# Now adding coords

combined_dat2 <- combined_dat %>% 
  mutate( Location = case_when(
    Area == "Maury City" ~ "35.804169, -89.217252",
    County == "Crockett" & Area == "MC" ~ "35.804169, -89.217252",
    County == "Crockett" & Area == "Millington" ~ "35.804169	-89.217252",
    Area == "MREC" ~ "35.913904, -88.743188",
    Area == "Milan" ~ "35.913904, -88.743188",
    Area == "Somerville" ~ "35.233241, -89.44296",
    TRUE ~ as.character(Location))) %>%
  drop_na(Location) %>%
  separate(Location,sep=",",into=c("Latitude","Longitude")) %>%
  mutate(Longitude = gsub(" ", "", Longitude)) %>%
  filter(CEW != "---") %>% filter(CEW != "*") %>%
  filter(CEW != "stolen") %>% filter(CEW != "(Other)") %>% 
  filter(CEW != ".") %>% 
  mutate(Location = paste(County,Area,sep="_"),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         Location = as.factor(Location),
         CEW = as.numeric(CEW),,
         State = rep("Tennessee", nrow(.)),
         CEW = replace_na(CEW,0)) %>%
  select(3:7)

levels(unique(as.factor(combined_dat2$CEW)))  

#now combining together with the other state data

WI_NC_MS_NY <- as.tibble(read.csv("data/raw/National_data_combined/WI_NC_MS_NY_Jul9.csv")) %>%
  mutate(Date = as.Date(Date),
         CEW = as.numeric(CEW),
         CEW = replace_na(CEW,0)) 



WI_NC_MS_NY_TN <- bind_rows(WI_NC_MS_NY,combined_dat2) 

WI_NC_MS_NY_TN$Date <- as.Date(WI_NC_MS_NY_TN$Date)

summary(WI_NC_MS_NY)

#checking to confirm spatial distribution

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")
name(states)

ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-94,-70), ylim = c(30,47), expand = FALSE)+
  geom_point(data = (WI_NC_MS_NY_TN), aes(x=Longitude,y=Latitude)) +
  theme_pubr() + theme(legend.position = "none")


write_csv(WI_NC_MS_NY_TN, file = "data/raw/National_data_combined/WI_NC_MS_NY_TN_Jul9.csv")

       
                