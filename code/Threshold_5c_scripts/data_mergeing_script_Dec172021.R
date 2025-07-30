########################################################
########################################################
########   Data combining to weekly count    ##########
########           July 16 2021                #########
########         Douglas Lawton               ##########
########################################################
########################################################

#######
#  This script is designed to cleanly merge 
#  all Zea data from all sources into the same 
#  analysis ready format
#
#  This means that all records will be averaged into
#  the coarsest temporal division which is week of year
#
#  This will produce weekly averages for CEW catches. We use the week of year
#  as calculated in the ISO 8601 system
#
#  I take a conservative approach to location identification
#
#  This dataset combines both blacklight and pheromone trapping
#  There is a specific column that denotes whether specific observation comes from
#  blight light only, pheromone only, or black light and pheromone 
#  
#######

#####
# Column descriptions
# 
# location = location of trap
# year = year
# woy = week of year (ISO 8601 system)
# CEW_sum = weekly sum of corn ear worm count data
# CEW_std = standard error of weekly average of corn ear worm count data
# longitude = longitude
# latitude = latitude
# date = date (YYYY-MM-DD format)
# trap_type = type of trap used. BL = black light, PH = Pheromone, BL_PH = black light and pheromone, unsure = unsure what trap used
# contact = person of contact for specific observations
# state = The state where observation was recorded
#
#####

rm(list=ls()) # If you need to clear your workspace


# loading in the various packages and functions needed
########
library(XML)
library(xml2)
library(data.table)
library(readxl)
library(tidyverse)
library(measurements)
library(lubridate)
library(rnaturalearth)
library(ggpubr)
library(sp)
library(sf)
library(gtools)
library(rnaturalearth)
library(anytime)
library(rgdal)
library(raster)
library(spatialEco)
library(spdplyr)

#function to find the standard error
std <- function(x) sd(x)/sqrt(length(x))

#function to find the mode of a categorical factor
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Function to find week mid-point
midweek <- function(year, month, month_week) {
  # first day of the month
  date <- paste(year, month, "1st") %>% lubridate::ymd()
  # return middle date of the month week
  date + 7 * month_week - 4
}
########

#North Carolina Data
##### 
# North Carolina Data
# This data was given to me by Anders Huseth

NC <- as_tibble(read.csv("data/raw/Huseth_data/NorthCarolina_Hzea_BlacklightData_2021-07-22.csv")) # North Carolina Data

# this replaces all the '.' to NAs and eventually zeros
NC <- NC %>%
  mutate_at(vars(CEW), as.numeric) %>%
  mutate_at(vars(CEW), replace_na, '0')%>%
  mutate_at(vars(CEW), as.numeric) %>%
  filter(notes != "light bulb out")

# there are locations that are listed as '.' that span different counties. For this data, I will just use the county name

NC <- NC %>% mutate(
  location = case_when(
    location == "." ~ county,
    TRUE ~ location
  )
)

#extracting week of year

#NC$date1 <- as.Date(NC$doy,format="%Y%d%m") #I am not sure what the doy column is for
#NC$year1 <- year(NC$date1)
NC$date <- parse_date_time(NC$date,order="ymd")
#NC <- NC %>% mutate(date = coalesce(date,date1)) %>% dplyr::select(!c(date1)) %>%
  mutate(year = coalesce(year,year1)) %>% dplyr::select(!c(year1))

NC$woy <- isoweek(NC$date)


# Now calculating week of year averages by location
# the hierarchy is as follows Trap > Year > Week

NC_woy1 <- NC %>% drop_na(date) %>% group_by(location,as.factor(year),woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = as.numeric(first(longitude)),latitude = as.numeric(first(latitude)),
            date = first(date)) %>% drop_na(woy,date) %>%
  rename("year" = `as.factor(year)`) %>%
  mutate(year = as.numeric(as.character(year)))

NC_woy1$trap_type <- rep("BL",nrow(NC_woy1))
NC_woy1$contact <- rep("Anders Huseth",nrow(NC_woy1))
NC_woy1$state <- rep("NC",nrow(NC_woy1))



# The following data was given to me by Domininc Reisig

# 2011/2012 Data

dat <- as_tibble(read.csv("data/raw/Reisig_data/2011_2012_dat.csv")) %>%
  rename(Location = "g", Date = 'g.1', CEW = "g.3") %>%
  dplyr::select(1,2,4) %>%
  filter(Location != "") %>%
  mutate(Location = paste(rep('2011_2012_Reisig',nrow(.)),Location,sep="_"),
         CEW = replace_na(CEW, 0),
         Date = as.Date(as.integer(Date), origin = "1899-12-30"))

RM <- dat %>% filter(Location == "2011_2012_Reisig_RM") %>% mutate(
  Latitude = rep("35.895",nrow(.)),
  Longitude = rep("-77.674",nrow(.))
)

TC <- dat %>% filter(Location == "2011_2012_Reisig_TC") %>% mutate(
  Latitude = rep("35.824",nrow(.)),
  Longitude = rep("-76.205",nrow(.))
)


WC <- dat %>% filter(Location == "2011_2012_Reisig_WC") %>% mutate(
  Latitude = rep("35.817",nrow(.)),
  Longitude = rep("-76.598",nrow(.))
)

count_2011_2012 <- rbind(RM,TC,WC)


# 2015 Data

path <- ("data/raw/Reisig_data/2015_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames

coords <- mylist$Sheet1 %>% dplyr::select(1:4) %>% drop_na() %>% separate(
  col=Coords,into=c("Latitude","Longitude"), sep = "(?<=[N])") %>% dplyr::rename("Trap #" = Trap) %>% drop_na() %>%
  mutate(
    Latitude = as.numeric(char2dms(gsub("°","d",Latitude))),
    Longitude = as.numeric(char2dms(gsub("°","d",Longitude)))
  )


count <-  mylist$Sheet1 %>% dplyr::select(6:10)  

count_2015 <- count %>% left_join(coords,by="Trap #") %>% distinct() %>% dplyr::select(!c(`Date set`)) %>% drop_na(Latitude) %>%
  mutate(Location = rep('Reisig',nrow(.)),
         Location = paste(Location,`Trap #`,sep="_")) %>%
  mutate(row = row_number()) %>%
  dplyr::select(Location,Lure...8,`# Moths`,`Date Checked`,Latitude,Longitude,row) %>%
  filter((Lure...8) == "CEW") %>%
  pivot_wider(names_from = (Lure...8),values_from = `# Moths`) %>%
  rename('Date' = `Date Checked`) %>% dplyr::select(!row)

# 2016 data

path <- ("data/raw/Reisig_data/2016_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames

coords <- mylist$`Traps&Coords` %>% dplyr::select(1:5) %>% slice(1:12) %>% 
  drop_na() %>% separate(
    col=Coordinates,into=c("Latitude","Longitude"), sep = ", ") %>% 
  dplyr::rename("Trap" = Trap...2) %>% 
  dplyr::select(2:5) %>%
  mutate(Trap=as.numeric(Trap))

count_2016 <-  mylist$`Traps&Coords` %>% dplyr::select(7:11) %>%
  rename("Trap" = Trap...8) %>%
  left_join(coords,by="Trap") %>% distinct() %>%
  mutate(Date = as.Date(as.integer(Date...7), origin = "1899-12-30"),
         Location = paste(rep('Reisig',nrow(.)),Trap,sep="_")) %>%
  dplyr::select(2:4,7:10) %>%
  filter(LURE == "CEW") %>%
  pivot_wider(names_from = LURE,values_from = `# moths`)


# 2017 data

path <- ("data/raw/Reisig_data/2017_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames

coords <- mylist$Coordinates[2,]

Latitde <- 35.789505
Longitude <- -76.58663

count_2017 <- mylist$Sheet2 %>%
  fill(starts_with("Date"),.direction="down") %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols=everything()) %>%
  mutate(name=as.factor(substring(name,1,1)),
         name= fct_recode(name,Trap = "T",Date = "D", CEW = "#")) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  fill(c(Trap,Date),.direction="down") %>%
  drop_na(CEW) %>% dplyr::select(2:4) %>%
  filter(Trap==2) %>%
  mutate(Latitude = rep(35.789505,nrow(.)),
         Longitude = rep(-76.58663,nrow(.)),
         Location = paste(rep('2017_Reisig',nrow(.)),Trap,sep="_")) %>%
  dplyr::select(!Trap)

# 2018 data

path <- ("data/raw/Reisig_data/2018_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% filter(Lure == "CEW") %>%
  mutate(Coordinates = gsub(".*35","",`Coordinates/Address`)) %>%
  separate(col=Coordinates,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = paste0("35",Latitude),
         Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,5,6)

count_2018 <- mylist$`Pheromone Trap Data` %>% dplyr::select(1:4) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2018_Reisig',nrow(.)),Trap,sep="_")) %>%
  dplyr::select(1,4:7)

# 2019 data

path <- ("data/raw/Reisig_data/2019_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% filter(Lure == "CEW") %>%
  mutate(Coordinates = gsub(".*35","",`Coordinates/Address`)) %>%
  separate(col=Coordinates,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = paste0("35",Latitude),
         Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,5,6)

count_2019 <- mylist$DataSheet %>% dplyr::select(1:4) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2019_Reisig',nrow(.)),Trap,sep="_"),
         Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  dplyr::select(1,4:7)

# 2020 Data

path <- ("data/raw/Reisig_data/2020_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% dplyr::select(1:4) %>% filter(Lure == "CEW") %>%
  separate(col=`Coordinates/Address`,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,3,4)

count_2020 <- mylist$DataSheet %>% dplyr::select(1:4) %>% drop_na(`Date Checked`) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2020_Reisig',nrow(.)),Trap,sep="_"),
         CEW = as.numeric(CEW),
         CEW = replace_na(CEW, 0),
         CEW = round(CEW/2))  %>% # this is because there were two traps set up at this location. I will take the average between the two and round up
  dplyr::select(1,4:7)

# now combining all dataframes together

Reisig_dat <- rbind(count_2011_2012,count_2015,count_2017,(count_2016 %>% dplyr::select(!Trap)),count_2018,count_2019,count_2020) %>% drop_na(Latitude)

Reisig_woy <- Reisig_dat %>% mutate(
  year = year(Date),
  woy = isoweek(Date)) %>%
  group_by(Location, year,woy)  %>%
  summarize(CEW_sum = sum(as.integer(CEW),na.rm=TRUE),longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename(
    "location" = Location
  ) %>% mutate(
    year = as.numeric(as.character(year)),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )


Reisig_woy$trap_type <- rep("PH",nrow(Reisig_woy))
Reisig_woy$contact <- rep("Dominic Reisig",nrow(Reisig_woy))
Reisig_woy$state <- rep("NC",nrow(Reisig_woy))
#Reisig_woy$year <- as.numeric(as.character(Reisig_woy$year))

NC_woy <- rbind(Reisig_woy,NC_woy1)
NC_woy$location <- paste0(NC_woy$location,"_NC")

#####

#Wisconsin Data
#####
# Wisconsin Data
# This data was given to me by Anders Huseth

WI <- read.csv("data/raw/Huseth_data/WI-DATCP_BlacklightData.csv")

WI <- WI %>%
  mutate_at(vars(13:34), as.numeric) %>%
  mutate_at(vars(13:34), replace_na, '0')%>%
  mutate_at(vars(13:34), as.numeric)

# WI week catches run from Wednesday to Tuesday. The week of year calculation 
# will use the mid point between the state and end dates

WI$dateStart <- as.Date(WI$dateStart, format="%m/%d/%Y")
WI$dateEnd <- as.Date(WI$dateEnd, format="%m/%d/%Y")

WI$date <- WI$dateStart + floor((WI$dateEnd-WI$dateStart)/2) #im just taking the mid point date here
WI$year <- year(WI$date)
WI$woy <- isoweek(WI$date)


WI <- WI %>% 
  dplyr::select(1:11,CEW,CEWp,date,woy) %>%
  mutate(trap_type = case_when(
    CEW == 0 & CEWp == 0 ~ "unsure",
    CEW > 0 & CEWp > 0 ~ "BL_PH",
    CEW > 0 & CEWp == 0 ~ "BL",
    CEW == 0 & CEWp > 0 ~ "PH")) %>%
  rowwise() %>% 
  mutate(CEW_total = sum(CEW, CEWp,na.rm = TRUE)) 

WI_woy <- WI %>% group_by(loc,year,woy) %>%
  summarize(CEW_sum = sum(CEW_total),longitude = first(longitude),latitude = first(latitude),
            date = first(date)) %>% drop_na(woy,date) %>%
  rename("location" = loc) 

WI_woy$trap_type <- rep("BL_PH",nrow(WI_woy))
WI_woy$contact <- rep("Anders Huseth",nrow(WI_woy))
WI_woy$state <- rep("WI",nrow(WI_woy))
WI_woy$location <- paste0(WI_woy$location,"_WI")
#####

#Iowa Data
#####
# Iowa Data
#  This data was given to me by:
#   Erin Hodgson and Ashley Dean

IA_2021 <- read_xlsx("data/raw/IA_data/CEW_data_IA_2021.xlsx") %>%
  dplyr::select(!`Trap Type`)

IA <- read_xlsx("data/raw/IA_data/CEW Data_Iowa_2020 (1).xlsx") %>%
  rbind(IA_2021) %>%
  mutate(location = `Location Name`,
         date = as.Date(`Observation Date`),
         CEW = `CEW Counts`,
         State = rep("IA",nrow(.))) %>%
  dplyr::select(3:4,7:10)

IA$year <- year(IA$date)
IA$woy <- isoweek(IA$date)

IA_woy <- IA %>% group_by(location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(Longitude),latitude = first(Latitude),
            date = first(date)) %>% drop_na(woy,date)

IA_woy$trap_type <- rep("PH",nrow(IA_woy))
IA_woy$contact <- rep("Erin Hodgson",nrow(IA_woy))
IA_woy$state <- rep("IA",nrow(IA_woy))
IA_woy$location <- paste0(IA_woy$location,"IA")
#####

#pest watch data
#####
# This is pest watch data which spans multiple states
# This data was given to me by: Shelby Fleischer
# This data is split into XML files and one large csv.


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

PW_woy <- rbind(Y98_06,y2007_2021) %>%
  rename(Location = "ID") %>%
  mutate(year = year(Date),
         woy=isoweek(Date)) %>%
  group_by(Location, year,woy) %>%
  summarize(CEW_sum = sum(CEW),
            longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date),state = first(State)) %>% drop_na(woy,date) %>%
  rename('location'=Location)

PW_woy$trap_type <- rep("PH",nrow(PW_woy))
PW_woy$contact <- rep("Shelby Fleischer",nrow(PW_woy))
PW_woy$location <- paste0(PW_woy$location,"_PW")
unique((PW_woy %>% filter(!complete.cases(state)))$location)





#####

# Tennessee
#####
# Tennessee data
# This data was given to me by Scott Stewert

# Going year by year to combine it all

coords <-  read_excel("data/raw/TN_data/CAPS GPS and Counties - Stewart.xlsx") %>% fill(c(`Site #`,Location)) %>%
  dplyr::select(County, Area, Location) %>% distinct()

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

dat_2005 <- read_excel("data/raw/TN_data/Moth Trap Data05.xls",sheet = 1)[26:47,]

names(dat_2005) <- dat_2005 %>% slice(1) %>% unlist()

dat_2005_2 <- dat_2005[2:20,] %>% dplyr::select(1,12:17)  %>%
  mutate_at(vars(1:6),as.character) %>%
  pivot_longer(cols= 2:7,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30"),
         County = gsub("_","-",County)) %>%
  separate(County,sep="-",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "WTREC" = "S"),
         Area = fct_recode(Area, "North" = "N"),
         Area = fct_recode(Area, "Kenton" = "ktn"),
         Area = fct_recode(Area, "Newbern" = "Nbrn"),
         Area = fct_recode(Area, "West" = "W")) %>%
  left_join(coords,by=c('County')) %>%
  dplyr::select(1:4,6) %>%
  rename(Area = "Area.x")#this is where I have to leave off. Unsure about missing coords



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



# 2014 data

dat_2014 <- read_excel("data/raw/TN_data/2014MothTrappingData.xls",sheet = "Moths")[3:22,1:15]


names(dat_2014) <- dat_2014 %>% slice(1) %>% unlist()

dat_2014_2 <- dat_2014 %>% slice(-1) %>% 
  mutate_if(is.numeric,as.character) %>%
  pivot_longer(cols= 2:15,names_to = "Date",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  separate(`Corn Earworm (Bollworm)`,sep=" ",into=c("County","Area")) %>% 
  mutate(Area = fct_recode(Area, "Maury City" = "(Maury", "Milan - REC" = "(Milan","Coleman Farm" = "(Coleman")) %>%
  mutate(Area = gsub("[()]", "", Area)) %>%
  left_join(coords, by=c("County", "Area"))  # there are plenty of GPS coords missing.



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
                      dat_2013_2,dat_2015_2,dat_2016_2,dat_2017_2,dat_2018_2,dat_2019_2,dat_2020_2,dat_2005_2,dat_2014_2)


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
         CEW = as.numeric(CEW),
         State = rep("Tennessee", nrow(.)),
         CEW = replace_na(CEW,0)) %>%
  dplyr::select(3:7)


TN_woy <- combined_dat2 %>% mutate(
  year = year(Date),
  woy = isoweek(Date)) %>%
  group_by(Location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(Longitude),latitude = first(Latitude),
                                         date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 

TN_woy$trap_type <- rep("PH",nrow(TN_woy))
TN_woy$contact <- rep("Scott Stewart",nrow(TN_woy))
TN_woy$state <- rep("TN",nrow(TN_woy))
TN_woy$location <- paste0(TN_woy$location,"_TN")

#####

# New York Data
#####
# New York Data
# This data was given to me by Abby Seaman

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


coords_combined <- coords_combined %>% dplyr::select(Site, Lat, Long, Date,CEW) %>%
  rename(Location = "Site", Latitude = "Lat", Longitude = 'Long') %>%
  mutate(CEW = replace_na(CEW,0),
         State = rep("New York",nrow(.)))

coords_combined$CEW <- (as.numeric(coords_combined$CEW))
coords_combined$Latitude <- (as.numeric(coords_combined$Latitude))
coords_combined$Longitude <- (as.numeric(coords_combined$Longitude))

NY_woy <- coords_combined %>% mutate(
  year = year(Date),
  woy = isoweek(Date)) %>%
  group_by(Location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 

NY_woy$trap_type <- rep("PH",nrow(NY_woy))
NY_woy$contact <- rep("Abby Seaman",nrow(NY_woy))
NY_woy$state <- rep("NY",nrow(NY_woy))
NY_woy$location <- paste0(NY_woy$location,"_NY")
#####  

# Louisiana Data
#####
# Louisiana Data
# This data was given to me by Tyler Towles

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
  dplyr::select(Location, Date, CEW, State, Latitude,Longitude)



LA_woy <- dat2 %>% mutate(
  woy = isoweek(Date),
  year = year(Date)) %>%
  group_by(Location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 
  
  
LA_woy$trap_type <- rep("PH",nrow(LA_woy))
LA_woy$contact <- rep("Scott Stewart",nrow(LA_woy))
LA_woy$state <- rep("LA",nrow(LA_woy))
LA_woy$location <- paste0(LA_woy$location,"_LA")
#####

# Mississippi
#####
# Mississippi data
# This data was given to me by Fred Musser

list <- lapply(excel_sheets("data/raw/Musser_data/moth trap data 06-20.xlsx"), read_excel, path = "data/raw/Musser_data/moth trap data 06-20.xlsx")

# 2006

y2006 <- list[[1]] %>%
  dplyr::select(1:4,6) %>%
  rename(
    latitude = "Latitude",
    longitude = "Longitude"
  )

# 2007


y2007 <- list[[2]]  %>% mutate(
  latitude = paste0(Degrees...3,"d",Minutes...4)) %>%
  separate(latitude,into=c("left","right"),sep="\\.") %>% mutate(
    right = as.numeric(paste0(".", right))*60,
    latitude = as.numeric(char2dms((paste0(left,"\'",right,"\"N"))))
  ) %>% mutate(
    longitude = paste0(Degrees...5,"d",Minutes...6)) %>%
  separate(longitude,into=c("left","right"),sep="\\.") %>% mutate(
    right = as.numeric(paste0(".", right))*60,
    longitude = as.numeric(char2dms((paste0(left,"\'",right,"\"W"))))
  ) %>% dplyr::select(1,2,ends_with("_CEW"), latitude,longitude) %>%
  mutate_at(vars(3:21), as.numeric) %>%
  mutate_at(vars(3:21), replace_na, '0')%>%
  mutate_at(vars(3:21), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                    names_to = c("Date", ".value"), 
                    names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2008


y2008 <- list[[3]]   %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  dplyr::select(1:4,ends_with("_CEW")) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...3",longitude = "Degrees...4") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2009


y2009 <- list[[4]] %>%
  dplyr::select(1:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1)%>%
  dplyr::select(1,4:7) %>%
  rename(Location = "Trap Location")

#2010

y2010 <- list[[5]] %>%
  dplyr::select(1:4,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...3",longitude = "Degrees...4") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2011

y2011 <- list[[6]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1)  %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2012

y2012 <- list[[7]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1)  %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2013

y2013 <- list[[8]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1)  %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2014
# this year is missing lat and long coords

y2014 <- list[[9]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1)  %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2015

y2015 <- list[[10]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2016

y2016 <- list[[11]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2017

y2017 <- list[[12]] %>%
  dplyr::select(1,2,4:5,ends_with("_CEW")) %>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  mutate_at(vars(ends_with("_CEW")), replace_na, '0')%>%
  mutate_at(vars(ends_with("_CEW")), as.numeric) %>%
  tidyr::pivot_longer(cols = c(ends_with("_CEW")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date))%>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:6) %>%
  rename(Location = "Trap Location")

#2018

y2018 <- list[[13]] %>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% filter(Location != "2") %>%
  filter(Location != "3") %>% filter(Insect == "CEW") %>%
  dplyr::select(!Insect) %>%
  pivot_longer(cols = starts_with("43"),names_to = "Dates",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Dates), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,6:9) 


#2019



y2019 <- list[[14]] %>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% filter(Location != "2") %>%
  filter(Location != "3") %>% filter(Insect == "CEW") %>%
  dplyr::select(!Insect) %>%
  pivot_longer(cols = starts_with("43"),names_to = "Dates",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Dates), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,6:9) 

#2020



y2020 <- list[[15]] %>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% filter(Location != "2") %>%
  filter(Location != "3") %>% filter(Insect == "CEW") %>%
  dplyr::select(!Insect) %>%
  pivot_longer(cols = c(starts_with("44"),starts_with("43")),names_to = "Dates",values_to = "CEW") %>%
  mutate(Date = as.Date(as.integer(Dates), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,6:9) 



MS_combined <- rbind(y2006, y2007,y2008,y2009,y2010,y2011,y2012,y2013,
              y2014,y2015,y2016,y2017,y2018,y2019,y2020)

view(MS_combined %>% filter(!complete.cases(latitude))
)
# now getting the data into the overall framework


MS_woy <- MS_combined %>% mutate(
  woy = isoweek(Date),
  year = year(Date)) %>%
  group_by(Location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(longitude),latitude = first(latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 


MS_woy$trap_type <- rep("PH",nrow(MS_woy))
MS_woy$contact <- rep("Fred Musser",nrow(MS_woy))
MS_woy$state <- rep("MS",nrow(MS_woy))
MS_woy$location <- paste0(MS_woy$location,"_MS")

#####

# South Carolina
#####
# South Carolina data
# This data was given to me by Jeremy Greene
#

coords <- as_tibble(read_excel("data/raw/SC_data/Trap Coordinates - Bollworm SC.xlsx",skip=3)) %>%
  dplyr::select(1,4,7) %>% rename(Location = "Trap Name/Location")

dat <- as_tibble(read_excel("data/raw/SC_data/Detailed Trap Numbers Bollworm in SC 2007 through 2020.xlsx")) %>%
  pivot_longer(cols=c(!Date),names_to = "Location",values_to = "CEW") %>%
  mutate(Location = fct_recode(Location, "D9C" = "BW D15B (D9C)", "D12 A" = "BW D12A", "F12 (middle)" = "BW F12m",
                               "F12 (end)" = "BW F12e", "F22 A" = "BW F22a", "G10" = "BW G10", "G9 (CP 2)" = "BW G9cp2",
                               "Old Pivot" = "OldPivot or BW C8b (D12B for 2007 and 2008)", "C8" = "BW C8", "D6" = "BW D6"
  )) %>% left_join(coords, by="Location")

SC_woy <- dat %>% mutate(
  woy = isoweek(Date),
  year = year(Date)) %>%
  group_by(Location,year,woy) %>%
  summarize(CEW_sum = sum(CEW),longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 

SC_woy$trap_type <- rep("PH",nrow(SC_woy))
SC_woy$contact <- rep("Jeremy Greene",nrow(SC_woy))
SC_woy$state <- rep("SC",nrow(SC_woy))
SC_woy$location <- paste0(SC_woy$location,"_SC")

#####

# Virginia
#####
# Virginia Data
# this data was given to me by Sean Malone

VA_dat <- as_tibble(read_excel("data/raw/VA_data/Virginia black light trap 2003 to 2020 (1).xlsx")) %>%
  mutate_at(vars(4:325),as.character) %>%
  pivot_longer(cols=(4:325),names_to = "date", values_to = "CEW") %>%
  mutate( date = as.Date(as.integer(substr(date, start = 1, stop = 5)), origin = "1899-12-30"))

VA_woy <- VA_dat %>% mutate(
  woy = isoweek(date),
  year = year(date)) %>%
  group_by(Location,year,woy) %>%
  mutate(CEW = as.numeric(CEW)) %>%
  summarize(CEW_sum = sum(CEW,na.rm=TRUE), CEW_std = std(CEW),longitude = first(longitude),latitude = first(latitude),
            date = first(date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) 


VA_woy$trap_type <- rep("BL",nrow(VA_woy))
VA_woy$contact <- rep("Sean Malone",nrow(VA_woy))
VA_woy$state <- rep("VA",nrow(VA_woy))

# The following data was supplied by Helene Doughty

Helene_dat <- as_tibble(read_excel("data/raw/VA_data/Helene_Doughty/Corn Earworm Black Light Trap Catch 1977 to 2021 - ESAREC, Painter VA (1).xlsx"))[1:37,] %>%
  dplyr::select(!...2) %>%
  mutate_at(vars(starts_with("42"),starts_with("43")),as.character) %>%
  pivot_longer(cols=c(starts_with("42"),starts_with("43")),names_to = "date",values_to = "CEW") %>%
  mutate(
    date = as.Date(as.numeric(date), origin = "1899-12-30"),
    date = as.Date((paste0(...1,sep="-",month(date),sep="-",day(date)))),
    CEW = as.numeric(CEW),
    latitude = as.numeric(37.587778), # supplied by Helene (37 35’ 16” N) converted in google earth engine
    longitude = as.numeric(-75.823056), # supplied by Helene (75 49’ 23” W) converted in google earth engine
  )

Helene_dat$location = as.factor(rep("AREC",nrow(Helene_dat)))

Helene_dat_woy <- Helene_dat %>%
  drop_na(CEW) %>% dplyr::select(!...1)  %>% mutate(
    woy = isoweek(date),
    year = year(date)) %>%
  group_by(location,year,woy) %>%
  mutate(CEW = as.numeric(CEW)) %>%
  summarize(CEW_sum = sum(CEW,na.rm=TRUE), CEW_std = std(CEW),longitude = first(longitude),latitude = first(latitude),
            date = first(date)) %>% drop_na(woy,date) 
  
Helene_dat_woy$trap_type <- rep("BL",nrow(Helene_dat_woy))
Helene_dat_woy$contact <- rep("Helene Doughty",nrow(Helene_dat_woy))
Helene_dat_woy$state <- rep("VA",nrow(Helene_dat_woy))

  
VA_woy <- rbind(VA_woy,Helene_dat_woy)
VA_woy$location <- paste0(VA_woy$location,"_VA")
#####

# Utah
#####
# Utah data
# This data was given to me by Lori Spears

#2015

coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2015.csv")) %>%
  dplyr::select(1,4:5)

utah_2015_1 <- as_tibble(read.csv("data/raw/Utah_data/Results_Utah_AlfalfaCorn_1.csv",skip=5)[,1:15]) %>%
  dplyr::select(3,8,14,15) %>%
  mutate(date = parse_date_time(DATE.OF.COLLECTION,order="mdy"),
         location = TRAP.ID) %>%
  rename(
    Zea_dissected = "H.zea..Dissected.",
    Zea_not_dissected = "Helicoverpa.sp...Not.Dissected."
  ) %>%
  dplyr::select(3:6)


utah_2015_2 <- as_tibble(read_xlsx("data/raw/Utah_data/Results_Utah_AlfalfaCorn_2.xlsx",skip=5)[,1:15]) %>%
  dplyr::select(3,8,14,15) %>%
  rename(
    Zea_dissected = "H. zea (Dissected)",
    Zea_not_dissected = "Helicoverpa sp. (Not Dissected)",
    date = "DATE OF COLLECTION"
  ) %>%
  mutate(location = `TRAP ID`) %>%
  dplyr::select(1,3:5) 

y2015_UT <- rbind(utah_2015_1,utah_2015_2) %>% drop_na() %>%
  rowwise() %>%
  mutate(CEW = sum(as.numeric(Zea_dissected), as.numeric(Zea_not_dissected),na.rm=TRUE)) %>%
  dplyr::select(3:5) %>%
  mutate(location = gsub("ALF","ALF15_",location)) %>%
  left_join(coords, by = c("location" = "Site_Name"))


#2016

coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2016.csv")) %>%
  dplyr::select(1,4:5) 

y2016_UT <- as_tibble(read_xlsx("data/raw/Utah_data/Results_Utah_AlfalfaCorn_all_2016.xlsx",skip=5)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select(1:11,18) %>%  # if you want more identifiers
  dplyr::select(3,8,12) %>%
  separate(`DATE RANGE`,sep = "-",into=c("Start","End")) %>%
  drop_na(Start) %>%
  filter(Start != "DATE RANGE") %>%
  mutate(CEW = as.numeric(`Helicoverpa sp.`)) %>%
  mutate_at(vars(5), replace_na, '0') %>%
  mutate( date = parse_date_time(End,order="mdy")) %>%
  dplyr::select(3,5,6) %>%
  rename(location = "TRAP ID") %>%
  mutate(location = gsub("HA","ALF16_",location)) %>%
  left_join(coords, by = c("location" = "Site_Name"))


#2017
coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2017.csv")) %>%
  dplyr::select(1,4:5) %>%
  mutate(Site = gsub("_0(?<=\\d)","_",Site,perl = TRUE))

y2017_UT_1 <- as_tibble(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=3)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select("DATE OF TRAP COLLECTION","TRAP ID","Helicoverpa zea (dissected)") %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea (dissected)`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5) %>%
  rename(location = "TRAP ID")

y2017_UT_2 <- as_tibble(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_2.xlsx",sheet=4)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select("DATE OF TRAP COLLECTION","TRAP ID","Helicoverpa zea (dissected)") %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea (dissected)`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5) %>%
  rename(location = "TRAP ID")


y2017_UT <- rbind(y2017_UT_1,y2017_UT_2)  %>%
  mutate(location = gsub("HA-","ALF17_",location)) %>%
  left_join(coords, by = c("location" = "Site"))

# 2018

coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2018.csv")) %>%
  dplyr::select(1,4:5)

#Alfalfa

y2018_UT_1 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Alfalfa_RESULTS_2018.xlsx",skip=5,sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select(1:9,13) %>%  # if you want more identifiers
  dplyr::select(3,9,10) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`# moths`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5)

y2018_UT_2 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Alfalfa_RESULTS_2018.2.xlsx",sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select(1:9,13) %>%  # if you want more identifiers
  dplyr::select(3,9,10) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`# moths`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5)

y2018_UT_3 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Alfalfa_RESULTS_2018.3.xlsx",sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  dplyr::select(1:9,12) %>%  # if you want more identifiers
  dplyr::select(`DATE OF TRAP COLLECTION`,`TRAP ID`,`Helicoverpa zea`) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5)

#Corn
y2018_UT_4 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Corn_RESULTS_2018.2.xlsx",sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  #dplyr::select(1:9,12) %>%  # if you want more identifiers
  dplyr::select(`DATE OF TRAP COLLECTION`,`TRAP ID`,`Helicoverpa zea`) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5)


y2018_UT_5 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Corn_RESULTS_2018.3.xlsx",skip=2,sheet=1)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  #dplyr::select(1:9,12) %>%  # if you want more identifiers
  dplyr::select(`DATE OF TRAP COLLECTION`,`TRAP ID`,`Helicoverpa zea`) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION` - years(4)) %>% #error in csv date is not 2022 but rather 2018
  dplyr::select(2,4,5)

#Grain 

y2018_UT_6 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Grain_RESULTS_2018.2.xlsx",sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  #dplyr::select(1:9,12) %>%  # if you want more identifiers
  dplyr::select(`DATE OF TRAP COLLECTION`,`TRAP ID`,`H. zea`) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`H. zea`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION`) %>%
  dplyr::select(2,4,5)

y2018_UT_7 <- as_tibble(read_xlsx("data/raw/Utah_data/Utah_Grain_RESULTS_2018.3.xlsx",skip=5,sheet=2)) %>%
  filter(`LURE (=target)` == "Helicoverpa armigera") %>%
  #dplyr::select(1:9,12) %>%  # if you want more identifiers
  dplyr::select(`DATE OF TRAP COLLECTION`,`TRAP ID`,`Helicoverpa zea`) %>%
  drop_na(`DATE OF TRAP COLLECTION`) %>%
  mutate(CEW = as.numeric(`Helicoverpa zea`)) %>%
  mutate_at(vars(CEW), replace_na, '0') %>%
  mutate( date = `DATE OF TRAP COLLECTION` - years(4)) %>%
  dplyr::select(2,4,5)

UT_combined_2018 <- rbind(y2018_UT_7,y2018_UT_6,y2018_UT_5,y2018_UT_4,y2018_UT_3,y2018_UT_2,y2018_UT_1)  %>%
  mutate(location = gsub(" HA","",`TRAP ID`)) %>%
  mutate(location = paste0("ALF18_",location)) %>%
  left_join(coords, by = c("location" = "Site_Name"))  %>%
  dplyr::select(!`TRAP ID`)


  

UT_combined <- rbind(UT_combined_2018, y2017_UT, y2015_UT,y2016_UT)



UT_woy <- UT_combined %>% mutate(
  woy = isoweek(date),
  year = year(date)) %>%
  group_by(location,year,woy) %>%
  mutate(CEW = as.numeric(CEW)) %>%
  summarize(CEW_sum = sum(CEW,na.rm=TRUE),longitude = first(Lat),latitude = first(Long),
            date = first(date)) %>% drop_na(woy,date) 

UT_woy$trap_type <- rep("PH",nrow(UT_woy))
UT_woy$contact <- rep("Lori Spears",nrow(UT_woy))
UT_woy$state <- rep("UT",nrow(UT_woy))  
UT_woy$location <- paste0(UT_woy$location,"_UT")
#####


# Arizona
#####
# Arizona data
# This data was given to me by John Palumbo

coords <- as_tibble(read_excel("data/raw/AZ_data/GPS Coordinates_H zea_Traps_Yuma AZ _1998-2021.xls")) %>% drop_na(...2) %>%
  rename(location = "1998-2002", latitude = "...2",longitude = "...3") %>%
  mutate(location = fct_recode(location,
                               "Texas Hill" = "1",
                               "Tacna" = "2",
                               "Roll"= "3",
                               "Wellton" = "4",
                               "Dome Valley-East"  = "5",
                               "Dome Valley-West"  = "6",
                               "Gila Valley-East"  = "7",
                               "Gila Valley-West"  = "8",
                               "Gila Valley South"  = "9",
                               "Yuma Valley -Dinnsmore"  = "10",
                               "Yuma Valley -Barkley" = "11",
                               "Yuma Valley - Darrigo" = "12",
                               "Yuma Valley- Ranch 88"  = "13",
                               "Yuma Valley"  = "14",
                               "Yuma Valley  YAC"  = "15",
                               "Bard"  = "16"))



# 1998 -2002
yuma_valley_1 <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,1:3]

yuma_valley_1 <- yuma_valley_1 %>%
  dplyr::select(1,3) %>%
  mutate(location = rep("Yuma Valley 1",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Yuma Valley -1`))),
         CEW = ...3) %>%
  dplyr::select(3:5)

yuma_valley_2 <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,5:7]

yuma_valley_2 <- yuma_valley_2 %>%
  dplyr::select(1,3) %>%
  mutate(location = rep("Yuma Valley 2",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Yuma Valley -2`))),
         CEW = ...7) %>%
  dplyr::select(3:5)

gila_valley <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,9:11]

gila_valley_2 <- gila_valley %>%
  dplyr::select(1,3) %>%
  mutate(location = rep("Gila Valley",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Gila Valley`))),
         CEW = ...11) %>%
  dplyr::select(3:5)


dome_valley <- as_tibble(read_excel("data/raw/AZ_data/H zea_H virescens_Traps_Yuma_1998-2002 (2).xls"))[6:209,13:15]

dome_valley_2 <- dome_valley %>%
  dplyr::select(1,3) %>%
  mutate(location = rep("Dome Valley",nrow(.)),
         Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", `Dome Valley`))),
         CEW = ...15) %>%
  dplyr::select(3:5) %>% drop_na(Date)




# 2013 - 2021

yuma_valley_3 <- as_tibble(read_excel("data/raw/AZ_data/AILRC_Trap Counts_H. zea_Yuma AZ 2013-2021 (1).xlsx",skip=2))[,1:19]

yuma_valley_3 <- yuma_valley_3 %>% fill(`2013-14`,.direction=c("down"))

yuma_valley_3$`2013-14`[is.na(yuma_valley_3$`2013-14`)] <- "2013-14"

yuma_valley_3 <- yuma_valley_3 %>% rename("Year" = "2013-14") %>% drop_na(...3)

yuma_valley_3_split <- yuma_valley_3 %>%  filter(Year != "2013-14")%>%
  mutate( Date = as.Date(as.integer(Date), origin = "1899-12-30"))

az_2013 <- yuma_valley_3 %>%  filter(Year == "2013-14") %>%
  mutate(Date =mdy(sub("00$", "20", sub("^(.)(..)(..)$", "0\\1\\2\\3", Date))))
  
yuma_valley_4  <- rbind(yuma_valley_3_split,az_2013) %>% dplyr::select(!...3) %>%
  mutate_at(vars(3:18),as.character) %>%
  pivot_longer(cols=!c(Year,Date),names_to = "location",values_to="CEW") %>%
  dplyr::select(!Year) 


yuma_valley_5 <- rbind(yuma_valley_4,dome_valley_2,gila_valley_2,yuma_valley_2,yuma_valley_1) %>%
  left_join(coords,by="location")



AZ_woy <- yuma_valley_5 %>% mutate(
  woy = isoweek(Date),
  year = year(Date)) %>%
  group_by(location,year,woy) %>%
  mutate(CEW = as.numeric(CEW)) %>%
  drop_na(CEW) %>%
  summarize(CEW_sum = round(sum(CEW,na.rm=TRUE)),longitude = first(longitude),latitude = first(latitude),
            date = first(Date)) %>% drop_na(woy,date)


AZ_woy$trap_type <- rep("PH",nrow(AZ_woy))
AZ_woy$contact <- rep("John Palumbo",nrow(AZ_woy))
AZ_woy$state <- rep("AZ",nrow(AZ_woy))
AZ_woy$location <- paste0(AZ_woy$location,"_AZ")

#####


# Florida
#####
# Florida data
# This data was given to me by Izailda Barbosa dos Santos

FL <- as_tibble(read_excel("data/raw/FL_data/Year-round h zea catch in florida panhandle_1.xlsx"))

FL$Date_diff <- as.numeric(((FL$END_DATE - FL$START_DATE)/86400)/7)

FL$TRAP_ID <- paste(FL$`TRAP ID`,FL$REP,sep="_")
unique(FL$TRAP_ID)


error1 <- FL %>% filter(Date_diff < 0) %>%
  mutate(END_DATE = END_DATE + 3.154e+7,
         Date_diff= as.numeric(((END_DATE - START_DATE))))

error2 <- FL %>% filter(Date_diff > 100) %>%
  mutate(END_DATE = END_DATE - (3.154e+7),
         Date_diff= as.numeric(((END_DATE - START_DATE))))

FL2 <- FL %>% filter(between(Date_diff,0,100)) %>%
  rbind(error1) %>%
  rbind(error2) %>%
  mutate(`PEST COUNT` = as.numeric(`PEST COUNT`),
         Date_diff = as.numeric(Date_diff)/7) %>%
  drop_na(`PEST COUNT`)

adjustment <- FL2 %>% filter(Date_diff > 1) %>%
  mutate(`PEST COUNT` = `PEST COUNT`/Date_diff)

FL3 <- FL2 %>% filter(Date_diff <= 1) %>%
  rbind(adjustment) %>%
  mutate(midpoint = as.POSIXct((as.numeric(END_DATE) + as.numeric(START_DATE)) / 2, origin = '1970-01-01'),
         woy = lubridate::isoweek(midpoint))





FL_woy <- FL3 %>% mutate(
  woy = isoweek(midpoint),
  year = year(midpoint)) %>%
  group_by(TRAP_ID,year,woy) %>%
  mutate(CEW = as.numeric(`PEST COUNT`)) %>%
  drop_na(CEW) %>%
  summarize(CEW_sum = round(sum(CEW,na.rm=TRUE)),longitude = first(LONGITUDE),latitude = first(LATITUDE),
            date = first(midpoint)) %>% drop_na(woy,date)


FL_woy$trap_type <- rep("PH",nrow(FL_woy))
FL_woy$contact <- rep("Izailda Barbosa dos Santos",nrow(FL_woy))
FL_woy$state <- rep("FL",nrow(FL_woy))
FL_woy$location <- paste0(FL_woy$TRAP_ID,"_FL")

FL_woy <- FL_woy %>% ungroup() %>% dplyr::select(!TRAP_ID)

#####

# Great lakes
#####
# Ontario/Great lakes data
# This data was given to me by Tracey Baute

GL <- as_tibble(read_excel("data/raw/Great_lakes/CEW Traps 2019 20 21 GLMPMN (1).xlsx")) %>%
  dplyr::select(1,2,6:8,10:11) %>%
  group_by(`What is the Trap Name?`) %>%
  arrange(`Date trap was checked?`) %>%
  mutate(date = as.Date(`Date trap was checked?`),
         state = `State or Province`,
         location = `What is the Trap Name?`,
         CEW = `Moth Counts`,
         ndays =as.numeric((date - lag(date)))) %>%
  dplyr::select(5:10,"ndays","CEW","location") %>%
  filter(ndays < 100) %>%
  drop_na(ndays) %>% 
  mutate(
    CEW_nightly = CEW/ndays,
    woy = isoweek(date),
    year = year(date)) %>% 
  ungroup() %>%
  filter_at(vars(CEW_nightly), all_vars(!is.infinite(.))) %>%
  dplyr::select(3:6,7,9,"woy",'year','state',"location","CEW_nightly") 
  


GL_woy <- GL %>%
  group_by(location,year,woy) %>%
  summarize(CEW_sum = round(sum(CEW_nightly,na.rm=TRUE)),longitude = first(Longitude),latitude = first(Latitude),
            date = first(date),location = first(location),state=first(state)) %>% drop_na(woy,date)

GL_woy$trap_type <- rep("PH",nrow(GL_woy))
GL_woy$contact <- rep("Tracey Baute",nrow(GL_woy))
GL_woy$state <- rep("FL",nrow(GL_woy))
GL_woy$location <- paste0(GL_woy$location,"_GL")

#####


# Oregon Data
#####
# Ontario/Great lakes data
# This data was given to me by Tracey Baute

OR <- as_tibble(fread("data/raw/Oregon_data/OR_HzeaCounts_8Oct21.csv")) %>%
  dplyr::select(1,3:6,8,10:11) %>%
  mutate(date = parse_date_time(check_date,order="mdy"),
         location = location_id,
         CEW = Hzea_count) %>%
  dplyr::select(1,2,4:5,7,9:11) %>%
  mutate(
    CEW_nightly = CEW/days_between_check,
    woy = isoweek(date),
    year = year(date)) %>% 
  dplyr::select(1:4,6:7,9:10) 


OR_woy <- OR %>%
  group_by(location,year,woy) %>%
  summarize(CEW_sum = round(sum(CEW_nightly,na.rm=TRUE)),longitude = first(longitude),latitude = first(latitude),
            date = first(date),location = first(location),state=first(state)) %>% drop_na(woy,date) %>%
  mutate(latitude = as.numeric(latitude))

OR_woy$trap_type <- rep("PH",nrow(OR_woy))
OR_woy$contact <- rep("Seth Dorman",nrow(OR_woy))
OR_woy$location <- paste0(OR_woy$location,"_OR")




#####

# Minnesota Data
#####
# Minnesota Data
# This data was provided by William Hutchison

Rosemount <- 
  as_tibble(read_excel("data/raw/MN_data/Hutchison UMN-MN CEW pheromone trap data summary-multi year and location 10-6-21-rev.xlsx",skip=5,sheet=1)) %>%
  filter(`2007` != "Missing") %>%
  mutate(month = month(`Month/Day`),day = day(`Month/Day`),`2007` = as.numeric((`2007`))) %>% 
  dplyr::select(!`Month/Day`) %>%
  pivot_longer(cols=starts_with("20")) %>%
  mutate(date = as.Date(paste(name,month,day,sep="-")), location = "Rosemount",
         latitude = 44.705605, longitude = -93.100794) %>% 
  dplyr::select(location,value,date,latitude,longitude)

Blue_Earth <- 
  as_tibble(read_excel("data/raw/MN_data/Hutchison UMN-MN CEW pheromone trap data summary-multi year and location 10-6-21-rev.xlsx",skip=5,sheet=2)) %>%
  filter(`2007` != "Missing") %>%
  mutate(month = month(`Month/Day`),day = day(`Month/Day`),`2007` = as.numeric((`2007`))) %>% 
  dplyr::select(!`Month/Day`) %>%
  pivot_longer(cols=starts_with("20")) %>%
  mutate(date = as.Date(paste(name,month,day,sep="-")), location = "Blue_Earth",
         latitude = 43.68, longitude = -93.95) %>%
  dplyr::select(location,value,date,latitude,longitude)

Le_Sueur <- 
  as_tibble(read_excel("data/raw/MN_data/Hutchison UMN-MN CEW pheromone trap data summary-multi year and location 10-6-21-rev.xlsx",skip=5,sheet=3)) %>%
  filter(`2007` != "Missing") %>%
  mutate(month = month(`Month/Day`),day = day(`Month/Day`),`2007` = as.numeric((`2007`))) %>% 
  dplyr::select(!`Month/Day`) %>%
  pivot_longer(cols=starts_with("20")) %>%
  mutate(date = as.Date(paste(name,month,day,sep="-")), location = "Le_Sueur",
         latitude = 44.471764, longitude = -93.90253) %>%
  dplyr::select(location,value,date,latitude,longitude)

Lamberton <- 
  as_tibble(read_excel("data/raw/MN_data/Hutchison UMN-MN CEW pheromone trap data summary-multi year and location 10-6-21-rev.xlsx",skip=5,sheet=4)) %>%
  filter(`2007` != "Missing") %>%
  mutate(month = month(`Month/Day`),day = day(`Month/Day`),`2007` = as.numeric((`2007`))) %>% 
  dplyr::select(!`Month/Day`) %>%
  pivot_longer(cols=starts_with("20")) %>%
  mutate(date = as.Date(paste(name,month,day,sep="-")), location = "Lamberton",
         latitude = 44.241347, longitude = -95.315091) %>%
  dplyr::select(location,value,date,latitude,longitude)

Owatonna <- 
  as_tibble(read_excel("data/raw/MN_data/Hutchison UMN-MN CEW pheromone trap data summary-multi year and location 10-6-21-rev.xlsx",skip=5,sheet=5)) %>%
  mutate(month = month(`Month/Day`),day = day(`Month/Day`),`2007` = as.numeric((`2007`))) %>% 
  dplyr::select(!`Month/Day`) %>%
  pivot_longer(cols=starts_with("20")) %>%
  mutate(date = as.Date(paste(name,month,day,sep="-")), location = "Owatonna",
         latitude = 44.02, longitude = -93.22) %>%
  dplyr::select(location,value,date,latitude,longitude)

MN_data <- rbind(Rosemount,Blue_Earth,Le_Sueur,Lamberton,Owatonna)

MN_woy <- MN_data %>%
  mutate(woy = isoweek(date), year = year(date)) %>%
  group_by(location,year,woy) %>%
  summarize(CEW_sum = round(sum(value,na.rm=TRUE)),longitude = first(longitude),latitude = first(latitude),
            date = first(date),location = first(location),state="MN") %>% drop_na(woy,date) %>%
  mutate(
    trap_type = "PH",
    contact = "William Hutchison",
    location = paste0(location,"_MN")
  )

#####




# now combining all the WOY data together
# adding ecoregion levels
# formatting for google earth engine importing
#####
# Combine the WOY data together
# there are a bunch of NA coords from the pest watch database. For now I am just removing them
# fixing a few weird coordinates
?distinct
National_WOY <- rbind(IA_woy,LA_woy,MS_woy,NC_woy,NY_woy,PW_woy,SC_woy,TN_woy,WI_woy,VA_woy,AZ_woy,UT_woy,FL_woy,GL_woy,OR_woy, MN_woy) %>%
  mutate(longitude = (abs(longitude)) * -1) %>% drop_na(latitude) %>% distinct(latitude,longitude,date,woy,CEW_sum,.keep_all = TRUE) %>% 
  drop_na(longitude)


error_dat <- National_WOY %>% filter(latitude == 71.15) %>%
  mutate(longitude = longitude * -1,
         latitude = latitude * -1) %>%
  rename( longitude = latitude,
          latitude = longitude)

National_WOY <- National_WOY %>% rbind(error_dat)

error_dat2 <- National_WOY %>% filter(longitude < -133) %>%
  mutate(longitude = longitude + 20)

National_WOY <- National_WOY  %>% rbind(error_dat2)

error_dat3 <- National_WOY %>% filter(latitude == 4.0933)  %>%
  mutate(latitude = latitude * 10)

National_WOY <- National_WOY  %>% rbind(error_dat3)

states <- c("MA","MD","NY","VA")
 
error_dat4 <- National_WOY  %>%
  filter(contact == "Shelby Fleischer") %>%
  filter(state == "MA") %>%
  filter(longitude == -41.51) %>%
  mutate(longitude = longitude * -1) %>%
  mutate(latitude = latitude * -1)

National_WOY <- National_WOY  %>% rbind(error_dat4)

data_checking <- National_WOY %>%
  group_by(state) %>%
  summarize(min = min(year),mean = mean(year),max=max(year))

# now removing points that dont fall within North America

N_america <- ne_countries(continent = "north america")

#plot(dat_projected3,add=TRUE)
dat_projected <- SpatialPointsDataFrame(coords=National_WOY[5:6], data=National_WOY,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


st_crs(N_america) == st_crs(dat_projected) 

dat_projected3 <- raster::crop(dat_projected,N_america)



# Adding overwintering zones

zone <- raster("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif",layer=42)


final_dat <- as_tibble(dat_projected3@data) %>%
  dplyr::select(1:10, ends_with("NAME")) %>%
  dplyr::select(1:10,starts_with("NA_")) %>%
  mutate(zone_30y = raster::extract(zone, dat_projected3, method='simple'))

ow_zones <- raster("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif")


band <- seq(1,42,by=1)
year <- seq(1981,2022,by=1)

year_table <- as.data.frame(cbind(band,year))


dat_projected3_sf <- st_as_sf(dat_projected3)
y_list <- list()

for (i in band){
  yeari <- (year_table %>% filter(band == paste(i)))$year
  ydat <- dat_projected3_sf %>% filter(year == yeari)
  ow_zones <- raster("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif",band=paste(i))
  y_list[[i]] <- raster::extract(ow_zones,ydat,method='simple',sp=TRUE)
}

m <- do.call(rbind, y_list) 

m2 <- st_as_sf(m)

unique(factor(m2$Zea_over_winter_zones))


m3 <- m2 %>% drop_na(Zea_over_winter_zones) %>% mutate(zone_30y = raster::extract(zone, ., method='simple')
)
  

st_geometry(m3) <- NULL


# getting it into GEE format

final_dat <- m3 %>% mutate(
  unix_date = as.numeric(anytime(date))*1000,
  winter_end_date = as.Date(paste0(year,"-02-28")),
  winter_start_date = as.numeric(anytime(winter_end_date - 90))*1000, #unix timestamp. Remember R uses unixtime in SECONDS not miliseconds.
  winter_end_date = as.numeric(anytime(as.Date(paste0(year,"-02-28"))))*1000, # so you must multiple by 1000
  Latitude = latitude,
  Longitude = longitude,
  multiyear_zones = Zea_over_winter_zones
) %>% dplyr::select(!Zea_over_winter_zones) %>% as_tibble() %>% group_by(zone_30y,year,woy) %>%
  mutate(trap_num = length(unique(location)))




write.csv(final_dat,file="data/processed/Threshold_5c/final_zea_data/Hzea_ow_zones_Dec172021.csv")

#####



