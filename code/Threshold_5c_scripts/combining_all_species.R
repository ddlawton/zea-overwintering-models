###
# Creating a dataframe with
#  all trap data included
####

rm(list=ls())

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
  mutate(CEW = replace_na(as.numeric(CEW),0)) %>%
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
#mutate(year = coalesce(year,year1)) %>% dplyr::select(!c(year1))

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


count <-  mylist$Sheet1 %>% dplyr::select(6:9)  

count_2015 <- count %>% left_join(coords,by="Trap #") %>% distinct() %>% dplyr::select(!c(`Date set`)) %>% drop_na(Latitude) %>%
  mutate(Location = rep('Reisig',nrow(.)),
         Location = paste(Location,`Trap #`,sep="_")) %>%
  mutate(row = row_number()) %>%
  dplyr::select(Location,Lure...8,`# Moths`,`Date Checked`,Latitude,Longitude,row) %>%
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
  #filter(LURE == "CEW") %>%
  pivot_wider(names_from = LURE,values_from = `# moths`)


# 2017 data

path <- ("data/raw/Reisig_data/2017_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames

coords <- mylist$Coordinates %>%
  mutate(Coordinates = gsub(".*35","",`Coordinates/Address`)) %>%
  separate(col=Coordinates,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = paste0("35",Latitude),
         Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,2,7,8)


count_2017 <- mylist$Sheet2[1:4,1:57] %>%
  fill(starts_with("Date"),.direction="down") %>%
  dplyr::mutate(across(starts_with("Date"),as.character)) %>%
  stack() %>%
  mutate(ind = gsub("\\..*","",ind),
         id = rep(c(1:19),each=12)) %>%
  pivot_wider(names_from = ind,values_from = values)  %>%
  unnest() %>%
  rename(Trap = "Trap #") %>%
  mutate(Trap = as.integer(Trap)) %>%
  left_join(coords,by="Trap") %>%
  pivot_wider(names_from = Lure,values_from = `# moth`) %>%
  mutate(Location = paste(rep('2017_Reisig',nrow(.)),Trap,sep="_")) %>%
  dplyr::select(!Trap)

  
  
  
  
# 2018 data

path <- ("data/raw/Reisig_data/2018_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates  %>%
  mutate(Coordinates = gsub(".*35","",`Coordinates/Address`)) %>%
  separate(col=Coordinates,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = paste0("35",Latitude),
         Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(Lure,1,5,6)

count_2018 <- mylist$`Pheromone Trap Data` %>% dplyr::select(1:4) %>%
  rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  pivot_wider(names_from = Lure.x,values_from = `# Moths`) %>%
  mutate(Location = paste(rep('2018_Reisig',nrow(.)),Trap,sep="_")) %>%
  dplyr::select(1,4:10)

# 2019 data

path <- ("data/raw/Reisig_data/2019_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% #filter(Lure == "CEW") %>%
  mutate(Coordinates = gsub(".*35","",`Coordinates/Address`)) %>%
  separate(col=Coordinates,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = paste0("35",Latitude),
         Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,5,6)

count_2019 <- mylist$DataSheet %>% dplyr::select(1:4) %>%
  rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`) %>%
  mutate(Location = paste(rep('2019_Reisig',nrow(.)),Trap,sep="_")) %>%
  drop_na(`# Moths`) %>%
  pivot_wider(names_from = Lure,values_from = `# Moths`)
  dplyr::select(1,3:9)

# 2020 Data

path <- ("data/raw/Reisig_data/2020_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% dplyr::select(1:4)  %>%
  separate(col=`Coordinates/Address`,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  dplyr::select(1,3,4)

count_2020 <- mylist$DataSheet %>% dplyr::select(1:4) %>% drop_na(`Date Checked`) %>%
  rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,Moths = `# Moths`) %>% 
  mutate(Location = paste(rep('2020_Reisig',nrow(.)),Trap,sep="_"),
         Moths = as.numeric(Moths),
         Moths = replace_na(Moths, 0),
         Moths = round(Moths/2))  %>% # this is because there were two traps set up at this location. I will take the average between the two and round up
  pivot_wider(names_from = Lure,values_from = Moths,values_fn = {mean}) %>%
  dplyr::select(1,3:9)

# now combining all dataframes together

column_names <- c("CEW","ECB","ECB*","FAW","SBL")

Reisig_dat <- bind_rows(
  (count_2011_2012 %>% mutate(across(everything(),as.character))),
  (count_2015 %>% mutate(across(everything(),as.character))),
  (count_2017 %>% mutate(across(everything(),as.character))),
  (count_2016 %>% mutate(across(everything(),as.character))),
  count_2018 %>% mutate(across(everything(),as.character)),
  count_2019 %>% mutate(across(everything(),as.character)),
  count_2020 %>% mutate(across(everything(),as.character))) %>%
  select(1:9,12:14) %>%
  mutate(Date = as.Date(Date))



Reisig_woy <- Reisig_dat %>% mutate(
  year = year(Date),
  woy = isoweek(Date)) %>%
  mutate(ECB = as.character(sum(as.integer(ECB),as.integer(`ECB*`))),
         OWBW = `OWBW*`) %>%
  select(!c(`ECB*`,`OWBW*`)) %>%
  pivot_longer(cols =c(3,6:10,13),names_to = 'species',values_to = 'count') %>%
  group_by(Location, year,woy,species)  %>%
  summarize(count_sum = sum(as.integer(count)),
            longitude = first(Longitude),
            latitude = first(Latitude),
            date = first(Date)) %>% 
  drop_na(woy,date) %>%
  pivot_wider(names_from = "species",values_from = "count_sum") %>%
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

NC_woy <- rbind(Reisig_woy,
                NC_woy1 %>% rename(CEW = "CEW_sum"))
NC_woy$location <- paste0(NC_woy$location,"_NC")

#####



#Wisconsin Data
#####
# Wisconsin Data
# This data was given to me by Anders Huseth

WI <- read.csv("data/raw/Huseth_data/WI-DATCP_BlacklightData.csv") %>% tibble()

WI <- WI %>%
  mutate(across(13:34, as.numeric)) %>%
  mutate(across(13:34, ~replace_na(.,0)))

# WI week catches run from Wednesday to Tuesday. The week of year calculation 
# will use the mid point between the state and end dates

WI$dateStart <- as.Date(WI$dateStart, format="%m/%d/%Y")
WI$dateEnd <- as.Date(WI$dateEnd, format="%m/%d/%Y")

WI$date <- WI$dateStart + floor((WI$dateEnd-WI$dateStart)/2) #im just taking the mid point date here
WI$year <- year(WI$date)
WI$woy <- isoweek(WI$date)


WI <- WI %>% 
  mutate(trap_type = case_when(
    CEW == 0 & CEWp == 0 ~ "unsure",
    CEW > 0 & CEWp > 0 ~ "BL_PH",
    CEW > 0 & CEWp == 0 ~ "BL",
    CEW == 0 & CEWp > 0 ~ "PH")) %>%
  rowwise() %>%
  mutate(CEW2 = sum(CEW, CEWp,na.rm = TRUE)) %>%
  select(!CEWp) %>%
  rename(CEW2 = 'CEW')
names(WI)

WI_woy <- WI %>%
  select(!CEW) %>%
  rename(CEW = 'CEW2') %>%
  pivot_longer(cols=c(13:32,CEW),names_to = "species",values_to = "count") %>%
  group_by(loc,year,woy,species) %>%
  summarize(sum = sum(count),longitude = first(longitude),
            latitude = first(latitude),
            date = first(date)) %>% 
  drop_na(woy,date) %>%
  rename("location" = loc) %>%
  pivot_wider(names_from = "species",values_from = sum)
summary(WI_woy)

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
#2004

view(read_excel("data/raw/TN_data/Moth Trapping04.xlsx",sheet=2),col_names=FALSE) %>%
  print(n=Inf)

formats <- xlsx_formats("data/raw/TN_data/Moth Trapping04.xlsx")

tn2004 <-
  xlsx_cells("data/raw/TN_data/Moth Trapping04.xlsx", sheets = 2) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% #select specific cell descriptions
  mutate(underline = formats$local$font$underline[local_format_id]) %>% #find which cells are underlined
  filter(data_type != "blank") %>% #remove all blank cells
  behead_if(underline == "single", #Species name is at the left-top and is underlined
            direction = "left-up",
            name="Species") %>%
  behead('left','location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  select(Species,location,date.header,numeric) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions



#2005

read_excel("data/raw/TN_data/Moth Trap Data05.xlsx",sheet=1,col_names=FALSE)

formats <- xlsx_formats("data/raw/TN_data/Moth Trap Data05.xlsx")

tn2005 <-
  xlsx_cells("data/raw/TN_data/Moth Trap Data05.xlsx", sheets = 1) %>%
  filter(col <= 17) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2006")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>% #select the important cell descriptions
  mutate(date.header = as.Date(date.header) - lubridate::years(1)) %>%
  mutate(across(everything(),as.character)) #incorrect dates -- off by a year

#2006

read_excel("data/raw/TN_data/Moth Trap Data06 Entered.xlsx",sheet=1,col_names=FALSE)

formats <- xlsx_formats("data/raw/TN_data/Moth Trap Data06 Entered.xlsx")

tn2006 <-
  xlsx_cells("data/raw/TN_data/Moth Trap Data06 Entered.xlsx", sheets = 1)  %>%
  filter(col <= 19) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2006")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2007

path <- "data/raw/TN_data/Moth Traps 07.xlsx"

read_excel(path,sheet=1,col_names=FALSE)

formats <- xlsx_formats(path)

tn2007 <-
  xlsx_cells(path, sheets = 1)  %>%
  filter(col <= 17) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2007")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2008

path <- "data/raw/TN_data/Moth Traps 08.xlsx"

read_excel(path,sheet=1,col_names=FALSE)

formats <- xlsx_formats(path)

tn2008 <-
  xlsx_cells(path, sheets = 1)  %>%
  filter(col <= 17) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2008")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2009

path <- "data/raw/TN_data/Moth Traps 09.xlsx"

read_excel(path,sheet=1,col_names=FALSE)

formats <- xlsx_formats(path)

tn2009 <-
  xlsx_cells(path, sheets = 1)  %>%
  filter(col <= 17) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2009")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions


#2010

path <- "data/raw/TN_data/Moth Trap Data 2010.xlsx"

read_excel(path,sheet=1,col_names=FALSE)

formats <- xlsx_formats(path)

tn2010 <-
  xlsx_cells(path, sheets = 1)  %>%
  filter(col <= 17 & row >= 3) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(caps == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2010")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2011

path <- "data/raw/TN_data/2011 MothTrappingData.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2011 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(col <= 16 & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2011")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions


#2012

path <- "data/raw/TN_data/2012 MothTrapping Data.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2012 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(col <= 16 & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2012")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2013

path <- "data/raw/TN_data/2013 Moth Trapping Data.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2013 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(col <= 16 & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2013")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2014

path <- "data/raw/TN_data/2014MothTrappingData.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2014 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,2,17) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2014")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character))#select the important cell descriptions

#2015

path <- "data/raw/TN_data/2015 Moth Trapping Summary.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2015 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,15) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2015")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions


#2016

path <- "data/raw/TN_data/2016 Moth Trapping Summary.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2016 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,16) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2016")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2017

path <- "data/raw/TN_data/2017 Moth Trapping Summary.xlsx"

read_excel(path,sheet=1,col_names=FALSE,skip=2)

formats <- xlsx_formats(path)

tn2017 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,16) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2017")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2018

path <- "data/raw/TN_data/2018MothTrappingData.xlsx"

read_excel(path,sheet=1)

formats <- xlsx_formats(path)

tn2018 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,17) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2018")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2019

path <- "data/raw/TN_data/2019MothTrappingData wGraphs.xlsx"

read_excel(path,sheet=1)

formats <- xlsx_formats(path)

tn2019 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,17) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2019")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character)) #select the important cell descriptions

#2020

path <- "data/raw/TN_data/2020 Moth Trapping Data.xlsx"

read_excel(path,sheet=1)

formats <- xlsx_formats(path)

tn2020 <-
  xlsx_cells(path, sheets = 'Moths')  %>%
  filter(is.na(formula)) %>%
  filter(between(col,1,17) & row >= 4) %>%
  select(row, col, data_type, character, numeric,local_format_id,date,address,date)  %>% 
  filter(data_type != "blank") %>% #remove all blank cells
  mutate(caps = !str_detect(character, "[[:lower:]]")) %>%
  mutate(bold = formats$local$font$bold[local_format_id]) %>% #find which cells are underlined
  behead_if(bold == TRUE, #Species name is at the left-top and all caps
            direction = "left-up",
            name="Species") %>%
  behead_if(bold == FALSE,
            direction = 'left',
            name = 'location') %>% #location is to the left
  behead('up','date') %>% #date is at the top
  filter(str_detect(date.header, "2020")) %>%
  mutate(count = coalesce(character,as.character(numeric))) %>%
  as_tibble() %>%
  drop_na(location) %>%
  select(Species,location,date.header,count) %>%
  mutate(across(everything(),as.character))

#combine dat and add coordinates


coords <-  read_excel("data/raw/TN_data/CAPS GPS and Counties - Stewart.xlsx") %>% fill(c(`Site #`,Location)) %>%
  dplyr::select(County, Area, Location) %>% distinct()

tn_dat <- tn2004 %>% bind_rows(tn2005,tn2006,tn2007,tn2008,tn2009,tn2010,
                               tn2011,tn2012,tn2013,tn2013,tn2014,tn2015,
                               tn2016,tn2017,tn2018,tn2019,tn2020)

tn_dat2 <- tn_dat %>%
  mutate(count = coalesce(numeric,count),
         species = factor(Species),
         location = factor(location),
         date = as.Date(date.header)) %>%
  select(!c(date.header,Species,numeric)) %>%
  janitor::clean_names() %>%
  separate(location,sep=" ",into=c("County","Area")) %>% 
  filter(County != "County") %>%
  mutate(Area = gsub("[()]", "", Area),
         Area = gsub("\\,", "", Area),
         Area = fct_recode(Area, "WTREC" = "South"),
         Area = fct_recode(Area, "Brownsville" = "Brownville"),
         Area = fct_recode(Area, "19 W" = "19"),
         Area = fct_recode(Area, "Mt. Carmel" = "Mt."),
         Area = fct_recode(Area, "Maury City" = "Maury"),
         Area = fct_recode(Area, "Ag Center" = "Ag"),
         Area = fct_recode(Area, "WTREC" = "S"),
         Area = fct_recode(Area, "North" = "N"),
         Area = fct_recode(Area, "Kenton" = "ktn"),
         Area = fct_recode(Area, "Newbern" = "Nbrn"),
         Area = fct_recode(Area, "West" = "W"),
         Area = fct_recode(Area, "Bolivar" = "B", 
                           "Whiteville" = "W","
                           Millington" = "M",
                           "Covington" = "C",
                           "West" = "W",
                           "Brownsville" = "B",
                           "Alamo" = "A",
                           "Newbern" = "N",
                           "Ridgley" = "R",
                           "Kenton" = "K",
                           "Goldust" = "G"), 
         Area = case_when(
           County == 'Tipton' ~ "W",
           County == 'Hardeman' ~ "Bolivar",
           County == 'Fayette' ~ "Whiteville",
           County == 'Madison' ~ "North",
           County == 'Carroll' ~ "W",
           TRUE ~ as.character(Area)),
         Area = fct_recode(Area, "Bolivar" = "B", "Whiteville" = "W","Millington" = "M","Covington" = "C","West" = "W",
                           "Brownsville" = "B","Alamo" = "A","Newbern" = "N","Ridgley" = "R","Kenton" = "K","Goldust" = "G"), 
         Area = case_when(
           County == 'Tipton' ~ "W",
           County == 'Hardeman' ~ "Bolivar",
           County == 'Fayette' ~ "Whiteville",
           County == 'Madison' ~ "North",
           County == 'Carroll' ~ "W",
           County == 'Dyer' ~ "B",
           TRUE ~ as.character(Area)),
         County = fct_recode(County,"Lauderdale" = "Lauder"),
         Area = fct_recode(Area, "Maury City" = "Maury", "Milan - REC" = "Milan","Coleman Farm" = "Coleman"))%>%
  left_join(coords, by=c("County", "Area"))


tn_combined <- tn_dat2  %>% 
  mutate( Location = case_when(
    Area == "Maury City" ~ "35.804169, -89.217252",
    County == "Crockett" & Area == "MC" ~ "35.804169, -89.217252",
    County == "Crockett" & Area == "Millington" ~ "35.804169	-89.217252",
    Area == "MREC" ~ "35.913904, -88.743188",
    Area == "Milan" ~ "35.913904, -88.743188",
    Area == "Somerville" ~ "35.233241, -89.44296",
    TRUE ~ as.character(Location)),
    species = fct_recode(species, "CEW" = "Corn Earworm (Bollworm)",
                         "CEW" = "Corn Earworm",
                         "TBW" = "Tobacco Budworm",
                         "BAW" = "Beet Armyworm",
                         "SWCB" = "Southwestern Corn Borer")) %>%
  separate(Location,sep=", ",into=c("Longitude","Latitude"))

# Now adding coords

combined_dat2 <- tn_combined %>% 
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
  filter(count != "---") %>% 
  filter(count != "*") %>%
  filter(count != "stolen") %>% 
  filter(count != "(Other)") %>% 
  filter(count != ".") %>% 
  mutate(Location = paste(County,Area,sep="_"),
         Longitude = as.numeric(Longitude),
         Latitude = as.numeric(Latitude),
         Location = as.factor(Location),
         count = as.numeric(count),
         State = rep("Tennessee", nrow(.)),
         count = replace_na(count,0)) %>%
  dplyr::select(3:9)


TN_woy <- combined_dat2 %>% mutate(
  year = year(date),
  woy = isoweek(date)) %>%
  group_by(Location,year,woy) %>%
  summarize(count = sum(count),longitude = first(Longitude),latitude = first(Latitude),
            date = first(date)) %>% drop_na(woy,date) %>%
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


coords_combined <- coords_combined %>% dplyr::select(Site, Lat, Long, Date,CEW,`ECB-E`,`ECB-Z`,FAW,WBC) %>%
  rename(Location = "Site", Latitude = "Lat", Longitude = 'Long') %>%
  mutate(CEW = replace_na(CEW,0),
         State = rep("New York",nrow(.)))

coords_combined$CEW <- (as.numeric(coords_combined$CEW))
coords_combined$Latitude <- (as.numeric(coords_combined$Latitude))
coords_combined$Longitude <- (as.numeric(coords_combined$Longitude))

NY_woy <- coords_combined %>% mutate(
  year = year(Date),
  woy = isoweek(Date)) %>%
  pivot_longer(cols=c(5:9),names_to = "species",values_to = "count") %>%
  group_by(Location,year,woy,species) %>%
  summarize(count_sum = sum(count,na.rm=TRUE),longitude = first(Longitude),latitude = first(Latitude),
            date = first(Date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) %>%
  pivot_wider(names_from = species,values_from = count_sum)

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

names(y2007)
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
  ) %>% dplyr::select(1,2,starts_with("2007-"), latitude,longitude) %>%
  mutate_at(vars(3:59), as.numeric) %>%
  tidyr::pivot_longer(cols = c(starts_with("2007-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(1,3:8) %>%
  rename(Location = "Trap Location")

#2008


y2008 <- list[[3]]   %>%
  mutate_at(vars(starts_with("2008-")), as.numeric) %>%
  dplyr::select(1:4,starts_with("2008-")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2008-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...3",longitude = "Degrees...4") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:8) %>%
  rename(Location = "Trap Location")

#2009


y2009 <- list[[4]]  %>%
  mutate_at(vars(starts_with("2009-")), as.numeric) %>%
  dplyr::select(1,starts_with("2009-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2009-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2010

y2010 <- list[[5]]   %>%
  mutate_at(vars(starts_with("2010-")), as.numeric) %>%
  dplyr::select(1:4,starts_with("2010-")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2010-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...3",longitude = "Degrees...4") %>%
  mutate(longitude = longitude * -1) %>%
  dplyr::select(1,3:8) %>%
  rename(Location = "Trap Location")

#2011

y2011 <- list[[6]]  %>%
  mutate_at(vars(starts_with("2011-")), as.numeric) %>%
  dplyr::select(1,starts_with("2011-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2011-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2012

y2012 <- list[[7]]%>%
  mutate_at(vars(starts_with("2012-")), as.numeric) %>%
  dplyr::select(1,starts_with("2012-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2012-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2013

y2013 <- list[[8]] %>%
  mutate_at(vars(starts_with("2012-")), as.numeric) %>%
  dplyr::select(1,starts_with("2012-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2012-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date) + lubridate::years(1)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2014
# this year is missing lat and long coords

y2014 <- list[[9]] %>%
  mutate_at(vars(starts_with("2014-")), as.numeric) %>%
  dplyr::select(1,starts_with("2014-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2014-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2015

y2015 <- list[[10]] %>%
  mutate_at(vars(starts_with("2015-")), as.numeric) %>%
  dplyr::select(1,starts_with("2015-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2015-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2016

y2016 <- list[[11]] %>%
  mutate_at(vars(starts_with("2016-")), as.numeric) %>%
  dplyr::select(1,starts_with("2016-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2016-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2017

y2017 <- list[[12]] %>%
  mutate_at(vars(starts_with("2017-")), as.numeric) %>%
  dplyr::select(1,starts_with("2017-"),starts_with("Degrees..")) %>%
  tidyr::pivot_longer(cols = c(starts_with("2017-")),
                      names_to = c("Date", ".value"), 
                      names_pattern = "(.*)_(.*)") %>%
  mutate(Date = as.Date(Date)) %>%
  rename(latitude = "Degrees...4",longitude = "Degrees...5") %>%
  mutate(longitude = longitude * -1) %>%
  rename(Location = "Trap Location")

#2018

y2018 <- list[[13]] %>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% 
  filter(Location != "2") %>%
  filter(Location != "3") %>% 
  pivot_longer(cols=c(starts_with("43")),names_to="date",values_to = "count") %>%
  mutate(date = as.Date(as.integer(date), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,5:9) %>%
  pivot_wider(names_from = Insect,values_from = count)


#2019



y2019 <- list[[14]]%>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% 
  filter(Location != "2") %>%
  filter(Location != "3") %>% 
  pivot_longer(cols=c(starts_with("43")),names_to="date",values_to = "count") %>%
  mutate(date = as.Date(as.integer(date), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,5:9) %>%
  pivot_wider(names_from = Insect,values_from = count)

#2020



y2020 <- list[[15]] %>%
  fill(Location,North, West,County,.direction="down") %>%
  filter(Location != "1") %>% 
  filter(Location != "2") %>%
  filter(Location != "3") %>% 
  pivot_longer(cols=c(starts_with("43"),starts_with("44")),names_to="date",values_to = "count") %>%
  mutate(date = as.Date(as.integer(date), origin="1899-12-30"),
         longitude = as.numeric(West) * -1,
         latitude = as.numeric(North)) %>%
  dplyr::select(1,5:9) %>%
  pivot_wider(names_from = Insect,values_from = count)



MS_combined <- bind_rows(y2006, y2007,y2008,y2009,y2010,y2011,y2012,y2013,
                     y2014,y2015,y2016,y2017,y2018,y2019,y2020) %>%
  mutate(date = coalesce(Date,date)) %>%
  select(!Date)

view(MS_combined %>% filter(!complete.cases(latitude))
)

# now getting the data into the overall framework


MS_woy <- MS_combined %>% mutate(
  woy = isoweek(date),
  year = year(date)) %>%
  pivot_longer(cols=c("Weevil","CEW","TBW","BAW","SWCB"),names_to = "species",values_to = "count") %>%
  group_by(Location,year,woy,species) %>%
  summarize(count_sum = hablar::sum_(count),longitude = first(longitude),latitude = first(latitude),
            date = first(date)) %>% drop_na(woy,date) %>%
  rename("location" = Location) %>%
  pivot_wider(names_from = species,values_from=count_sum)


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
  summarize(CEW_sum = hablar::sum_(CEW), CEW_std = std(CEW),longitude = first(longitude),latitude = first(latitude),
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
names(utah_2015_1)

utah_2015_1 <- as_tibble(read.csv("data/raw/Utah_data/Results_Utah_AlfalfaCorn_1.csv",skip=5)[,1:31]) %>%
  janitor::clean_names() %>%
  dplyr::select(3,8,9,12:31) %>%
  mutate(date = lubridate::parse_date_time(date_of_collection,order="mdy"),
         location = trap_id) %>%
  mutate(across(4:23,as.numeric)) %>%
  pivot_longer(c(6:23),names_to = "species",values_to = "count") %>%
  mutate(species = case_when(
    species == "helicoverpa_sp_not_dissected" ~ "H_zea",
    species == "h_zea_dissected" ~ "H_zea",
    TRUE ~ species
  )) %>%
  select(!date_of_collection)


utah_2015_2 <- as_tibble(read_xlsx("data/raw/Utah_data/Results_Utah_AlfalfaCorn_2.xlsx",skip=5)[,1:31]) %>%
  janitor::clean_names() %>%
  dplyr::select(3,8,9,12:29) %>%
  mutate(location = trap_id) %>%
  mutate(across(4:21,as.numeric)) %>%
  pivot_longer(c(6:21),names_to = "species",values_to = "count") %>%
  mutate(species = case_when(
    species == "helicoverpa_sp_not_dissected" ~ "H_zea",
    species == "h_zea_dissected" ~ "H_zea",
    TRUE ~ species
  )) %>%
  rename(date = "date_of_collection")


y2015_UT <- bind_rows(utah_2015_1,utah_2015_2) %>% 
  mutate(number_moths_reported = coalesce(number_moths_reported,x_moths_reported)) %>%
  select(!x_moths_reported) %>%
  mutate(location = gsub("ALF","ALF15_",location)) %>%
  left_join(coords, by = c("location" = "Site_Name"))


#2016

coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2016.csv")) %>%
  mutate(Address=gsub("[^A-Z0-9]","",Adresss)) %>%
  dplyr::select(1,4:5,Address) 

y2016_UT <- as_tibble(read_xlsx("data/raw/Utah_data/Results_Utah_AlfalfaCorn_all_2016.xlsx",skip=5)) %>%
  janitor::clean_names() %>%
  dplyr::select(3,8,9,12:23,property_address) %>%
  separate(date_range,sep = "-",into=c("Start","End")) %>%
  drop_na(Start) %>%
  filter(Start != "DATE RANGE") %>%
  pivot_longer(cols=c(6:16),names_to = "species",values_to = "count") %>%
  mutate(count = as.numeric(count)) %>%
  mutate(date = parse_date_time(End,order="mdy")) %>%
  dplyr::select(3,5:8,property_address) %>%
  rename(location = "trap_id") %>%
  mutate(location = gsub("HA","ALF16_",location)) %>%
  mutate(Address=gsub("[^A-Z0-9]","",property_address)) %>%
  left_join(coords, by = ("Address")) %>%
  select(1,2,4:5,8:9)

#2017

coords <- as_tibble(read.csv("data/raw/Utah_data/coords/site_coordinates2017.csv")) %>%
  dplyr::select(1,4:5) %>%
  mutate(Site = gsub("_0(?<=\\d)","_",Site,perl = TRUE))
?read_xlsx
y2017_UT_1 <- as_tibble(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=3) %>% mutate(across(everything(),as.character))) %>%
  bind_rows(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=2) %>% mutate(across(everything(),as.character))) %>%
  bind_rows(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=4) %>% mutate(across(everything(),as.character))) %>%
  bind_rows(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=5) %>% mutate(across(everything(),as.character))) %>%
  janitor::clean_names()






y2017_UT_1 <- as_tibble(read_xlsx("data/raw/Utah_data/RESULTS_Utah_AlfalfaCorn_2017_1.xlsx",sheet=3) %>%
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
  #filter(`LURE (=target)` == "Helicoverpa armigera") %>%
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
