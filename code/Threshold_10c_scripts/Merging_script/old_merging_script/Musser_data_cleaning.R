#####
# Cleaning Musser Data
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
library(patchwork)

# 2011/2012 Data

dat <- as_tibble(read.csv("data/raw/Reisig_data/2011_2012_dat.csv")) %>%
  rename(Location = "g", Date = 'g.1', CEW = "g.3") %>%
  select(1,2,4) %>%
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
  select(Location,Lure...8,`# Moths`,`Date Checked`,Latitude,Longitude,row) %>%
  filter((Lure...8) == "CEW") %>%
  pivot_wider(names_from = (Lure...8),values_from = `# Moths`) %>%
  rename('Date' = `Date Checked`) %>% select(!row)

# 2016 data

path <- ("data/raw/Reisig_data/2016_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames

coords <- mylist$`Traps&Coords` %>% dplyr::select(1:5) %>% slice(1:12) %>% 
  drop_na() %>% separate(
    col=Coordinates,into=c("Latitude","Longitude"), sep = ", ") %>% 
  dplyr::rename("Trap" = Trap...2) %>% 
  select(2:5) %>%
  mutate(Trap=as.numeric(Trap))

count_2016 <-  mylist$`Traps&Coords` %>% dplyr::select(7:11) %>%
  rename("Trap" = Trap...8) %>%
  left_join(coords,by="Trap") %>% distinct() %>%
  mutate(Date = as.Date(as.integer(Date...7), origin = "1899-12-30"),
         Location = paste(rep('Reisig',nrow(.)),Trap,sep="_")) %>%
  select(2:4,7:10) %>%
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
  select(!Trap)

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
  select(1,5,6)

count_2018 <- mylist$`Pheromone Trap Data` %>% select(1:4) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2018_Reisig',nrow(.)),Trap,sep="_")) %>%
  select(1,4:7)

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
  select(1,5,6)

count_2019 <- mylist$DataSheet %>% select(1:4) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2019_Reisig',nrow(.)),Trap,sep="_"),
         Date = as.Date(as.integer(Date), origin = "1899-12-30")) %>%
  select(1,4:7)

# 2020 Data

path <- ("data/raw/Reisig_data/2020_Moth_Lure_Counts.xlsx")

sheetnames <- excel_sheets(path)

mylist <- lapply(excel_sheets(path), read_excel, path = path)

names(mylist) <- sheetnames


coords <- mylist$Coordinates %>% select(1:4) %>% filter(Lure == "CEW") %>%
  separate(col=`Coordinates/Address`,into=c("Latitude","Longitude"), sep = "(?<=[,])") %>%
  mutate(Latitude = sub(',', '', Latitude),
         Longitude = sub(' ', '', Longitude)) %>%
  select(1,3,4)

count_2020 <- mylist$DataSheet %>% select(1:4) %>% drop_na(`Date Checked`) %>%
  filter(Lure == "CEW") %>% rename(Trap = `Trap #`) %>% left_join(coords,by="Trap") %>%
  rename(Date = `Date Checked`,CEW = "# Moths") %>% 
  mutate(Location = paste(rep('2020_Reisig',nrow(.)),Trap,sep="_"),
         CEW = as.numeric(CEW),
         CEW = replace_na(CEW, 0),
         CEW = round(CEW/2))  %>% # this is because there were two traps set up at this location. I will take the average between the two and round up
  select(1,4:7)

# now combining all dataframes together

Reisig_dat <- rbind(count_2011_2012,count_2015,count_2017,(count_2016 %>% select(!Trap)),count_2018,count_2019,count_2020) %>% drop_na(Latitude)


#now combining together with the Anders data

WI_NC_MS_NY <- as.tibble(read.csv("data/raw/National_data_combined/WI_NC_MS_NY.csv"))


Reisig_dat <- Reisig_dat %>% 
  mutate(State = rep("North Carolina",nrow(.)))

WI_NC_MS_NY_Jul8 <- rbind(Reisig_dat,WI_NC_MS_NY) %>% drop_na(CEW)

#checking to confirm spatial distribution

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")
name(states)

old <- ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-85,-74), ylim = c(32,37), expand = FALSE)+
  geom_point(data = (WI_NC_MS_NY %>% filter(State == "North Carolina")), aes(x=as.numeric(Longitude),y=as.numeric(Latitude))) +
  theme_pubr() + theme(legend.position = "none")

new <- ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-85,-74), ylim = c(32,37), expand = FALSE)+
  geom_point(data = (WI_NC_MS_NY_Jul8 %>% filter(State == "North Carolina")), aes(x=as.numeric(Longitude),y=as.numeric(Latitude))) +
  theme_pubr() + theme(legend.position = "none")

old + new


write_csv(WI_NC_MS_NY_Jul8, file = "data/raw/National_data_combined/WI_NC_MS_NY_Jul8.csv")
