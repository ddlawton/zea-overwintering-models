#####
# PRISM Monthly
#   data
#####

library(tidyverse)
library(lubridate)
library(splitstackshape)
library(data.table)
library(anytime)
library(zoo)
library(viridis)

monthly <- as.tibble(read.csv("data/processed/WI_NC_prism_monthly.csv"))

monthly2 <- cSplit(setDT(monthly)[, lapply(.SD, gsub, pattern = "[][}]", 
                                    replacement = "")], names(monthly), sep=",", direction='long', fixed = FALSE, "long")
monthly2[monthly2 == ""] <- NA # define NA pattern
monthly3 <- monthly2[rowSums(is.na(monthly2)) != ncol(monthly2), ]

monthly4 <- as_tibble(monthly3 %>% tidyr::fill(c("system.index", "CEW", "CEWp","FAW", "ID", 
                               "Latitude","Longitude","county","date","doy","location","state","unix",
                               "year",".geo"), .direction = 'down') )

monthly4$dates <- anytime(monthly4$dates/1000)

monthly4$date <- as.Date(monthly4$date,format="%m/%d/%y")

monthly4$diff_months <- as.numeric(difftime(monthly4$dates, monthly4$date, units = "weeks"))/4

ggplot((monthly4 %>% filter(state != "Wisconsin")),aes(x=Season,y=(min_temps),z=(CEWp))) + stat_summary_hex() +scale_fill_viridis() + facet_wrap(~state) + theme_pubclean()


mod <- bam(CEWp ~ te(diff_months,min_temps) + as.factor(year), data=monthly4,family=poisson(),select=TRUE)

draw(mod)
summary(mod)

