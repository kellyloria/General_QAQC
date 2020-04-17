# QA'QC of:
## Water quality 

library(lme4)
library(lmerTest)
library(MuMIn) 
library(tidyverse)
library(lubridate)

setwd("~/Documents/Niwot LTER 2017-2019/GeneralQ/General_QAQC/2019 water quality")

WQ19 <- read.csv("water_quality_GLV19.csv")
summary(WQ19)

WQ18 <- read.csv("2018_water_quality_GLV.dm.data_v2.csv")
summary(WQ18)


# DO%, sat 

# Fix date varibles
WQ19$date1 <- as.Date.character(WQ19$date, format="%m/%d/%y")
range(ch19$date1)