## ---------------------------
## QA'QC for GLV water chemistry:
##    https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.10.1
## 
## Author: Kelly A. Loria
## Date Created: 2020-04-20
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
## ---------------------------
# Workflow:
#   1. Manually add in sampling time and notes, combine 2 header rows, depending 
#      on Arikaree .xlxs report might need to manually add in more info
#   2. Add in dection limits
#   3. Select relevant colunms in the correct order 
#   4. Change column names to match old output
#   5. Check new data to see if it can be added to old data
## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/GeneralQ/')){
  inputDir<- '/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/GeneralQ/'
  outputDir<- '/Users/kellyloria/Desktop/' 
}

# Don't forget to 
#     1. Set output path to personal desktop 
#     2. Physically move final files (pending datamanager approval) into final folder in server

## ---------------------------
# I. Read in new data: here 2019 Water Chem "WC19"
WC19 <- read.csv(paste0(inputDir, "/WaterChem/19074_NWTLimnoReport.csv"))
summary(WC19) # need to combine 1st 2 rows into 1 header 

WC19$date1 <- as.Date.character(WC19$date, format="%m/%d/%y")
range(WC19$date1)

## ---------------------------
# 2. Replace 0 with dection limits:
#   * Dectection limits:   
#       NH4+ uEQ/L == <0.50
#       PO4 3- mg P/L == <0.0004
#       PO4 3- uEQ/L == <0.08
#       TDP mg P/L == <0.008
#       IN uMOLES/L == <0.53
#       TDP uMOLES/L == <0.04
#       IP uMOLES/L == <0.03

WC19.Q=WC19 %>% 
  mutate(NH4_uEQ_L = replace(NH4_uEQ_L, NH4_uEQ_L<0.50, '<0.50')) %>% 
  mutate(PO4_mg_L = replace(PO4_mg_L, PO4_mg_L<0.0004, '<0.0004')) %>%
  mutate(PO4_uEQ_L = replace(PO4_uEQ_L, PO4_uEQ_L<0.08, '<0.08')) %>%
  mutate(TDP_mg_L = replace(TDP_mg_L, TDP_mg_L<0.008, '<0.008'))  %>%
  mutate(IN_umol_L = replace(IN_umol_L, IN_umol_L<0.53, '<0.53')) %>%
  mutate(TDP_umol_L = replace(TDP_umol_L, TDP_umol_L<0.04, '<0.04')) %>%
  mutate(IP_umol_L = replace(IP_umol_L, IP_umol_L<0.03, '<0.03'))  %>%
  mutate(NO3_mg_L = replace(NO3_mg_L, NO3_mg_L<0.02, '<0.02')) %>% # MDL based off mg/L
  mutate(TDN_mg_L = replace(TDN_mg_L, TDN_mg_L<0.008, '<0.008')) %>% # MDL based off mg/L
  mutate(DOP_umol_L = replace(DOP_umol_L, DOP_umol_L<0.0008, '<0.0008'))
view(WC19.Q)
  

## ---------------------------
# 3. Add in columns for data no longer collected 
WC19.Q$TN <- NaN
WC19.Q$PN <- NaN
WC19.Q$TP <- NaN
WC19.Q$PP <- NaN
WC19.Q$d18O <- NaN
WC19.Q$d18O_sdev <- NaN
WC19.Q$dDeut <- NaN
WC19.Q$dD_sdev <- NaN
WC19.Q$D_excess <- NaN
WC19.Q$Trit <- NaN
WC19.Q$T_sdev <- NaN
WC19.Q$TOC <- NaN
WC19.Q$POC <- NaN

WC19.Q1 <- subset(WC19.Q, select=c(LTER_site, local_site, location, depth, year, date1,
                                   time, pH, COND_uS_cm, ANC_uEQ_L, H_uEQ_L, NH4_uEQ_L,
                                   Ca_uEQ, Mg_uEQ_L, Na_uEQ_L, K_uEQ_L, Cl_uEQ_L, NO3_uEQ_L,
                                   SO4_uEQ_L, PO4_uEQ_L, Si_umol_L, cat_sum, an_sum, chg_bal,
                                   TN, TDN_umol_L, PN, DON_umol_L, IN_umol_L, TP, TDP_umol_L, 
                                   PP, DOP_umol_L, IP_umol_L, d18O, d18O_sdev, dDeut, dD_sdev,
                                   D_excess, Trit, T_sdev, TOC, DOC_mg_L, POC, comments))

# Change names
colnames(WC19.Q1)[6] = "date"
colnames(WC19.Q1)[9] = "conduct"
colnames(WC19.Q1)[10] = "ANC"
colnames(WC19.Q1)[11] = "H.plus."
colnames(WC19.Q1)[12] = "NH4.plus."
colnames(WC19.Q1)[13] = "Ca.plus..plus."
colnames(WC19.Q1)[14] = "Mg.plus..plus."
colnames(WC19.Q1)[15] = "Na.plus."
colnames(WC19.Q1)[16] = "K.plus."
colnames(WC19.Q1)[17] = "Cl.hyphen."
colnames(WC19.Q1)[18] = "NO3.hyphen."
colnames(WC19.Q1)[19] = "SO4.hyphen..hyphen."
colnames(WC19.Q1)[20] = "PO4.hyphen..hyphen..hyphen."
colnames(WC19.Q1)[21] = "Si" 
colnames(WC19.Q1)[26] = "TDN"
colnames(WC19.Q1)[28] = "DON"
colnames(WC19.Q1)[29] = "IN"
colnames(WC19.Q1)[31] = "TDP"
colnames(WC19.Q1)[33] = "DOP"
colnames(WC19.Q1)[34] = "IP"
colnames(WC19.Q1)[43] = "DOC"

# write.csv(WC19.Q1, paste0(outputDir, "Summer2019_GLVWaterChem.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# II. Check to see if the new data fits with old:
# Read in old data:
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-nwt/10/1/454485276bd5ee8d4a8a5e30a71853a7" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "LTER_site",     
                 "local_site",     
                 "location",     
                 "depth",     
                 "year",     
                 "date",     
                 "time",     
                 "pH",     
                 "conduct",     
                 "ANC",     
                 "H.plus.",     
                 "NH4.plus.",     
                 "Ca.plus..plus.",     
                 "Mg.plus..plus.",     
                 "Na.plus.",     
                 "K.plus.",     
                 "Cl.hyphen.",     
                 "NO3.hyphen.",     
                 "SO4.hyphen..hyphen.",     
                 "PO4.hyphen..hyphen..hyphen.",     
                 "Si",     
                 "cat_sum",     
                 "an_sum",     
                 "chg_bal",     
                 "TN",     
                 "TDN",     
                 "PN",     
                 "DON",     
                 "IN",     
                 "TP",     
                 "TDP",     
                 "PP",     
                 "DOP",     
                 "IP",     
                 "d18O",     
                 "d18O_sdev",     
                 "dDeut",     
                 "dD_sdev",     
                 "D_excess",     
                 "Trit",     
                 "T_sdev",     
                 "TOC",     
                 "DOC",     
                 "POC",     
                 "comments"    ), check.names=TRUE)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(dt1$LTER_site)!="factor") dt1$LTER_site<- as.factor(dt1$LTER_site)
if (class(dt1$local_site)!="factor") dt1$local_site<- as.factor(dt1$local_site)
if (class(dt1$location)!="factor") dt1$location<- as.factor(dt1$location)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]
if (class(dt1$conduct)=="factor") dt1$conduct <-as.numeric(levels(dt1$conduct))[as.integer(dt1$conduct) ]
if (class(dt1$ANC)=="factor") dt1$ANC <-as.numeric(levels(dt1$ANC))[as.integer(dt1$ANC) ]
if (class(dt1$H.plus.)=="factor") dt1$H.plus. <-as.numeric(levels(dt1$H.plus.))[as.integer(dt1$H.plus.) ]
if (class(dt1$NH4.plus.)=="factor") dt1$NH4.plus. <-as.numeric(levels(dt1$NH4.plus.))[as.integer(dt1$NH4.plus.) ]
if (class(dt1$Ca.plus..plus.)=="factor") dt1$Ca.plus..plus. <-as.numeric(levels(dt1$Ca.plus..plus.))[as.integer(dt1$Ca.plus..plus.) ]
if (class(dt1$Mg.plus..plus.)=="factor") dt1$Mg.plus..plus. <-as.numeric(levels(dt1$Mg.plus..plus.))[as.integer(dt1$Mg.plus..plus.) ]
if (class(dt1$Na.plus.)=="factor") dt1$Na.plus. <-as.numeric(levels(dt1$Na.plus.))[as.integer(dt1$Na.plus.) ]
if (class(dt1$K.plus.)=="factor") dt1$K.plus. <-as.numeric(levels(dt1$K.plus.))[as.integer(dt1$K.plus.) ]
if (class(dt1$Cl.hyphen.)=="factor") dt1$Cl.hyphen. <-as.numeric(levels(dt1$Cl.hyphen.))[as.integer(dt1$Cl.hyphen.) ]
if (class(dt1$NO3.hyphen.)=="factor") dt1$NO3.hyphen. <-as.numeric(levels(dt1$NO3.hyphen.))[as.integer(dt1$NO3.hyphen.) ]
if (class(dt1$SO4.hyphen..hyphen.)=="factor") dt1$SO4.hyphen..hyphen. <-as.numeric(levels(dt1$SO4.hyphen..hyphen.))[as.integer(dt1$SO4.hyphen..hyphen.) ]
if (class(dt1$PO4.hyphen..hyphen..hyphen.)=="factor") dt1$PO4.hyphen..hyphen..hyphen. <-as.numeric(levels(dt1$PO4.hyphen..hyphen..hyphen.))[as.integer(dt1$PO4.hyphen..hyphen..hyphen.) ]
if (class(dt1$Si)=="factor") dt1$Si <-as.numeric(levels(dt1$Si))[as.integer(dt1$Si) ]
if (class(dt1$cat_sum)=="factor") dt1$cat_sum <-as.numeric(levels(dt1$cat_sum))[as.integer(dt1$cat_sum) ]
if (class(dt1$an_sum)=="factor") dt1$an_sum <-as.numeric(levels(dt1$an_sum))[as.integer(dt1$an_sum) ]
if (class(dt1$chg_bal)=="factor") dt1$chg_bal <-as.numeric(levels(dt1$chg_bal))[as.integer(dt1$chg_bal) ]
if (class(dt1$TN)=="factor") dt1$TN <-as.numeric(levels(dt1$TN))[as.integer(dt1$TN) ]
if (class(dt1$TDN)=="factor") dt1$TDN <-as.numeric(levels(dt1$TDN))[as.integer(dt1$TDN) ]
if (class(dt1$PN)=="factor") dt1$PN <-as.numeric(levels(dt1$PN))[as.integer(dt1$PN) ]
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]
if (class(dt1$IN)=="factor") dt1$IN <-as.numeric(levels(dt1$IN))[as.integer(dt1$IN) ]
if (class(dt1$TP)=="factor") dt1$TP <-as.numeric(levels(dt1$TP))[as.integer(dt1$TP) ]
if (class(dt1$TDP)=="factor") dt1$TDP <-as.numeric(levels(dt1$TDP))[as.integer(dt1$TDP) ]
if (class(dt1$PP)=="factor") dt1$PP <-as.numeric(levels(dt1$PP))[as.integer(dt1$PP) ]
if (class(dt1$DOP)=="factor") dt1$DOP <-as.numeric(levels(dt1$DOP))[as.integer(dt1$DOP) ]
if (class(dt1$IP)=="factor") dt1$IP <-as.numeric(levels(dt1$IP))[as.integer(dt1$IP) ]
if (class(dt1$d18O)=="factor") dt1$d18O <-as.numeric(levels(dt1$d18O))[as.integer(dt1$d18O) ]
if (class(dt1$d18O_sdev)=="factor") dt1$d18O_sdev <-as.numeric(levels(dt1$d18O_sdev))[as.integer(dt1$d18O_sdev) ]
if (class(dt1$dDeut)=="factor") dt1$dDeut <-as.numeric(levels(dt1$dDeut))[as.integer(dt1$dDeut) ]
if (class(dt1$dD_sdev)=="factor") dt1$dD_sdev <-as.numeric(levels(dt1$dD_sdev))[as.integer(dt1$dD_sdev) ]
if (class(dt1$D_excess)=="factor") dt1$D_excess <-as.numeric(levels(dt1$D_excess))[as.integer(dt1$D_excess) ]
if (class(dt1$Trit)=="factor") dt1$Trit <-as.numeric(levels(dt1$Trit))[as.integer(dt1$Trit) ]
if (class(dt1$T_sdev)=="factor") dt1$T_sdev <-as.numeric(levels(dt1$T_sdev))[as.integer(dt1$T_sdev) ]
if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]
if (class(dt1$POC)=="factor") dt1$POC <-as.numeric(levels(dt1$POC))[as.integer(dt1$POC) ]
if (class(dt1$comments)!="factor") dt1$comments<- as.factor(dt1$comments)

# Convert Missing Values to NA for non-dates
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NP")),NA,dt1$pH)
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("QNS")),NA,dt1$pH)
dt1$conduct <- ifelse((trimws(as.character(dt1$conduct))==trimws("NP")),NA,dt1$conduct)
dt1$ANC <- ifelse((trimws(as.character(dt1$ANC))==trimws("NP")),NA,dt1$ANC)
dt1$ANC <- ifelse((trimws(as.character(dt1$ANC))==trimws("QNS")),NA,dt1$ANC)
dt1$H.plus. <- ifelse((trimws(as.character(dt1$H.plus.))==trimws("NP")),NA,dt1$H.plus.)
dt1$H.plus. <- ifelse((trimws(as.character(dt1$H.plus.))==trimws("QNS")),NA,dt1$H.plus.)
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("NP")),NA,dt1$NH4.plus.)
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.38")),NA,dt1$NH4.plus.)
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.40")),NA,dt1$NH4.plus.)
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("<0.22")),NA,dt1$NH4.plus.)
dt1$NH4.plus. <- ifelse((trimws(as.character(dt1$NH4.plus.))==trimws("u")),NA,dt1$NH4.plus.)
dt1$Ca.plus..plus. <- ifelse((trimws(as.character(dt1$Ca.plus..plus.))==trimws("NP")),NA,dt1$Ca.plus..plus.)
dt1$Ca.plus..plus. <- ifelse((trimws(as.character(dt1$Ca.plus..plus.))==trimws("QNS")),NA,dt1$Ca.plus..plus.)
dt1$Mg.plus..plus. <- ifelse((trimws(as.character(dt1$Mg.plus..plus.))==trimws("NP")),NA,dt1$Mg.plus..plus.)
dt1$Mg.plus..plus. <- ifelse((trimws(as.character(dt1$Mg.plus..plus.))==trimws("QNS")),NA,dt1$Mg.plus..plus.)
dt1$Na.plus. <- ifelse((trimws(as.character(dt1$Na.plus.))==trimws("NP")),NA,dt1$Na.plus.)
dt1$Na.plus. <- ifelse((trimws(as.character(dt1$Na.plus.))==trimws("QNS")),NA,dt1$Na.plus.)
dt1$K.plus. <- ifelse((trimws(as.character(dt1$K.plus.))==trimws("NP")),NA,dt1$K.plus.)
dt1$K.plus. <- ifelse((trimws(as.character(dt1$K.plus.))==trimws("QNS")),NA,dt1$K.plus.)
dt1$Cl.hyphen. <- ifelse((trimws(as.character(dt1$Cl.hyphen.))==trimws("NP")),NA,dt1$Cl.hyphen.)
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("NP")),NA,dt1$NO3.hyphen.)
dt1$NO3.hyphen. <- ifelse((trimws(as.character(dt1$NO3.hyphen.))==trimws("u")),NA,dt1$NO3.hyphen.)
dt1$SO4.hyphen..hyphen. <- ifelse((trimws(as.character(dt1$SO4.hyphen..hyphen.))==trimws("NP")),NA,dt1$SO4.hyphen..hyphen.)
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("NP")),NA,dt1$PO4.hyphen..hyphen..hyphen.)
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.06")),NA,dt1$PO4.hyphen..hyphen..hyphen.)
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("<0.05")),NA,dt1$PO4.hyphen..hyphen..hyphen.)
dt1$PO4.hyphen..hyphen..hyphen. <- ifelse((trimws(as.character(dt1$PO4.hyphen..hyphen..hyphen.))==trimws("u")),NA,dt1$PO4.hyphen..hyphen..hyphen.)
dt1$Si <- ifelse((trimws(as.character(dt1$Si))==trimws("NP")),NA,dt1$Si)
dt1$cat_sum <- ifelse((trimws(as.character(dt1$cat_sum))==trimws("NP")),NA,dt1$cat_sum)
dt1$cat_sum <- ifelse((trimws(as.character(dt1$cat_sum))==trimws("QNS")),NA,dt1$cat_sum)
dt1$an_sum <- ifelse((trimws(as.character(dt1$an_sum))==trimws("NP")),NA,dt1$an_sum)
dt1$an_sum <- ifelse((trimws(as.character(dt1$an_sum))==trimws("QNS")),NA,dt1$an_sum)
dt1$chg_bal <- ifelse((trimws(as.character(dt1$chg_bal))==trimws("NP")),NA,dt1$chg_bal)
dt1$chg_bal <- ifelse((trimws(as.character(dt1$chg_bal))==trimws("QNS")),NA,dt1$chg_bal)
dt1$TN <- ifelse((trimws(as.character(dt1$TN))==trimws("NP")),NA,dt1$TN)
dt1$TN <- ifelse((trimws(as.character(dt1$TN))==trimws("QNS")),NA,dt1$TN)
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("NP")),NA,dt1$TDN)
dt1$TDN <- ifelse((trimws(as.character(dt1$TDN))==trimws("EQCL")),NA,dt1$TDN)
dt1$PN <- ifelse((trimws(as.character(dt1$PN))==trimws("NP")),NA,dt1$PN)
dt1$PN <- ifelse((trimws(as.character(dt1$PN))==trimws("u")),NA,dt1$PN)
dt1$PN <- ifelse((trimws(as.character(dt1$PN))==trimws("QNS")),NA,dt1$PN)
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NP")),NA,dt1$DON)
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("u")),NA,dt1$DON)
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("EQCL")),NA,dt1$DON)
dt1$IN <- ifelse((trimws(as.character(dt1$IN))==trimws("NP")),NA,dt1$IN)
dt1$IN <- ifelse((trimws(as.character(dt1$IN))==trimws("u")),NA,dt1$IN)
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("NP")),NA,dt1$TP)
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("u")),NA,dt1$TP)
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("QNS")),NA,dt1$TP)
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("NP")),NA,dt1$TDP)
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.04")),NA,dt1$TDP)
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("<0.01")),NA,dt1$TDP)
dt1$TDP <- ifelse((trimws(as.character(dt1$TDP))==trimws("u")),NA,dt1$TDP)
dt1$PP <- ifelse((trimws(as.character(dt1$PP))==trimws("NP")),NA,dt1$PP)
dt1$PP <- ifelse((trimws(as.character(dt1$PP))==trimws("u")),NA,dt1$PP)
dt1$PP <- ifelse((trimws(as.character(dt1$PP))==trimws("QNS")),NA,dt1$PP)
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("NP")),NA,dt1$DOP)
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("u")),NA,dt1$DOP)
dt1$DOP <- ifelse((trimws(as.character(dt1$DOP))==trimws("EQCL")),NA,dt1$DOP)
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("NP")),NA,dt1$IP)
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("<0.02")),NA,dt1$IP)
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("<0.05")),NA,dt1$IP)
dt1$IP <- ifelse((trimws(as.character(dt1$IP))==trimws("u")),NA,dt1$IP)
dt1$d18O <- ifelse((trimws(as.character(dt1$d18O))==trimws("NP")),NA,dt1$d18O)
dt1$d18O_sdev <- ifelse((trimws(as.character(dt1$d18O_sdev))==trimws("NP")),NA,dt1$d18O_sdev)
dt1$dDeut <- ifelse((trimws(as.character(dt1$dDeut))==trimws("NP")),NA,dt1$dDeut)
dt1$dD_sdev <- ifelse((trimws(as.character(dt1$dD_sdev))==trimws("NP")),NA,dt1$dD_sdev)
dt1$D_excess <- ifelse((trimws(as.character(dt1$D_excess))==trimws("NP")),NA,dt1$D_excess)
dt1$Trit <- ifelse((trimws(as.character(dt1$Trit))==trimws("NP")),NA,dt1$Trit)
dt1$T_sdev <- ifelse((trimws(as.character(dt1$T_sdev))==trimws("NP")),NA,dt1$T_sdev)
dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("NP")),NA,dt1$TOC)
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NP")),NA,dt1$DOC)
dt1$POC <- ifelse((trimws(as.character(dt1$POC))==trimws("NP")),NA,dt1$POC)
dt1$POC <- ifelse((trimws(as.character(dt1$POC))==trimws("u")),NA,dt1$POC)
dt1$POC <- ifelse((trimws(as.character(dt1$POC))==trimws("EQCL")),NA,dt1$POC)
dt1$comments <- as.factor(ifelse((trimws(as.character(dt1$comments))==trimws("NaN")),NA,as.character(dt1$comments)))
summary(dt1)

dt1.Q=dt1 %>% 
  mutate(NH4.plus. = replace(NH4.plus., NH4.plus.<0.50, '<0.50')) %>% 
  mutate(PO4.hyphen..hyphen..hyphen. = replace(PO4.hyphen..hyphen..hyphen., PO4.hyphen..hyphen..hyphen.<0.08, '<0.08')) %>%
  mutate(IN = replace(IN, IN<0.53, '<0.53')) %>%
  mutate(TDP = replace(TDP, TDP<0.04, '<0.04')) %>%
  mutate(IP = replace(IP, IP<0.03, '<0.03'))  %>%
  mutate(DOP = replace(DOP, DOP<0.0008, '<0.0008'))
# write.csv(dt1.Q, paste0(outputDir, "OLD_GLVWaterChem.csv")) # complied data file of all DO sensors along buoy line

# Merge new data with the old:
WCtest <- rbind(dt1, WC19.Q1)
summary(WCtest)

WCtest.plot <- ggplot(WCtest) +
  geom_point(aes( x=date , y= (pH)), color=(alpha(c("#457840"), 0.65))) +
  theme_classic() +
  facet_wrap(~year) 

## ---------------------------
# III. Fix 2017 data which was also missing 

WC17 <- read.csv(paste0(inputDir, "/WaterChem/17DMGL4_Done.csv"))
summary(WC17)

WC17$date1 <- as.Date.character(WC17$date, format="%m/%d/%y")
range(WC17$date1)


## ---------------------------
# 1. Replace 0 with dection limits:
#   * Dectection limits:   
#       NH4+ uEQ/L == <0.50
#       PO4 3- mg P/L == <0.0004
#       PO4 3- uEQ/L == <0.08
#       TDP mg P/L == <0.008
#       IN uMOLES/L == <0.53
#       TDP uMOLES/L == <0.04
#       IP uMOLES/L == <0.03

WC17.Q=WC17 %>% 
  mutate(NH4_uEQ_L = replace(NH4_uEQ_L, NH4_uEQ_L<0.50, '<0.50')) %>% 
  mutate(PO4_mg_L = replace(PO4_mg_L, PO4_mg_L<0.0004, '<0.0004')) %>%
  mutate(PO4_uEQ_L = replace(PO4_uEQ_L, PO4_uEQ_L<0.08, '<0.08')) %>%
  mutate(TDP_mg_L = replace(TDP_mg_L, TDP_mg_L<0.008, '<0.008'))  %>%
  mutate(IN_umol_L = replace(IN_umol_L, IN_umol_L<0.53, '<0.53')) %>%
  mutate(TDP_umol_L = replace(TDP_umol_L, TDP_umol_L<0.04, '<0.04')) %>%
  mutate(IP_umol_L = replace(IP_umol_L, IP_umol_L<0.03, '<0.03'))  %>%
  #mutate(NO3_mg_L = replace(NO3_mg_L, NO3_mg_L<0.02, '<0.02')) %>% # MDL based off mg/L
  mutate(TDN_mg_L = replace(TDN_mg_L, TDN_mg_L<0.008, '<0.008')) %>% # MDL based off mg/L
  mutate(DOP_umol_L = replace(DOP_umol_L, DOP_umol_L<0.0008, '<0.0008'))
#view(WC17.Q)

## ---------------------------
# 2. Add in columns for data no longer collected 
WC17.Q$TN <- NaN
WC17.Q$PN <- NaN
WC17.Q$TP <- NaN
WC17.Q$PP <- NaN
WC17.Q$d18O <- NaN
WC17.Q$d18O_sdev <- NaN
WC17.Q$dDeut <- NaN
WC17.Q$dD_sdev <- NaN
WC17.Q$D_excess <- NaN
WC17.Q$Trit <- NaN
WC17.Q$T_sdev <- NaN
WC17.Q$TOC <- NaN
WC17.Q$POC <- NaN
WC17.Q$comments <- NaN

WC17.Q1 <- subset(WC17.Q, select=c(LTER_site, local_site, location, depth, year, date1,
                                   time, pH, COND_uS_cm, ANC_uEQ_L, H_uEQ_L, NH4_uEQ_L,
                                   Ca_uEQ, Mg_uEQ_L, Na_uEQ_L, K_uEQ_L, Cl_uEQ_L, NO3_uEQ_L,
                                   SO4_uEQ_L, PO4_uEQ_L, Si_umol_L, cat_sum, an_sum, chg_bal,
                                   TN, TDN_umol_L, PN, DON_umol_L, IN_umol_L, TP, TDP_umol_L, 
                                   PP, DOP_umol_L, IP_umol_L, d18O, d18O_sdev, dDeut, dD_sdev,
                                   D_excess, Trit, T_sdev, TOC, DOC_mg_L, POC, comments))

# Change names
colnames(WC17.Q1)[6] = "date"
colnames(WC17.Q1)[9] = "conduct"
colnames(WC17.Q1)[10] = "ANC"
colnames(WC17.Q1)[11] = "H.plus."
colnames(WC17.Q1)[12] = "NH4.plus."
colnames(WC17.Q1)[13] = "Ca.plus..plus."
colnames(WC17.Q1)[14] = "Mg.plus..plus."
colnames(WC17.Q1)[15] = "Na.plus."
colnames(WC17.Q1)[16] = "K.plus."
colnames(WC17.Q1)[17] = "Cl.hyphen."
colnames(WC17.Q1)[18] = "NO3.hyphen."
colnames(WC17.Q1)[19] = "SO4.hyphen..hyphen."
colnames(WC17.Q1)[20] = "PO4.hyphen..hyphen..hyphen."
colnames(WC17.Q1)[21] = "Si" 
colnames(WC17.Q1)[26] = "TDN"
colnames(WC17.Q1)[28] = "DON"
colnames(WC17.Q1)[29] = "IN"
colnames(WC17.Q1)[31] = "TDP"
colnames(WC17.Q1)[33] = "DOP"
colnames(WC17.Q1)[34] = "IP"
colnames(WC17.Q1)[43] = "DOC"

# write.csv(WC17.Q1, paste0(outputDir, "Summer2017_GLVWaterChem.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# IV. Fix 2017 data which was also missing 
# Merge new data with the old:
WCtest2 <- rbind(WCtest, WC17.Q1)
summary(WCtest2)

WCtest.plot2 <- ggplot(WCtest2) +
  geom_point(aes( x=date , y= (pH)), color=(alpha(c("#457840"), 0.65))) +
  theme_classic() +
  facet_wrap(~year) 

# write.csv(WCtest, paste0(outputDir, "ALL_GLVWaterChem.csv")) # complied data file of all DO sensors along buoy line

# found some unflagged values in 2016 so fixing that too
WC16 <- subset(dt1, year==2016)
summary(WC16)
names(WC16)

WC16.Q=WC16 %>% 
  mutate(NH4.plus. = replace(NH4.plus., NH4.plus.<0.50, '<0.50')) %>% 
  mutate(NO3.hyphen. = replace(NO3.hyphen., NO3.hyphen.<0.008, '<0.008')) %>% #mg/L
  mutate(TDN = replace(TDN, TDN<0.20, '<0.02')) %>% #mg/L
  mutate(PO4.hyphen..hyphen..hyphen. = replace(PO4.hyphen..hyphen..hyphen., PO4.hyphen..hyphen..hyphen.<0.08, '<0.08')) %>%
  mutate(DOP = replace(DOP, DOP<0.0008, '<0.0008')) %>% #DOP in mg P/L 
  mutate(IN = replace(IN, IN<0.53, '<0.53')) %>% 
  mutate(TDP = replace(TDP, TDP<0.04, '<0.04')) %>%
  mutate(IP = replace(IP, IP<0.03, '<0.03')) %>%
  mutate(DOP = replace(DOP, DOP<0.0008, '<0.0008'))

WC16.Q%>%select(date, location, depth)%>%duplicated()%>%sum() # 6 dups

View(WC16.Q%>%
       inner_join(
         WC16.Q %>%
           group_by(date, location, depth) %>%
           summarize(ct=dplyr::n())%>% filter(ct>1)))

# Remove values:
WC16.Q2 = WC16.Q %>%
  distinct(date, location, depth, .keep_all = TRUE)

WC16.Q2%>%select(date, location, depth)%>%duplicated()%>%sum() 

# write.csv(WC16, paste0(outputDir, "Summer2016WaterChem_ISSUE.csv")) # complied data file of all DO sensors along buoy line
# write.csv(WC16.Q2, paste0(outputDir, "Summer2016WaterChem.csv")) # complied data file of all DO sensors along buoy line


# Take 2 on 2016 QAQC ~~~ not great.
WC16T <- read.csv(paste0(inputDir, "/WaterChem/16dmgl4.csv"))
summary(WC16T) # need to combine 1st 2 rows into 1 header 

WC16T$date1 <- as.Date.character(WC16T$date, format="%m/%d/%y")
range(WC16T$date1)

WC16T.Q=WC16T %>% 
  mutate(NH4_uEQ_L = replace(NH4_uEQ_L, NH4_uEQ_L<0.50, '<0.50')) %>% 
  mutate(PO4_mg_L = replace(PO4_mg_L, PO4_mg_L<0.0004, '<0.0004')) %>%
  mutate(PO4_uEQ_L = replace(PO4_uEQ_L, PO4_uEQ_L<0.08, '<0.08')) %>%
  mutate(TDP_mg_L = replace(TDP_mg_L, TDP_mg_L<0.008, '<0.008'))  %>%
  mutate(IN_umol_L = replace(IN_umol_L, IN_umol_L<0.53, '<0.53')) %>%
  mutate(TDP_umol_L = replace(TDP_umol_L, TDP_umol_L<0.04, '<0.04')) %>%
  mutate(IP_umol_L = replace(IP_umol_L, IP_umol_L<0.03, '<0.03'))  %>%
  mutate(NO3_mg_L = replace(NO3_mg_L, NO3_mg_L<0.02, '<0.02')) %>% # MDL based off mg/L
  mutate(TDN_mg_L = replace(TDN_mg_L, TDN_mg_L<0.008, '<0.008')) %>% # MDL based off mg/L
  mutate(DOP_umol_L = replace(DOP_umol_L, DOP_umol_L<0.04, '<0.04'))

# 2. Add in columns for data no longer collected 
WC16T.Q$TN <- NaN
WC16T.Q$PN <- NaN
WC16T.Q$TP <- NaN
WC16T.Q$PP <- NaN
WC16T.Q$d18O <- NaN
WC16T.Q$d18O_sdev <- NaN
WC16T.Q$dDeut <- NaN
WC16T.Q$dD_sdev <- NaN
WC16T.Q$D_excess <- NaN
WC16T.Q$Trit <- NaN
WC16T.Q$T_sdev <- NaN
WC16T.Q$TOC <- NaN
WC16T.Q$POC <- NaN
WC16T.Q$comments <- NaN

WC16T.Q1 <- subset(WC16T.Q, select=c(LTER_site, local_site, depth, date1,
                                   time, pH, COND_uS_cm, ANC_uEQ_L, H_uEQ_L, NH4_uEQ_L,
                                   Ca_uEQ, Mg_uEQ_L, Na_uEQ_L, K_uEQ_L, Cl_uEQ_L, NO3_uEQ_L,
                                   SO4_uEQ_L, PO4_uEQ_L, Si_umol_L, cat_sum, an_sum, chg_bal,
                                   TN, TDN_umol_L, PN, DON_umol_L, IN_umol_L, TP, TDP_umol_L, 
                                   PP, DOP_umol_L, IP_umol_L, d18O, d18O_sdev, dDeut, dD_sdev,
                                   D_excess, Trit, T_sdev, TOC, DOC_mg_L, POC, comments))

# Change names
colnames(WC17.Q1)[6] = "date"
colnames(WC17.Q1)[9] = "conduct"
colnames(WC17.Q1)[10] = "ANC"
colnames(WC17.Q1)[11] = "H.plus."
colnames(WC17.Q1)[12] = "NH4.plus."
colnames(WC17.Q1)[13] = "Ca.plus..plus."
colnames(WC17.Q1)[14] = "Mg.plus..plus."
colnames(WC17.Q1)[15] = "Na.plus."
colnames(WC17.Q1)[16] = "K.plus."
colnames(WC17.Q1)[17] = "Cl.hyphen."
colnames(WC17.Q1)[18] = "NO3.hyphen."
colnames(WC17.Q1)[19] = "SO4.hyphen..hyphen."
colnames(WC17.Q1)[20] = "PO4.hyphen..hyphen..hyphen."
colnames(WC17.Q1)[21] = "Si" 
colnames(WC17.Q1)[26] = "TDN"
colnames(WC17.Q1)[28] = "DON"
colnames(WC17.Q1)[29] = "IN"
colnames(WC17.Q1)[31] = "TDP"
colnames(WC17.Q1)[33] = "DOP"
colnames(WC17.Q1)[34] = "IP"
colnames(WC17.Q1)[43] = "DOC"







