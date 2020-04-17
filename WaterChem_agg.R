## ---------------------------
## QA'QC for 1 C7 sensor optically formatted to measure chlorophyll-a
##    ongoing space for aggregation of new deployment data
##
## Author: Kelly A. Loria
## Date Created: 2020-03-19
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(zoo)

## ---------------------------
# File path setup:
if (dir.exists('/Volumes/data/data2/rawarchive/gl4/buoy/')){
  inputDir<- '/Volumes/data/data2/rawarchive/gl4/buoy/'
  outputDir<- '/Users/kellyloria/Desktop/' 
}
# Don't forget to 
#     1. Set output path to personal desktop 
#     2. Physically move final files (pending datamanager approval) into final folder in server

## ---------------------------
# I. Summer 2018 deployment
old.datC7 <- read.csv(paste0(inputDir, "2018_2019/C7/1808_1907_deployment/gl4.buoy.PMEC7.data.csv"), header=T)

# 1. Fix timestamp - so it is no longer a character:
old.datC7$timestamp1 <- as.POSIXct(old.datC7$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datC7$timestamp1)
summary(old.datC7)

# 2. Check data distribution through plots:
qplot(timestamp1, C7_output, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# III. Winter 2018 Deployment:
#     C7 sensor @ 3m

# 1. Read in new raw data at depth (for 2018-2019): C7_240115_180823_190723_3m.TXT
C7.3m <- read.delim(paste0(inputDir, "2018_2019/C7/1907_1908_deployment/C7_240115_180823_190723_3m.TXT"), header=T, sep = ',')
names(C7.3m)
summary(C7.3m)

# 2. Fix timestamp
C7.3m$timestamp1 <- as.POSIXct(C7.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.3m$timestamp1)

# 3. Restrict for date range of deployment:
C7.3m <- subset(C7.3m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(C7.3m$timestamp1)

# 4. Plot the data:
qplot(timestamp1, Sensor, data = C7.3m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# 5. Add in column for depth, deployment and sensor 
C7.3m$depth <- 3
C7.3m$deployment <- "Winter2018"
C7.3m$sensor <- 240115

## ---------------------------
# IV Combine Summer2018 + Winter2018 data

# 1. Add in column for year 
PME_C7_winter18 <- transform(C7.3m,
                             year = as.numeric(format(timestamp1, '%Y')))

# 2. Select for relevant parameters
old.datC7_1 <- subset(old.datC7, select=c(sensor, deployment, year, timestamp1, depth,
                                          temperature, C7_output, gain,
                                          battery))

# 3. Select for relevant parameters
PME_C7_winter18_1 <- subset(PME_C7_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, Sensor, Gain,
                                                      Battery))
# 4. Change names of column to match data
colnames(PME_C7_winter18_1)[6] = "temperature"
colnames(PME_C7_winter18_1)[7] = "C7_output"
colnames(PME_C7_winter18_1)[8] = "gain"
colnames(PME_C7_winter18_1)[10] = "battery"

# 5. Add winter 2018 to summer 2018
PME_C7_agg18 <- rbind(old.datC7_1, PME_C7_winter18_1)
summary(PME_C7_agg18)

# 6. Plot and facet by deployment:
p <- ggplot(PME_C7_agg18, aes(x=timestamp1, y=(C7_output), colour =as.factor(deployment))) + # can swap color for deployment too 
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  theme_classic() + xlab("Time stamp") 


## ---------------------------
# V. Summer 2019 Deployment

# 1. Read in data:
C7.9m <- read.delim(paste0(inputDir, "2018_2019/C7/1907_1908_deployment/C7_240115_190730_190820_9m.TXT"), header=T, sep = ',')

# 2. Fix timestamp
C7.9m$timestamp1 <- as.POSIXct(C7.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.9m$timestamp1)

# 3. Restrict for date range
C7.9m <- subset(C7.9m,timestamp1 >= as.POSIXct('2019-07-30 12:00:00') & 
                  timestamp1 <= as.POSIXct('2019-08-20 00:00:00'))

# 4. Plot the data:
qplot(timestamp1, Sensor, data = C7.9m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# 5. Compare C7 output with chl-a extraction values. 
#       Need to extract just dates from timestamps and then restrict for morining sampling
C7.9m <- transform(C7.9m, ndate = as.Date(timestamp1))
C7.9m.1 <- with(C7.9m, C7.9m[hour(timestamp1)>= 10 & hour(timestamp1) < 12 , ] )

# 6. Add in column for depth, deployment and sensor 
C7.9m$depth <- 9
C7.9m$deployment <- "Summer2019"
C7.9m$sensor <- 240115

## ---------------------------
# VI. All C7 data + Summer2019 data

# 1. Add in variable for year: 
PME_C7_summer2019 <- transform(C7.9m,
                               year = as.numeric(format(timestamp1, '%Y')))
names(PME_C7_summer2019)
names(PME_C7_summer2019)

# 2. Select for relevant parameters
PME_C7_summer2019_1 <- subset(PME_C7_summer2019, select=c(sensor, deployment, year, timestamp1, depth,
                                                          Temperature, Sensor, Gain,
                                                          Battery))

# 3. change names
colnames(PME_C7_summer2019_1)[6] = "temperature"
colnames(PME_C7_summer2019_1)[7] = "C7_output"
colnames(PME_C7_summer2019_1)[8] = "gain"
colnames(PME_C7_summer2019_1)[10] = "battery"

# 4. Add winter 2018 to summer 2018
PME_C7_agg19 <- rbind(PME_C7_agg18, PME_C7_summer2019_1)
summary(PME_C7_agg19)

# 5. Fix column names
colnames(PME_C7_agg19)[4] = "timestamp"

# 6. Plot and facet by deployment:
p <- ggplot(PME_C7_agg19, aes(x=timestamp, y=(C7_output), colour =as.factor(depth))) +
  geom_point(alpha = 0.5)  +
  theme_classic() + xlab("Time stamp")

## ---------------------------
# VII. Final QA'QC for temperature

# 1. Remove all the temperature values out of range 0-35 degreeC
#PME_C7_agg19 <- subset(PME_C7_agg19, temperature >= 0)

# 2. Flag temperature adn C7 values:
PME_C7_agg19.Q=PME_C7_agg19 %>% 
  mutate(temperature=ifelse(temperature>35, NA, temperature)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnC7=rollapply(C7_output, width = 15, FUN = mean, fill=NA),
         sdC7=rollapply(C7_output, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loC7=mnC7- (3*sdC7), hiC7=mnC7+ (3*sdC7))%>%
  full_join(., PME_C7_agg19)%>% #then use case_when to sort the final flags
  mutate(
    flag_temperature=
      case_when( #may as well add the m in here since your metadata days that flag was used
        is.na(temperature) ~ 'm',
        temperature>35 ~ 'q',
        temperature<loT&!is.na(loT) ~ 'o',
        temperature>hiT&!is.na(hiT) ~ 'o',
        temperature<0 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_C7=
      case_when( #may as well add the m in here since your metadata days that flag was used
        C7_output<loC7&!is.na(loC7) ~ 'o',
        C7_output>hiC7&!is.na(hiC7) ~ 'o', 
        timestamp <= as.POSIXct('2018-07-29 00:00:00') ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_battery=
      case_when( 
        battery<1.5 ~ 'q', TRUE ~ 'n'))

# 3.Check the flag 
p <- ggplot(PME_C7_agg19.Q, aes(x=timestamp, y=(C7_output), colour =as.factor(flag_C7), shape= deployment)) +
  geom_point(alpha = 0.7)  +
  theme_classic() #+ facet_wrap(~flag_temperature)

# 4. Remove unwanted variables:
PME_C7_agg19.Q2 <- subset(PME_C7_agg19.Q, select=c(sensor, deployment, year, timestamp, depth,
                                                   temperature, C7_output, gain,
                                                   battery, flag_temperature, flag_C7, flag_battery))

#   5. Double chec for duplicated values:
PME_C7_agg19.Q2%>%select(deployment, timestamp, depth)%>%duplicated()%>%sum() # 2 dups

View(PME_C7_agg19.Q2%>%
       inner_join(
         PME_C7_agg19.Q2 %>%
           group_by(deployment, timestamp, depth) %>%
           summarize(ct=dplyr::n())%>% filter(ct>1)))

# Remove values:
PME_C7_agg19.Q3 = PME_C7_agg19.Q2 %>%
  distinct(deployment, timestamp, depth, .keep_all = TRUE)

PME_C7_agg19.Q3%>%select(deployment, timestamp, depth)%>%duplicated()%>%sum() 

# 5. Export and save data:
# write.csv(PME_C7_agg19.Q3, paste0(outputDir, "Summer2019_PME_C7.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# VIII. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * Back ground information for users:
#       link to product mannual: https://www.turnerdesigns.com/cyclops-7f-submersible-fluorometer?lightbox=dataItem-jd6b16b81
#       And here: https://www.pme.com/wp-content/uploads/2014/07/Manual1.pdf