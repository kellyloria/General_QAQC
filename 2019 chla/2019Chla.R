# QA'QC of:
## Chlor-a extractions from 2019 on Trilogy fluorometer
library(lme4)
library(lmerTest)
library(MuMIn) 
library(tidyverse)
library(lubridate)


ch19 <- read.csv("GL4_2019_chla.csv")
summary(ch19)

# Fix date varibles
ch19$date1 <- as.Date.character(ch19$date, format="%m/%d/%y")
range(ch19$date1)

ch19 <- transform(ch19,
                        year = as.numeric(format(date1, '%Y')),
                        nmonth = as.numeric(format(date1, '%m')),
                        doy    = as.numeric(format(date1, '%j')))

ch19.Q <- ch19 %>% 
  subset(value > 0.01) %>%
  subset(doy > 120)
range((ch19.Q$value))
summary(ch19.Q)


ch19.Q.plot <- ggplot(ch19.Q, aes( x= doy, y= (value), color= measurement, shape= measurement)) +
  geom_point(alpha = 0.7) + geom_smooth(alpha = 0.5) +
  theme_classic() + ylab("value") + 
  scale_color_manual(values=alpha(c("#416e3a", "#a88e36"), 0.65))  +
  scale_shape_manual(values=c(17, 19)) +
  facet_wrap(~year) 

ggsave("ch19.Q.plot.jpeg", ch19.Q.plot, scale = 1.8, width = 10, height = 5, units = c("cm"), dpi = 500)


ch19.cha <- ch19.Q %>% 
  subset(measurement=="Chlorophyll" & year == 2018)
summary((ch19.cha))

chla19.Q.plot <- ggplot(ch19.cha) +
  geom_point(aes( x=doy , y= (value)), color=(alpha(c("#457840"), 0.65))) +
  theme_classic() + ylab("value") + 
  geom_point(aes( x=doy , y= (Hiroba_oldchla)), color =(alpha(c("#c40a0a"), 0.65))) +
  facet_wrap(~site) 

