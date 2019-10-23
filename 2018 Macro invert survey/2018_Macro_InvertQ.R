# Created 2019-10-22 by Kelly Loria #

####
# QA'QC For:
#   Sammy Yevak 2018 REU student's Macro-invert data from the GLV
#     Contains 3 data entities: 
#           1. Taxa abundance matrix by site visit
#           2. Environmental quality per site visit
#           3. Individual macro invert traits by site visit

# Goals: 
#   1. Get in parallel form to zooplankton data
####

library(ggplot2)
library(tidyverse)
library(reshape2)

#1. Abundance:
MIAbund <- read_csv("2018 Macro invert survey/MacroInvertAbundance_2018.csv")
summary(MIAbund)

# Convert wide to long:
MI_long <- gather(MIAbund, taxa, count, Trichoptera_apataniidae_apatania:Rhynchobdellida_glossiphoniidae, 
                  factor_key=TRUE)

# Fix date varibles
MI_long$date <- as.Date.character(MI_long$collection_date, format="%m/%d/%y")
range(MI_long$date)

# Organize columns
# select for relevant parameters
names(MI_long)
MI_long_exp <- subset(MI_long, select=c(LTER_site, local_site, location, project_site, 
                                        shore, date, taxa, count))
summary(MI_long_exp)

