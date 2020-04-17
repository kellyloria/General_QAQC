
library(lme4)
library(lmerTest)# for p-value
library(MuMIn) # forr squared
library(nlme)
library(car)
library(gridExtra)
library(tidyverse)
library(lubridate)

#
z19com <- read.csv(file.choose())
names(z19com)

z19com$date1 <- as.Date.character(z19com$date, format="%m/%d/%y")
range(z19com$date1)
summary(z19com$date1)

names(z19com)
summary(z19com)

z19com.qa <- z19com %>% 
  group_by(local_site, date1, location) %>% 
  summarise("water_volume" = sum(liters, na.rm = T),
            "pull_number" = sum(Pull_dummy, na.rm = T),
            "total"= sum(Total.Specimen.number, na.rm = T),
            "D.rosea"= sum(Daphnia.rosea, na.rm = T),
            "D.rosea_neonate"= sum(Daphnia.rosea.Neonate, na.rm = T),
            "D.pulicaria"= sum(Daphnia.pulicaria, na.rm = T),
            "D.pulicaria_neonate"= sum(Daphnia.pulex.Neonate, na.rm = T),
            "H.gibberum"= sum(Holopedium.gibberum, na.rm = T),
            "H.gibberum_neonate"= sum(Holopedium.gibberum.neonate, na.rm = T),
            "H.shoshone"= sum(Hesperodiaptomus.shoshone, na.rm = T),
            "D.thomasi"= sum(Diacyclops.thomasi, na.rm = T),
            "naupli"= sum(naupilus, na.rm = T),
            "Bosmina"= sum(Bosmina, na.rm = T),
            "Chydoridae"= sum(Chydoridae, na.rm = T),
            "Ascomorpha"= sum(Ascomorpha, na.rm = T),
            "Asplanchna"= sum(Asplanchna.spp., na.rm = T),
            "Conochiloides"= sum(Conochiloides.spp., na.rm = T),
            "Kellicottia"= sum(Kellicottia.sp1, na.rm = T),
            "Keratella"= sum(Keratella.sp1, na.rm = T),
            "Notholca"= sum(Notholca.spp., na.rm = T),
            "Ostracoda"= sum(Ostracoda.spp., na.rm = T),
            "Chaoborus"= sum(Chaoborus, na.rm = T),
            "Chronomid"= sum(chronomid1, na.rm = T),
            "Odonata"= sum(Dragonfly.larvae, na.rm = T),
            "Mite"= sum(Mite, na.rm = T),
            "Nematoda"= sum(Nematoda, na.rm = T),
            "Unknown"= sum(unknown, na.rm = T))
names(z19com.qa)
summary(z19com.qa)


z19com.qa$total.D <- (z19com.qa$total/ z19com.qa$water_volume)
z19com.qa$D.rosea.D <- (z19com.qa$D.rosea/ z19com.qa$water_volume)
z19com.qa$D.rosea_neonate.D <- (z19com.qa$D.rosea_neonate/ z19com.qa$water_volume)
z19com.qa$D.pulicaria.D <- (z19com.qa$D.pulicaria/ z19com.qa$water_volume)
z19com.qa$D.pulicaria_neonate.D <- (z19com.qa$D.pulicaria_neonate/ z19com.qa$water_volume)
z19com.qa$H.gibberum.D <- (z19com.qa$H.gibberum/ z19com.qa$water_volume)
z19com.qa$H.gibberum_neonate.D <- (z19com.qa$H.gibberum_neonate/ z19com.qa$water_volume)
z19com.qa$H.shoshone.D <- (z19com.qa$H.shoshone/ z19com.qa$water_volume)
z19com.qa$D.thomasi.D <- (z19com.qa$D.thomasi/ z19com.qa$water_volume)
z19com.qa$naupli.D <- (z19com.qa$naupli/ z19com.qa$water_volume)
z19com.qa$Bosmina.D <- (z19com.qa$Bosmina/ z19com.qa$water_volume)
z19com.qa$Chydoridae.D <- (z19com.qa$Chydoridae/ z19com.qa$water_volume)
z19com.qa$Ascomorpha.D <- (z19com.qa$Ascomorpha/ z19com.qa$water_volume)
z19com.qa$Asplanchna.D <- (z19com.qa$Asplanchna/ z19com.qa$water_volume)
z19com.qa$Conochiloides.D <- (z19com.qa$Conochiloides/ z19com.qa$water_volume)
z19com.qa$Kellicottia.D <- (z19com.qa$Kellicottia/ z19com.qa$water_volume)
z19com.qa$Keratella.D <- (z19com.qa$Keratella/ z19com.qa$water_volume)
z19com.qa$Notholca.D <- (z19com.qa$Notholca/ z19com.qa$water_volume)
z19com.qa$Ostracoda.D <- (z19com.qa$Ostracoda/ z19com.qa$water_volume)
z19com.qa$Chaoborus.D <- (z19com.qa$Chaoborus/ z19com.qa$water_volume)
z19com.qa$Chronomid.D <- (z19com.qa$Chronomid/ z19com.qa$water_volume)
z19com.qa$Odonata.D <- (z19com.qa$Odonata/ z19com.qa$water_volume)
z19com.qa$Mite.D <- (z19com.qa$Mite/ z19com.qa$water_volume)
z19com.qa$Nematoda.D <- (z19com.qa$Nematoda/ z19com.qa$water_volume)
z19com.qa$Unknown.D <- (z19com.qa$Unknown/ z19com.qa$water_volume)

summary(z19com.qa)

z19com.qa1.c <- z19com.qa %>%
  select(local_site, location, date1, water_volume, pull_number, 
         total, D.rosea, D.rosea_neonate, D.pulicaria, D.pulicaria_neonate, 
         H.gibberum, H.gibberum_neonate, H.shoshone, D.thomasi,
         naupli, Bosmina, Chydoridae, Ascomorpha, Asplanchna,
         Conochiloides, Kellicottia, Keratella, Notholca,
         Ostracoda, Chaoborus, Chronomid, Odonata, Mite, Nematoda,
         Unknown)

z19com.qa1.d <- z19com.qa %>%
  select(local_site, location, date1, water_volume, pull_number,
         total.D, D.rosea.D, D.rosea_neonate.D, D.pulicaria.D, D.pulicaria_neonate.D, 
         H.gibberum.D, H.gibberum_neonate.D, H.shoshone.D, D.thomasi.D,
         naupli.D, Bosmina.D, Chydoridae.D, Ascomorpha.D, Asplanchna.D,
         Conochiloides.D, Kellicottia.D, Keratella.D, Notholca.D,
         Ostracoda.D, Chaoborus.D, Chronomid.D, Odonata.D, Mite.D, Nematoda.D,
         Unknown.D)

colnames(z19com.qa1.d)[6] = "total"
?colnames()



write.csv(data_longD, "z19com.qa1.d.csv")

z19com.qa2 <- read.csv(file.choose())
names(z19com.qa2)

data_long <- gather(z19com.qa1.c, taxon, count, total:Unknown, factor_key=TRUE)
data_long

data_longD <- gather(z19com.qa1.d, taxon, density, total:Unknown.D, factor_key=TRUE)
data_longD

z19com.qa2$date1 <- as.Date.character(z19com.qa2$date, format="%m/%d/%y")
range(z19com.qa2$date1)
summary(z19com.qa2$date1)


d.allL <- left_join(data_long, z19com.qa2[c("local_site","location", "date1", "density")],
                by = c("local_site" = "local_site","date1" = "date1", "location"= "location","taxon"="taxon"))
summary(d7)


write.csv(data_long, "z19com.qa.long.csv")

hist(data_long$density)
summary(data_long$water_volume)



# RMNP zoop 2015
#
zRMNP15 <- read.csv(file.choose())
names(zRMNP15)

zRMNP15$date <- as.Date.character(zRMNP15$Collection.Date, format="%m/%d/%y")
range(zRMNP15$date)

summary(zRMNP15)

zRMNP15.qa <- zRMNP15 %>% 
  group_by(Site, date, Vertical.Horizontal) %>% 
  summarise("water_volume" = sum(volume.examined, na.rm = T),
            "pull_number" = sum(pull.dummy, na.rm = T),
            "total"= sum(Total.Specimen.number, na.rm = T),
            "D.parvula"= sum(Daphnia.parvula, na.rm = T),
            "D.parvula_neonate"= sum(Daphnia.parvula.Neonate, na.rm = T),
            "D.pulicaria"= sum(Daphnia.pulex, na.rm = T),
            "D.pulicaria_neonate"= sum(Daphnia.pulex.Neonate, na.rm = T),
            "H.gibberum"= sum(Holopediu.gibberum, na.rm = T),
            "H.gibberum_neonate"= sum(Holopediu.gibberum.Neonate, na.rm = T),
            "D.branchyurum"= sum(Diaphanosoma.branchyurum, na.rm = T),
            "H.shoshone"= sum(Hesperodiaptomus.shoshone, na.rm = T),
            "D.thomasi"= sum(Diacyclops.thomasi, na.rm = T),
            "D.bicuspidatus.odessanus"= sum(Diacyclops.bicuspidatus.odessanus, na.rm = T),
            "M.rubellus"= sum(Microcyclops.rubellus, na.rm = T),
            "calanoid.naupilus"= sum(calanoid.naupilus, na.rm = T),
            "cyclopoid.naupilus"= sum(cyclopoid.naupilus, na.rm = T),
            "Bosmina"= sum(Bosmina, na.rm = T),
            "Trichotria.spp"= sum(Trichotria.spp, na.rm = T),
            "Kellicottia"= sum(Kellicottia.sp., na.rm = T),
            "Keratella"= sum(Keratella.cf..Cochlearis, na.rm = T),
            "Ostracoda"= sum(Ostracoda.spp., na.rm = T),
            "Chaoborus"= sum(Chaoborus, na.rm = T),
            "Chironomidiae"= sum(Chironomidiae, na.rm = T),
            "Culicidae"= sum(Mosquito.larva, na.rm = T),
            "Odonata"= sum(Dragonfly.larvae, na.rm = T),
            "Plecoptera"= sum(Stonefly.nymph, na.rm = T),
            "Ephemeroptera"= sum(Mayfly.larvae, na.rm = T),
            "Cercaria"= sum(Ophthalmoxiphidio.cercaria, na.rm = T))
names(zRMNP15.qa)
summary(zRMNP15.qa)




zRMNP15.qa$total.D <- (zRMNP15.qa$total/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.parvula.D <- (zRMNP15.qa$D.parvula/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.parvula_neonate.D <- (zRMNP15.qa$D.parvula_neonate/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.pulicaria.D <- (zRMNP15.qa$D.pulicaria/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.pulicaria_neonate.D <- (zRMNP15.qa$D.pulicaria_neonate/ zRMNP15.qa$water_volume)
zRMNP15.qa$H.gibberum.D <- (zRMNP15.qa$H.gibberum/ zRMNP15.qa$water_volume)
zRMNP15.qa$H.gibberum_neonate.D <- (zRMNP15.qa$H.gibberum_neonate/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.branchyurum.D <- (zRMNP15.qa$D.branchyurum/ zRMNP15.qa$water_volume)
zRMNP15.qa$H.shoshone.D <- (zRMNP15.qa$H.shoshone/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.thomasi.D <- (zRMNP15.qa$D.thomasi/ zRMNP15.qa$water_volume)
zRMNP15.qa$D.bicuspidatus.odessanus.D <- (zRMNP15.qa$D.bicuspidatus.odessanus/ zRMNP15.qa$water_volume)
zRMNP15.qa$M.rubellus.D <- (zRMNP15.qa$M.rubellus/ zRMNP15.qa$water_volume)
zRMNP15.qa$calanoid.naupilus.D <- (zRMNP15.qa$calanoid.naupilus/ zRMNP15.qa$water_volume)
zRMNP15.qa$cyclopoid.naupilus.D <- (zRMNP15.qa$cyclopoid.naupilus/ zRMNP15.qa$water_volume)
zRMNP15.qa$Bosmina.D <- (zRMNP15.qa$Bosmina/ zRMNP15.qa$water_volume)
zRMNP15.qa$Trichotria.spp.D <- (zRMNP15.qa$Trichotria.spp/ zRMNP15.qa$water_volume)
zRMNP15.qa$Ascomorpha.D <- (zRMNP15.qa$Ascomorpha/ zRMNP15.qa$water_volume)
zRMNP15.qa$Asplanchna.D <- (zRMNP15.qa$Asplanchna/ zRMNP15.qa$water_volume)
zRMNP15.qa$Kellicottia.D <- (zRMNP15.qa$Kellicottia/ zRMNP15.qa$water_volume)
zRMNP15.qa$Keratella.D <- (zRMNP15.qa$Keratella/ zRMNP15.qa$water_volume)
zRMNP15.qa$Ostracoda.D <- (zRMNP15.qa$Ostracoda/ zRMNP15.qa$water_volume)
zRMNP15.qa$Chaoborus.D <- (zRMNP15.qa$Chaoborus/ zRMNP15.qa$water_volume)
zRMNP15.qa$Chironomidiae.D <- (zRMNP15.qa$Chironomidiae/ zRMNP15.qa$water_volume)
zRMNP15.qa$Odonata.D <- (zRMNP15.qa$Odonata/ zRMNP15.qa$water_volume)
zRMNP15.qa$Culicidae.D <- (zRMNP15.qa$Culicidae/ zRMNP15.qa$water_volume)
zRMNP15.qa$Plecoptera.D <- (zRMNP15.qa$Plecoptera/ zRMNP15.qa$water_volume)
zRMNP15.qa$Ephemeroptera.D <- (zRMNP15.qa$Ephemeroptera/ zRMNP15.qa$water_volume)
zRMNP15.qa$Cercaria.D <- (zRMNP15.qa$Cercaria/ zRMNP15.qa$water_volume)

#write.csv(zRMNP15.qa, "zRMNP15.qa.csv")









