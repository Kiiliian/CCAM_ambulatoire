### Installation des librairies utiles

library(readr)
library(dplyr)

### Tables non agrégées

ccam_22 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_22/Open_ccam_22.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccam_21 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_21/Open_ccam_21.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccam_19 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_19/Open_ccam_19.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccam_18 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_18/Open_ccam_18.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccam_17 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_17/Open_ccam_17.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccam_16 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_16/Open_ccam_16_.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="latin1"))

ccam_15 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/open_ccam_15/Open_ccam_15.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="latin1"))

### Ajout de colonnes indiquant l'année ou vide pour 2016 et 2017.

ccam_22["annee"] <- "2022"
ccam_21["annee"] <- "2021"
ccam_19["annee"] <- "2019"
ccam_18["annee"] <- "2018"
ccam_17["annee"] <- "2017"
ccam_16["annee"] <- "2016"
ccam_15["annee"] <- "2015"

ccam_15["nb_sejsea"] <- NA
ccam_16["nb_sejsea"] <- NA
ccam_17["nb_sejsea"] <- NA
ccam_15["nb_acte_ambu"] <- NA
ccam_16["nb_acte_ambu"] <- NA
ccam_17["nb_acte_ambu"] <- NA

names(ccam_15)[names(ccam_15)=="Finess"] <- "finess"
names(ccam_15)[names(ccam_15)=="Acte CCAM + phase"] <- "acte"

ccam_15 <- select(ccam_15, c("finess","Finess géographique","acte","nb_sejsea","nb_actes","dms",
                             "nb_sej_0_nuit","nb_acte_ambu","dep","reg","annee"))
ccam_16 <- select(ccam_16, c("finess","Finess géographique","acte","nb_sejsea","nb_actes","dms",
                             "nb_sej_0_nuit","nb_acte_ambu","dep","reg","annee"))
ccam_17 <- select(ccam_17, c("finess","FinessGeo","acte","nb_sejsea","nb_actes","dms",
                             "nb_sej_0_nuit","nb_acte_ambu","dep","reg","annee"))

names(ccam_15) <- names(ccam_18)
names(ccam_16) <- names(ccam_18)
names(ccam_17) <- names(ccam_18)

#On peut fusionner les tables selon les lignes

ccam <- do.call("rbind", list(ccam_15,ccam_16, ccam_17, ccam_18, ccam_19, ccam_21, ccam_22))

write.csv(ccam, "C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/agreg_an/CCAM_AGREG.csv", row.names=FALSE)


### Pour les tables agrégrées par région