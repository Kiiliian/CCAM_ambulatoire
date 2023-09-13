###Chargement des librairies utiles

library(readr)
library(dplyr)

##Lecture des tables

#Capacité de l'établissment

capact15 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact15.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact16 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact16.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact17 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact17.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact18 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact18.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact19 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact19.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact20 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact20.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
capact21 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capact21.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)



#On fusionne les bases capcite

capacite <- do.call("rbind",list(capact15,capact16,capact17,capact18,capact19,capact20))

#On conserve les colonnes utiles

capacite <- select(capacite, c("AN","FI","PLA","LIT","SEJHC","JOU","SEJHP","PAS"))

#L'année 21 a des noms différents

capact21 <- select(capact21, c("an","fi","PLA","LIT","SEJHC","JOU","SEJHP","PAS"))

names(capact21) <- c("AN","FI","PLA","LIT","SEJHC","JOU","SEJHP","PAS")

capacite <- rbind(capacite,capact21)


#Suppression des NA values dans le num finess

capacite <- capacite[!is.na(capacite$FI),]

#Création indice unique (Finess + année)

capacite$ID <- paste(capacite$FI, as.character(capacite$AN), sep="_")

#Dans la table capacite, il y a un tri par discipline, on somme donc sur les doublons

capacite <- select(capacite, c("ID","PLA","LIT","SEJHC","JOU","SEJHP","PAS"))

capacite[is.na(capacite)] <- 0

capacite_sum <- aggregate(capacite[,-1], list(ID=capacite[[1]]), FUN = sum)


write.csv(capacite_sum, "C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capacite_SAE.csv", row.names=FALSE)

################################################################################################################################
################################################################################################################################
#Non utilisé
################################################################################################################################
################################################################################################################################

#On va également appairer les tables hospidiag

library(readxl)

### Lecture des tables

hospidiag_16 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_16.xlsx")
hospidiag_17 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_17.xlsx")
hospidiag_18 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_18.xlsx")
hospidiag_19 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_19.xlsx")
hospidiag_20 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_20.xlsx")
hospidiag_21 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_21.xlsx")

#Il n'y a pas les colonnes CI_DF1, CI_DF2, CI_DF3, CI_DF4 et CI_DF5 pour les années 20 et 21

hospidiag_16 <- select(hospidiag_16, -c("CI_DF1", "CI_DF2", "CI_DF3", "CI_DF4", "CI_DF5"))
hospidiag_17 <- select(hospidiag_17, -c("CI_DF1", "CI_DF2", "CI_DF3", "CI_DF4", "CI_DF5"))
hospidiag_18 <- select(hospidiag_18, -c("CI_DF1", "CI_DF2", "CI_DF3", "CI_DF4", "CI_DF5"))
hospidiag_19 <- select(hospidiag_19, -c("CI_DF1", "CI_DF2", "CI_DF3", "CI_DF4", "CI_DF5"))

hospidiag_16$annee <- 2016
hospidiag_17$annee <- 2017
hospidiag_18$annee <- 2018
hospidiag_19$annee <- 2019
hospidiag_20$annee <- 2020
hospidiag_21$annee <- 2021


#Unique base de données pour toutes les années

hospidiag <- do.call("rbind",list(hospidiag_21,hospidiag_20,hospidiag_19,hospidiag_18,hospidiag_17,hospidiag_16))


###Selection des colonnes contenant les infos sur le nombre de lits, de places et les ETP

hospidiag <- select(hospidiag,c("finess","annee","CI_AC1","CI_AC6","CI_AC8","CI_AC5","CI_AC7",
                                "CI_AC9","CI_RH1"))

#"CI_AC1","CI_AC6" et "CI_AC8" correspondent aux lits en médecine, chirurgie et obstétrie, on les somme

hospidiag$LIT <- as.numeric(hospidiag$CI_AC1) + as.numeric(hospidiag$CI_AC6) + as.numeric(hospidiag$CI_AC8)

# De même pour les places avec "CI_AC5","CI_AC7" et "CI_AC9"

hospidiag$PLA <- as.numeric(hospidiag$CI_AC5) + as.numeric(hospidiag$CI_AC7) + as.numeric(hospidiag$CI_AC9)

#Conversion en numérique de la dernière colonne rh (On commence par remplacer les virgules décimales par des points)

hospidiag$RH <- gsub(",", ".", hospidiag$CI_RH1)

hospidiag$ETPSAL <- as.numeric(hospidiag$RH)

#Selection des colonnes restantes

hospidiag$ID <- paste(hospidiag$finess, as.character(hospidiag$annee), sep="_")

hospidiag <- select(hospidiag, c("ID", "PLA", "LIT", "finess", "annee", "ETPSAL"))
names(hospidiag) <- c("ID", "PLA", "LIT", "FI", "AN", "ETPSAL")

#Pour les équivalents temps plein (ETP)

###Fusion des tables de capacité et hospidiag

Liste <- setdiff(hospidiag$ID,capacite_sum$ID)

#############################################################################################################################

write.csv(capacite_et,"C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capacite_et.csv", row.names = FALSE)
