### Installation des librairies utiles

library(readr)
library(dplyr)

### Lecture des tables

CCAM <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/brut_ccam/agreg_an/CCAM_AGREG.csv")


finess <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/finess_cat.csv")

###############################################################################################################


### Modification des noms des colonnes

names(CCAM)[1] <- "FINESS_EJ"
names(CCAM)[2] <- "FINESS_ET"

## Pour l'année 2016, il manque le "0" dans le numéro du département dans les indentifiants finess

CCAM$FINESS_ET <- ifelse(nchar(CCAM$dep)==1, paste("0",CCAM$FINESS_ET,sep=""), CCAM$FINESS_ET)
CCAM$FINESS_EJ <- ifelse(nchar(CCAM$dep)==1, paste("0",CCAM$FINESS_EJ,sep=""), CCAM$FINESS_EJ)
CCAM$dep <- ifelse(nchar(CCAM$dep)==1, paste("0",CCAM$dep,sep=""), CCAM$dep)

## Merge

CCAM_finess <- merge(CCAM, finess, by = "FINESS_ET")

####################################################################################################################


### On repère la catégorie de l'établissement via le SPH

#Classement des établissements (1 : CHU , 2 : Public , 3 : Privé lucratif , 4 : Privé non lucratif)
#On va repertorier les CHU via un autre moyen

CCAM_finess["code_cat"] <- NA
CCAM_finess$code_cat[CCAM_finess$codesph == "6" |CCAM_finess$codesph == "7"] <- "4"
CCAM_finess$code_cat[CCAM_finess$codesph == "0" |CCAM_finess$codesph == "3"] <- "3"
CCAM_finess$code_cat[CCAM_finess$codesph == "1"] <- "2"
CCAM_finess$code_cat[CCAM_finess$codesph == "9" |is.na(CCAM_finess$codesph)] <- "9"

# Liste des établissements ayant un sph indeterminé

liste_inde <- unique(CCAM_finess$FINESS_ET[CCAM_finess$code_cat == "9"])

#Recherche manuel de la cétagorie

liste_cat_inde <- c("3","4","4","4",
                    "4","3","4","4",
                    "9","3","3","4",
                    "4","3","3","4",
                    "4","4","4","3",
                    "9","4","4","3",
                    "4","4","4","4",
                    "4","4","4","3",
                    "3","3","3","4","3")

for (i in (1:length(liste_inde))){
  num_finess <- liste_inde[i]
  cat <- liste_cat_inde[i]
  CCAM_finess$code_cat[CCAM_finess$FINESS_ET == num_finess] <- cat
}

#Les etablissements avec un 9 dans code_cat sont mixtes privé-public et pas assez nombreux pour une catégorie à part entière
#On les retire donc

CCAM_finess <- CCAM_finess[CCAM_finess$code_cat != "9",]

#Tous les CHU sonrt des CHR (catégorie 101) sauf pour Orlean et Metz

CCAM_finess$code_cat[CCAM_finess$categetab == "101"] <- "1"

#On remet la catégorie 2 pour ces 2 CHR : FINESS EJ 450000088 et 570005165 (Orleans et Metz)

CCAM_finess$code_cat[CCAM_finess$FINESS_EJ == "450000088" |CCAM_finess$FINESS_EJ == "570005165"] <- "2"

#On met un libellé

CCAM_finess["cat_libelle"] <- NA
CCAM_finess$cat_libelle[CCAM_finess$code_cat == "1"] <- "CHU"
CCAM_finess$cat_libelle[CCAM_finess$code_cat == "2"] <- "Public"
CCAM_finess$cat_libelle[CCAM_finess$code_cat == "3"] <- "Privé lucratif"
CCAM_finess$cat_libelle[CCAM_finess$code_cat == "4"] <- "Privé non lucratif"

################################################################################################################


###Actes CCAM

##On commence par retrouver les tarifications et les catégories grâce à la base de la CPAM

tarification <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/tarification_CCAM.csv")

names(tarification)[names(tarification)=="code_CCAM"] <- "acte"

CCAM_final <- inner_join(CCAM_finess, tarification, by = "acte")


Liste_actes <- setdiff(CCAM_finess$acte,CCAM_final$acte)
Liste_actes

###Pertes

(length(CCAM_finess$acte)-length(CCAM_final$acte))/length(CCAM_finess$acte)*100
#Seulement 0.57% de pertes

####################################################################

#Tables en .csv

write.csv(CCAM_final,"C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_final.csv", row.names=FALSE)

###############################################################################################################


###Ajout des indicateurs de capacité

capacite <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/capacites_etab/capacite_SAE.csv")

#Création de l'ID dans la table CCAM (FINESS + année)
CCAM_final$ID <- paste(CCAM_final$FINESS_ET, as.character(CCAM_final$annee),sep="_")

CCAM_capacite <- inner_join(CCAM_final, capacite, by = "ID")

###Pertes

(length(CCAM_final$acte)-length(CCAM_capacite$acte))/length(CCAM_final$acte)*100
#Seulement 13.89% de pertes, due à l'absence de l'année 2022

###Comparaison sans l'année 22

(length(CCAM_final$acte[CCAM_final$annee < 2022])-length(CCAM_capacite$acte[CCAM_capacite$annee < 2022]))/length(CCAM_final$acte[CCAM_final$annee < 2022])*100
#On passe à 0.12% de perte sansl'année 2022

#Tables en .csv

write.csv(CCAM_capacite,"C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_capacite.csv", row.names=FALSE)

CCAM <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_capacite.csv")

CCAM$finess_ej_annee <- CCAM$finess_ej_annee <- paste(CCAM$FINESS_EJ, CCAM$annee, sep="_")

library(readxl)

hospidiag_15 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_15.xlsx")
hospidiag_16 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_16.xlsx")
hospidiag_17 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_17.xlsx")
hospidiag_18 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_18.xlsx")
hospidiag_19 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_19.xlsx")
hospidiag_20 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_20.xlsx")
hospidiag_21 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/hospidiag/hospidiag_21.xlsx")

hospidiag_15$finess_ej_annee <- paste(hospidiag_15$finess, "2015", sep = "_")
hospidiag_16$finess_ej_annee <- paste(hospidiag_16$finess, "2016", sep = "_")
hospidiag_17$finess_ej_annee <- paste(hospidiag_17$finess, "2017", sep = "_")
hospidiag_18$finess_ej_annee <- paste(hospidiag_18$finess, "2018", sep = "_")
hospidiag_19$finess_ej_annee <- paste(hospidiag_19$finess, "2019", sep = "_")
hospidiag_20$finess_ej_annee <- paste(hospidiag_20$finess, "2020", sep = "_")
hospidiag_21$finess_ej_annee <- paste(hospidiag_21$finess, "2021", sep = "_")


hospidiag_15 <- hospidiag_15[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_16 <- hospidiag_16[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_17 <- hospidiag_17[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_18 <- hospidiag_18[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_19 <- hospidiag_19[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_20 <- hospidiag_20[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]
hospidiag_21 <- hospidiag_21[,c("finess_ej_annee", "taille_MCO", "taille_M", "taille_C", "taille_O","A7","A8","A9","A10","A11","A12","A13","A14","A15")]

hospidiag <- do.call("rbind",list(hospidiag_15,hospidiag_16,hospidiag_17,hospidiag_18,hospidiag_19,hospidiag_20,hospidiag_21))


CCAM_hospi <- inner_join(CCAM, hospidiag, by = "finess_ej_annee")

(length(CCAM$FINESS_ET)-length(CCAM_hospi$FINESS_ET))/ length(CCAM$FINESS_ET)*100
#Seulement 3 % de pertes

write.csv(CCAM_hospi,"C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_hospi.csv", row.names=FALSE)
