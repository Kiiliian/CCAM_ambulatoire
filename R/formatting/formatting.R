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

