### Installation des librairies utiles

library(readr)
library(dplyr)

### Lecture des tables

finess_23 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/finess.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE, skip = 1)

finess_22 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20221231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_21 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20211231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_20 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20201231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_19 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20191231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_18 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20181231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_17 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20171231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_16 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20161231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

finess_15 <- read_delim("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/archives/etalab_stock_et_20151231.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Fusion des tables indentiques (toutes sauf 23)

finess <- do.call("rbind",list(finess_22,finess_21,finess_20,finess_19,finess_18,finess_17,finess_16,finess_15))

#On retire les doublons (c'est la donnée la plus récente qui est gardée)

finess <- finess%>% distinct(nofinesset, .keep_all = TRUE)

## Table 2023

finess_23 <- finess_23[,-1] #On retire la première colonne inutile

#On renomme et on selectionne les colonnes

names(finess_23) <- c("FINESS_ET", "FINESS_EJ","rs", "rslongue", "complrs", 
                   "compldistrib", "numvoie", "typvoie", "voie", "compvoie", "lieuditbp", "commune",
                   "departement", "libdepartement", "ligneacheminement", "telephone", "telecopie", 
                   "categetab", "libcategetab", "categagretab", "libcategagretab", "siret", "codeape", 
                   "codemft", "libmft", "codesph", "libsph", "dateouv", "dateautor", "datemaj", "numuai")

finess_23_cat <- select(finess_23, c("FINESS_ET", "rs", "categetab", "libcategetab", "categagretab", "libcategagretab",
                               "codemft", "libmft", "codesph", "libsph"))

finess_cat <- select(finess, c("nofinesset","rs","categetab","libcategetab", "categretab",
                               "libcategretab", "mft", "libmft", "sph", "libsph"))

names(finess_cat) <- names(finess_23_cat)

#FUsion des bases de données

finess <- rbind(finess_23_cat, finess_cat)

#Elimination des doublons

finess <- finess%>% distinct(FINESS_ET, .keep_all = TRUE)

write.csv(finess, "C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/finess/finess_cat.csv", row.names=FALSE)
