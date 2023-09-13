###Chargement des librairies utiles

library(readxl)
library(dplyr)
library(tidyr)

##Lecture des tables

actes_22 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2022_actes-techniques-ccam_serie-annuelle.xlsx")

actes_21 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2021_actes-techniques-ccam_serie-annuelle.xlsx")

actes_20 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2020_actes-techniques-ccam_serie-annuelle.xlsx")

actes_19 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2019_actes-techniques-ccam_serie-annuelle.xls")

actes_18 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2018_actes-techniques-ccam_serie-annuelle.xls")

actes_17 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2017_actes-techniques-ccam_serie-annuelle.xls")

actes_16 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2016_actes-techniques-ccam_serie-annuelle.xls")

actes_15 <- read_excel("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/2015_actes-techniques-ccam_serie-annuelle.xls")


# Renommer les colonnes des tables 15, 16 et 17

names(actes_15) <- names(actes_22)
names(actes_16) <- names(actes_22)
names(actes_17) <- names(actes_22)

##Fusion

tarification <- do.call("rbind", list(actes_22, actes_21, actes_20, actes_19, actes_18, actes_17, actes_16, actes_15))

#On retire les doublons (c'est la donnée la plus récente qui est gardée)

tarification <- tarification%>% distinct(`Code acte/activité/phase`, .keep_all = TRUE)


write.csv(tarification, "C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/tarification.csv", row.names=FALSE)


###On crée une autre base de données qui pourra être marge avec la base Open CCAM

names(tarification) <- c("code_acte", "code_acte_acti_phase", "libelle", "code_activite", "code_phase",
                              "code_regroupement", "tarif_sect1", "tarif_sect2", "quantite_actes", 
                              "base_remboursement", "montants_rembourses", "categorie_acte", "sous_categorie_acte")


## Sur Open CCAM les actes sont codés code acte + code phase

tarification["code_CCAM"] <- paste(tarification$code_acte,tarification$code_phase,sep="")

##Creation seconde base avec un code_CCAM unique

tarification_CCAM <- select(tarification,c("code_CCAM","code_acte","libelle","code_phase",
                                           "categorie_acte","sous_categorie_acte"))

tarification_CCAM <- tarification_CCAM %>% distinct()

## On retire les derniers doublons 

#doublons en code_CCAM et NA value dans libelle

tarification_CCAM <- subset(tarification_CCAM, !(duplicated(tarification_CCAM$code_CCAM) & is.na(tarification_CCAM$libelle)))

#Doublons à cause d'un mélange entre catégorie et sous catégorie de l'acte

tarification_CCAM <- subset(tarification_CCAM, !(duplicated(tarification_CCAM$code_CCAM) & substr(tarification_CCAM$categorie_acte,1,5)!="Actes"))

##On ajoute désormais les autres colonnes 

#code regroupement sous forme d'indicatrices

Liste_code_regroupement <- unique(tarification$code_regroupement)
Liste_code_regroupement <- Liste_code_regroupement[!is.na(Liste_code_regroupement)]

for(x in Liste_code_regroupement){
  #Code CCAM des actes faisant parti du regroupement
  Liste_actes_regroupement <- unique(tarification$code_CCAM[tarification$code_regroupement == x])
  #Creation indicatrices
  tarification_CCAM[paste("regroupement",x,sep="_")] <- ifelse(tarification_CCAM$code_CCAM %in% Liste_actes_regroupement,1,0)
}

#code activité sous forme d'indicatrices

Liste_code_activite <- unique(tarification$code_activite)
Liste_code_activite <- Liste_code_activite[Liste_code_activite != "9" & !is.na(Liste_code_activite)]

for (x in Liste_code_activite){
  #Code CCAM des actes faisant parti de l'activite
  Liste_actes_activite <- unique (tarification$code_CCAM[tarification$code_activite == x])
  #Creation des indicatrices
  tarification_CCAM[paste("activite",x,sep="_")] <- ifelse(tarification_CCAM$code_CCAM %in% Liste_actes_activite,1,0)
}

#Tarification secteur 1 et 2 de l'acte selon le code activite

for (x in Liste_code_activite){
  #Creation data base temporaire 
  Tempo_tarif <- data.frame(code_CCAM = tarification$code_CCAM[tarification$code_activite == x],
                            tarif_sect1 = tarification$tarif_sect1[tarification$code_activite == x],
                            tarif_sect2 = tarification$tarif_sect2[tarification$code_activite == x])
  names(Tempo_tarif) <- c("code_CCAM",paste("tarif_sect1",x,sep="_"),paste("tarif_sect2",x,sep="_"))
  Tempo_tarif <- Tempo_tarif %>% drop_na(code_CCAM)
  #Merge avec tarification_CCAM
  tarification_CCAM <- left_join(tarification_CCAM, Tempo_tarif, by ="code_CCAM")
}


### On passe la table en .csv

write.csv(tarification_CCAM, "C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/tarification/tarification_CCAM.csv",row.names=FALSE)






