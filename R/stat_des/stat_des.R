###Installation des librairies

library(readr)
library(xtable) #Conversion des tables en tableau LateX
library(dplyr)
library(haven)
library(raster)
library(sf)
library("cartography")
library(viridis)
library(ggplot2)
library(tidyverse)
library(stringr)
library(miceadds)


#######################################################################################################

###Pour ggplot

no_axis <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_blank(),
                 axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                 axis.text.y=element_blank(), axis.ticks.y=element_blank())


####################################################################################################################
# TRIS
####################################################################################################################

###Fonctions


##Tri à plat
tri_simple <- function(df, colonne_sup, agrega, NAvalues){
  
  if (is.na(agrega)){
    temp <- df[,c(colonne_sup)]
    names(temp) <- c("colonne_sup")
  } else{
    temp <- df[,c(colonne_sup, agrega)]
    names(temp) <- c("colonne_sup", "agrega")
    
    #on sélectionne les lignes pour avoir un agrega par année
    temp <- temp %>% distinct(agrega, .keep_all = TRUE)
  }
    
  if(NAvalues){#On inclue les NA values
    return(t(table(temp$colonne_sup, useNA = "always")))
      
  } else{#On n'inclue pas les NA values
    return(t(table(temp$colonne_sup)))
  }
}


##Tri croisé
tri_croise <- function(df, colonne_sup, colonne_ver, agrega, NAvalues){
  
  if (is.na(agrega)){
    temp <- df[,c(colonne_sup, colonne_ver)]
    names(temp) <- c("colonne_sup", "colonne_ver")
  } else{
    
    temp <- df[,c(colonne_sup, colonne_ver, agrega)]
    names(temp) <- c("colonne_sup", "colonne_ver", "agrega")
    
    #on sélectionne les lignes pour avoir un agrega
    temp <- temp %>% distinct(agrega, .keep_all = TRUE)
  }
  
  if(NAvalues){#On inclue les NA values
    return(t(table(temp$colonne_sup, temp$colonne_ver, useNA = "always")))
    
  } else{#On n'inclue pas les NA values
    return(t(table(temp$colonne_sup, temp$colonne_ver)))
  }
}


##Tri différencié par année
tri_annuel <- function(tri_annee, colonne_sup, colonne_ver, agrega, NAvalues, vertical){
  
  if(tri_annee){#Tri par années
    
    liste_annee <- list("2015","2016","2017","2018","2019")
    
    if(is.na(colonne_ver)){#Tri simple
      
      tri <- data.frame()
      
      for(i in liste_annee){#Pour chaque année
        
        temp <- CCAM[CCAM$annee == i,] 
        tri_annuel <- as.data.frame.matrix(tri_simple(temp, colonne_sup, agrega, NAvalues))
        #Ajout d'une colonne année
        tri_annuel$annee <- i
        
        row.names(tri_annuel) <- NULL
        #Ajout au tableau tri
        tri <- rbind(tri, tri_annuel)
      }
      #On replace les colonnes
      nb_col <- length(names(tri))
      tri <- tri[,c(nb_col,nb_col-1, 1:(nb_col-2))]
      return(tri)
      
    }else{#Tri croisé
      
      tri <- data.frame()
      
      for(i in liste_annee){#Pour chaque année
        
        temp <- CCAM[CCAM$annee == i,] 
        tri_annuel <- as.data.frame.matrix(tri_croise(temp, colonne_sup, colonne_ver, agrega, NAvalues))
        #Ajout d'une colonne année
        tri_annuel$annee <- i
        
        tri_annuel[colonne_ver] <- row.names(tri_annuel)
        row.names(tri_annuel) <- NULL
        #Ajout au tableau tri
        tri <- rbind(tri, tri_annuel)
      }
      #On replace les colonnes
      nb_col <- length(names(tri))
      tri <- tri[,c(nb_col,nb_col-1, 1:(nb_col-2))]
      #On effectue un tri
      tri <- tri[order(tri[,colonne_ver]),]
      return(tri)
      
    }
    
  } else{#Tri sur toutes les années
    
    if(is.na(colonne_ver)){#Tri simple
      
      if (vertical){
        tri <- t(as.data.frame.matrix(tri_simple(CCAM, colonne_sup, agrega, NAvalues)))
      } else{
        tri <- as.data.frame.matrix(tri_simple(CCAM, colonne_sup, agrega, NAvalues))
      }
      return(tri)
      
    } else{#Tri croisé
      
      tri <- as.data.frame.matrix(tri_croise(CCAM, colonne_sup, colonne_ver, agrega, NAvalues))
      return(tri)
    }
  }  
}


##Passage en table lateX
tri_table <- function(tri_annee, colonne_sup, colonne_ver, agrega, NAvalues, vertical, titre, plusieurs_tables, test){
  
  tri <- tri_annuel(tri_annee, colonne_sup, colonne_ver, agrega, NAvalues, vertical)
  
  if(test){
    return(tri)
  } else{
    
    if(tri_annee & plusieurs_tables){
      
      liste_annee <- list("2015","2016","2017","2018","2019")
      
      for(i in liste_annee){
        #On ne garde qu'une année
        tri_temp <- tri[tri$annee == i,]
        tri_temp <- subset(tri_temp, select = -annee)
        #On modifie le titre
        titre_temp <- paste(titre,i,sep = " en ")
        
        print(xtable( tri_temp , caption = titre_temp), include.rownames=FALSE, caption.placement = "top")
      }
      
    } else if(tri_annee){
      
      print(xtable( tri , caption = titre), include.rownames=FALSE, caption.placement = "top",scalebox = 0.7)
      
    } else{
      
      print(xtable( tri , caption = titre), caption.placement = "top")
      
    }
  }
}



#######################################################################################################################
# MOYENNES ET MEDIANNES
#######################################################################################################################

### Fonctions

##Table de statistique selon des tris à plat ou croisés
description <- function(df, statistique, colonne, tri_haut, tri_ver){
  
  true_haut <- !is.na(tri_haut)
  true_ver <- !is.na(tri_ver)
  
  if(true_haut & true_ver) {#Cas où on regarde la statistique selon un tri croisé
    
    #Selection des variables utiles
    temp <- df[,c(colonne, tri_haut, tri_ver)]
    names(temp) <- c("colonne","tri_haut","tri_ver")
    
    noms_haut <- sort(unique(temp$tri_haut))
    noms_ver <- sort(unique(temp$tri_ver))
    stats <- data.frame()
    
    for (x in noms_ver){
      
      Liste_stat <- c()
      
      for (y in noms_haut){
        
        stat_temp <- statistique(temp$colonne[temp$tri_haut == y & temp$tri_ver == x])
        Liste_stat <- c(Liste_stat, stat_temp)
        
      }
      #Ajout de la statistique sur tri a plat vertical
      tot_ligne <- statistique(temp$colonne[temp$tri_ver == x])
      Liste_stat <- c(Liste_stat, tot_ligne)
      stats <- rbind(stats,Liste_stat)
    }
    
    #Ajout des statistiques sur tri a plat horizontal
    Liste_stat2 <- c()
    for (y in noms_haut){
      stat_temp <- statistique(temp$colonne[temp$tri_haut == y])
      Liste_stat2 <- c(Liste_stat2, stat_temp)
    }
    #Ajout statistique globale
    Liste_stat2 <- c(Liste_stat2, statistique(temp$colonne))
    
    #Ajout au tableau
    stats <- rbind(stats,Liste_stat2)
    
    row.names(stats) <- c(noms_ver, "Total")
    names(stats) <- c(noms_haut, "Total")
    
    return(stats)
    
    
  }else if(true_haut) {#Cas où on regarde la statistique selon un tri à plat sur la variable horizontale
    
    #Selection des variables utiles
    temp <- df[,c(colonne, tri_haut)]
    names(temp) <- c("colonne","tri")
    
    #Statitisiqu sur chacune des instances
    noms <- sort(unique(temp$tri))
    Liste_stat <- c()
    for(x in noms){
      
      stat_temp <- statistique(temp$colonne[temp$tri == x])
      Liste_stat <- c(Liste_stat, stat_temp)
    }
    #On rajoute la statistique globale
    Liste_stat <- c(Liste_stat, statistique(temp$colonne))
    noms <- c(noms, "Total")
    
    #Creation de la base de données
    stat_haut <- data.frame(t(data.frame(Liste_stat)))
    names(stat_haut) <- noms
    row.names(stat_haut) <- "Total"
    
    return (stat_haut)
    
    
  }else if(true_ver) {#Cas où on regarde la statistique selon un tri à plat sur la variable verticale
    
    #Selection des variables utiles
    temp <- df[,c(colonne, tri_ver)]
    names(temp) <- c("colonne","tri")
    
    #Statitisiqu sur chacune des instances
    noms <- sort(unique(temp$tri))
    Liste_stat <- c()
    for(x in noms){
      
      stat_temp <- statistique(temp$colonne[temp$tri == x])
      Liste_stat <- c(Liste_stat, stat_temp)
    }
    #On rajoute la statistique globale
    Liste_stat <- c(Liste_stat, statistique(temp$colonne))
    noms <- c(noms, "Total")
    
    #Creation de la base de données
    stat_haut <- (data.frame(Liste_stat))
    row.names(stat_haut) <- noms
    names(stat_haut) <- "Total"
    
    return (stat_haut)
    
    
  }else {#Cas où on regarde la statistique globale
    
    #selection de la variable utile
    temp <- df[,colonne]
    names(temp) <- "colonne"

    stat_glo <- data.frame(statistique(temp$colonne))
    names(stat_glo) <- "Total"
    row.names(stat_glo) <- "Total"
    return(stat_glo)
    
  }
  
}

##Passage de la table en LateX
description_brut <- function(statistique, colonne, tri_haut, tri_ver, var_selection, selection, titre, test){
  
  temp <- CCAM
  if(!(is.na(var_selection))){
    names(temp)[names(temp)==var_selection] <- "var_selection"
    temp <- temp[temp$var_selection == selection,]
  }
  stats <- description(temp, statistique, colonne, tri_haut, tri_ver)
  
  if(test){
    
    return(stats)
    
  } else {
    
    print(xtable( stats , caption = titre), caption.placement = "top")
    
  }
  
}


##Description avec agrégation
description_agrega <- function(statistique, colonne, tri_haut, tri_ver, var_selection, selection, agrega, fun, titre, test){
  
  #On sélectionne les lignes
  temp <- CCAM
  if(!(is.na(var_selection))){
    names(temp)[names(temp)==var_selection] <- "var_selection"
    temp <- temp[temp$var_selection == selection,]
  }
  
  names(temp)[names(temp)==colonne] <- "colonne"
  names(temp)[names(temp)==agrega] <- "agrega"
  
  #On aggrège
  temp_agreg <- aggregate(temp$colonne, by=list(temp$agrega), FUN= fun)
  names(temp_agreg) <- c("agrega","colonne")
  
  if(!is.na(tri_haut)){
    names(temp)[names(temp)==tri_haut] <- "tri_haut"
    temp_haut <- temp %>% distinct(agrega, tri_haut, .keep_all = FALSE)
    temp_agreg <- inner_join(temp_agreg, temp_haut, by = "agrega")
    
  }
  if(!is.na(tri_ver)){
    names(temp)[names(temp)==tri_ver] <- "tri_ver"
    temp_ver <- temp %>% distinct(agrega, tri_ver, .keep_all = FALSE)
    temp_agreg <- inner_join(temp_agreg, temp_ver, by = "agrega")
    
  }
  
  stats <- description(temp_agreg, statistique, "colonne", "tri_haut", "tri_ver")
  
  
  if(test){
    
    return(stats)
    
  } else {
    
    print(xtable( stats , caption = titre), caption.placement = "top")
    
  }
  
}

##Description avec selection d'un ou plusieurs actes
description_actes <- function(statistique, colonne, tri_haut, tri_ver, var_selection, selection, liste_actes, titre, test){
  
  temp <- CCAM
  if(!(is.na(var_selection))){
    names(temp)[names(temp)==var_selection] <- "var_selection"
    temp <- temp[temp$var_selection == selection,]
  }
  temp <- temp[temp$acte %in% liste_actes,]
  
  stats <- description(temp, statistique, colonne, tri_haut, tri_ver)
  
  if(test){
    
    return(stats)
    
  } else {
    
    print(xtable( stats , caption = titre), caption.placement = "top")
    
  }
  
}

##

maxima <- function(colonne, agrega, rs_libelle, nombre, var_selection, selection, fun, freq, freq_tot, entiers, titre, test){
  
  temp <- CCAM
  if(freq_tot){
    nb_total <- sum(temp[colonne], na.rm = TRUE)
  }
  if(!(is.na(var_selection))){
    names(temp)[names(temp)==var_selection] <- "var_selection"
    temp <- temp[temp$var_selection == selection,]
  }
  
  if(is.na(rs_libelle)){
    temp <- temp[,c(agrega, colonne)]
    names(temp) <- c("agrega", "colonne")
  } else{
    temp <- temp[,c(agrega, colonne, rs_libelle)]
    names(temp) <- c("agrega", "colonne", "rs_libelle")
  }
  
  temp_agreg <- aggregate(temp$colonne, by=list(temp$agrega), FUN=fun)
  names(temp_agreg) <- c("agrega","colonne")
  
  if(!is.na(rs_libelle)){
    temp_libelle <- temp %>% distinct(agrega, rs_libelle,.keep_all = FALSE)
    temp_libelle <- temp_libelle %>% distinct(agrega,.keep_all = TRUE)
    temp_agreg <- inner_join(temp_agreg, temp_libelle, by = "agrega")
  }
  
  #On tri selon les valeurs décroissantes
  temp_agreg <- temp_agreg[order(temp_agreg$colonne, decreasing = TRUE), ]
  
  #Ajout de la fréquence
  if(!freq_tot){
    nb_total <- sum(temp_agreg$colonne, na.rm = TRUE)
  }
  temp_agreg["Fréquence"] <- round(temp_agreg$colonne*100 / nb_total, digits =2)
  temp_agreg$Fréquence <- paste(as.character(temp_agreg$Fréquence), "%", sep = " ")
  
  if(entiers){
    temp_agreg$colonne <- as.character(round(temp_agreg$colonne, digits =0))
  }
  
  #On selectionne le nombre voulu de valeurs
  valeurs <- head(temp_agreg, nombre)
  
  if(test){
    
    return(valeurs)
    
  } else{
    
    if (freq & !is.na(rs_libelle)){
      valeurs <- valeurs[,c("agrega","Fréquence","rs_libelle")]
      print(xtable( valeurs, caption = titre), include.rownames=FALSE, caption.placement = "top")
    } else{
      print(xtable( valeurs, caption = titre), include.rownames=FALSE, caption.placement = "top")
    }
    
  }
}






###Chargement de la table

CCAM_base <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_capacite.csv")

####################################################################################################################

###On garde les années pre-covid, de 2015 à 2019

CCAM <- CCAM_base[CCAM_base$annee < 2020,]

##Conversion des années en nombres entiers

CCAM$annee <- as.character(CCAM$annee)





###
#########################################################################################################################
### APPLICATION
#########################################################################################################################
###

########################################################################################
#Contexte
########################################################################################





###Tri Etablissement par année

tri_table(TRUE, "cat_libelle", NA , "ID",FALSE, FALSE, "Répartition des établissements de santé par année", FALSE, FALSE)

###Tri Entités juridiques par année

#Besoin d'une colonne d'agrégation (Finess_ej + année)

CCAM$finess_ej_annee <- paste(CCAM$FINESS_EJ, CCAM$annee, sep="_")
tri_table(TRUE, "cat_libelle", NA , "finess_ej_annee",FALSE, FALSE,"Répartition des entités juridiques par année", FALSE, FALSE)







###Etablissements par région

#Ajustement table CCAM
CCAM$reg[CCAM$reg=="1"] <- "01"
CCAM$reg[CCAM$reg=="4"] <- "04"

CCAM$reg[CCAM$dep == "2A"] <- "94"
CCAM$reg[CCAM$dep == "2B"] <- "94"


# Récupération de la carte

fr <- getData('GADM', country='FRA', level=1) 

fr <- st_as_sf(fr, "sf") # Conversion au format sf

#Ajout code région

reg <- c("84","27","53","24","94","44","32","11","28","75","76","52","93")
fr <- cbind(fr, reg)

# Comptage des établissements

tri <- as.data.frame(tri_simple(CCAM[CCAM$annee == "2019",], "reg", "FINESS_ET", FALSE))[,-1]
names(tri) <- c("reg","Etablissements")
tri$reg <- as.character(tri$reg)

#Fusion à la table 

fr <- inner_join(fr, tri, by = "reg")

# Affichage

ggplot(data = fr, aes(fill = Etablissements)) +
  geom_sf() +
  scale_fill_gradient(low = "gold", high = "red2") +
  no_axis




###Etablissements par départements

fr <- getData('GADM', country='FRA', level=2) # Récupération de la carte

fr <- st_as_sf(fr, "sf") # Conversion au format sf


#Modification nom code departement

names(fr)[names(fr)== "CC_2"] <- "dep"

# Comptage des établissements

tri <- as.data.frame(tri_simple(CCAM[CCAM$annee == "2016",], "dep", "FINESS_ET", FALSE))[,-1]
names(tri) <- c("dep","Etablissements")
tri$dep <- as.character(tri$dep)

#Fusion à la table 

fr <- inner_join(fr, tri, by = "dep")

# Affichage

ggplot(data = fr, aes(fill = Etablissements)) +
  geom_sf() +
  scale_fill_gradient(low = "gold", high = "red2") +
  no_axis







###Tri par région et département, par année

#Récupération nom de régions, département

fr <- getData('GADM', country='FRA', level=2) # Récupération de la carte

fr <- st_as_sf(fr, "sf")

#Selection

fr <- as.data.frame(fr)[c("NAME_1","NAME_2","CC_2")]
names(fr) <- c("Region","Departement","dep")

CCAM <- left_join(CCAM, fr, by = "dep")

#Les departements et régions manquants sont des DROM regroupés

CCAM$Region[CCAM$reg=="01"] <- "Guadeloupe, Martinique, Guyane"
CCAM$Region[CCAM$reg=="04"] <- "Réunion, Polynésie"

CCAM$Departement[CCAM$dep=="9A"] <- "Guadeloupe"
CCAM$Departement[CCAM$dep=="9B"] <- "Martinique"
CCAM$Departement[CCAM$dep=="9C"] <- "Guyane"
CCAM$Departement[CCAM$dep=="9D"] <- "Réunion"
CCAM$Departement[CCAM$dep=="98"] <- "Polynésie"


tri_table(FALSE, "annee", "Region" , "ID",FALSE, FALSE, "Répartition des établissements de santé par région", FALSE, FALSE)

tri_table(FALSE, "annee", "Departement" , "ID",FALSE, FALSE, "Répartition des établissements de santé par département", FALSE, TRUE)

tri_table(TRUE, "cat_libelle", "Region" , "ID",FALSE, FALSE, "Répartition des établissements de santé par région", FALSE, FALSE)





#######################################################################################



###Actes les plus réalisés
maxima("nb_actes", "acte", "libelle", 20,NA, NA, sum, TRUE, TRUE, TRUE,"Liste des 20 actes les plus réalisés entre 2015 et 2019", FALSE)


###Catégories les plus réalisées
maxima("nb_actes", "categorie_acte", NA, 10,NA, NA, sum, TRUE, FALSE, TRUE, "Liste des 10 premières catégories d'actes les plus réalisés", FALSE)





###Actes les plus réalisés dans les 4 premières catégories (> 10%) et t ux d'ambulatoire de chaque acte
maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Actes techniques médicaux thérapeutiques", sum, TRUE, FALSE, TRUE,"Liste des actes techniques médicaux thérapeutiques les plus réalisés", TRUE)
maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Imagerie Radiographie", sum, TRUE, FALSE, TRUE,"Liste des actes d'imagerie et radiographie les plus réalisés", TRUE)
maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Actes techniques médicaux diagnostiques", sum, TRUE, FALSE, TRUE,"Liste des actes techniques médicaux diagnostiques les plus réalisés", TRUE)
maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Actes chirurgicaux", sum, TRUE, FALSE, TRUE,"Liste des actes chirurgicaux les plus réalisés", TRUE)
#Il faudrait ajouter le taux d'ambulatoire

#Conversion en numérique
CCAM$nb_sej_0_nuit[CCAM$nb_sej_0_nuit == "."] <- NA
CCAM$nb_sej_0_nuit <- as.numeric(CCAM$nb_sej_0_nuit)
CCAM$nb_sej_0_nuit[is.na(CCAM$nb_sej_0_nuit)] <- 0

#Agrégation par acte
CCAM_actes_1 <- aggregate(CCAM$nb_actes, by=list(CCAM$acte), FUN=sum)
names(CCAM_actes_1) <- c("acte", "nb_actes")
CCAM_actes_2 <- aggregate(CCAM$nb_sej_0_nuit , by=list(CCAM$acte), FUN=sum)
names(CCAM_actes_2) <- c("acte", "nb_sej_0_nuit")

#Création d'une colonne avec la part d'actes ambulatoire (pourcentage)
CCAM_actes <- merge(CCAM_actes_1, CCAM_actes_2, by = "acte")
CCAM_actes$part_ambu <- round(CCAM_actes$nb_sej_0_nuit / CCAM_actes$nb_actes * 100, digits = 2)
CCAM_actes$part_ambu_character <- paste(as.character(CCAM_actes$part_ambu), "%", sep =" ")

CCAM_part_ambu_character <- CCAM_actes[c("acte","part_ambu_character")]

#Création et impression des tableaux avec la part d'ambulatoire en plus
maxima_ambu <- maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Actes techniques médicaux thérapeutiques", sum, TRUE, FALSE, TRUE,"Liste des actes techniques médicaux thérapeutiques les plus réalisés", TRUE)
names(maxima_ambu)[names(maxima_ambu)== "agrega"] <- "acte"
maxima_ambu <- inner_join(maxima_ambu, CCAM_part_ambu_character, by = "acte")
maxima_ambu <- maxima_ambu[c("acte","Fréquence","part_ambu_character","rs_libelle")]
names(maxima_ambu) <- c("Code CCAM","Fréquence","Ambulatoire","Libellé abrégé de l'acte")
print(xtable(maxima_ambu,caption = "Liste des actes techniques médicaux thérapeutiques les plus réalisés"), include.rownames=FALSE, caption.placement = "top")

maxima_ambu <- maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Actes techniques médicaux diagnostiques", sum, TRUE, FALSE, TRUE,"Liste des actes techniques médicaux diagnostiques les plus réalisés", TRUE)
names(maxima_ambu)[names(maxima_ambu)== "agrega"] <- "acte"
maxima_ambu <- inner_join(maxima_ambu, CCAM_part_ambu_character, by = "acte")
maxima_ambu <- maxima_ambu[c("acte","Fréquence","part_ambu_character","rs_libelle")]
names(maxima_ambu) <- c("Code CCAM","Fréquence","Ambulatoire","Libellé abrégé de l'acte")
print(xtable(maxima_ambu,caption = "Liste des actes techniques médicaux diagnostiques les plus réalisés"), include.rownames=FALSE, caption.placement = "top")

maxima_ambu <- maxima("nb_actes", "acte", "libelle", 15, "categorie_acte", "Imagerie Radiographie", sum, TRUE, FALSE, TRUE,"Liste des actes d'imagerie et radiographie les plus réalisés", TRUE)
names(maxima_ambu)[names(maxima_ambu)== "agrega"] <- "acte"
maxima_ambu <- inner_join(maxima_ambu, CCAM_part_ambu_character, by = "acte")
maxima_ambu <- maxima_ambu[c("acte","Fréquence","part_ambu_character","rs_libelle")]
names(maxima_ambu) <- c("Code CCAM","Fréquence","Ambulatoire","Libellé abrégé de l'acte")
print(xtable(maxima_ambu,caption = "Liste des actes d'imagerie et radiographie les plus réalisés"), include.rownames=FALSE, caption.placement = "top")

maxima_ambu <- maxima("nb_actes", "acte", "libelle", 20, "categorie_acte", "Actes chirurgicaux", sum, TRUE, FALSE, TRUE,"Liste des actes chirurgicaux les plus réalisés", TRUE)
names(maxima_ambu)[names(maxima_ambu)== "agrega"] <- "acte"
maxima_ambu <- inner_join(maxima_ambu, CCAM_part_ambu_character, by = "acte")
maxima_ambu <- maxima_ambu[c("acte","Fréquence","part_ambu_character","rs_libelle")]
names(maxima_ambu) <- c("Code CCAM","Fréquence","Ambulatoire","Libellé abrégé de l'acte")
print(xtable(maxima_ambu,caption = "Liste des actes chirurgicaux les plus réalisés"), include.rownames=FALSE, caption.placement = "top")


###Description du nombre d'actes en ambulatoire

repartition <- t(data.frame(c(summary(CCAM_actes$part_ambu))))
print(xtable(repartition, caption = "Répartition du taux d'ambulatoire parmi les actes"), include.rownames=FALSE, caption.placement = "top")
hist(CCAM_actes$part_ambu, breaks =100, main =NULL, xlab ="Part d'actes en ambulatoires", ylab = "Fréquence")

#Part d'actes aec un taux nul
prop.table(table(CCAM_actes$part_ambu == 0))

#En retirant les actes avec une part nulle
CCAM_actes_non_nul <- CCAM_actes[CCAM_actes$part_ambu != 0,]

repartition <- t(data.frame(c(summary(CCAM_actes_non_nul$part_ambu))))
print(xtable(repartition, caption = "Répartition du taux d'ambulatoire (non nul) parmi les actes"), include.rownames=FALSE, caption.placement = "top")
hist(CCAM_actes_non_nul$part_ambu, breaks =100, main =NULL, xlab ="Part d'actes en ambulatoires", ylab = "Fréquence")



#On regarde le nombre d'actes

repartition <- t(data.frame(c(summary(CCAM_actes_non_nul$nb_actes))))
print(xtable(repartition, caption = "Descriptif du nombre d'actes(Actes réalisables en ambulatoires)"), include.rownames=FALSE, caption.placement = "top")

#quantiles
quantile(CCAM_actes_non_nul$nb_actes, probs = .9)
quantile(CCAM_actes_non_nul$nb_actes, probs = .99)


#Passage au log
CCAM_actes_non_nul$nb_actes_scale <- log(CCAM_actes_non_nul$nb_actes)

repartition <- t(data.frame(c(summary(CCAM_actes_non_nul$nb_actes_scale))))
print(xtable(repartition, caption = "Descriptif du logarithme du nombre d'actes(Actes réalisables en ambulatoires)"), include.rownames=FALSE, caption.placement = "top")

hist(CCAM_actes_non_nul$nb_actes_scale, breaks = 100, main =NULL, xlab ="Logarithme du nombre d'actes", ylab = "Fréquence")


##Nuage de points bivarié

plot_part_nb <- ggplot(CCAM_actes_non_nul) + aes(x = part_ambu, y = nb_actes_scale) +geom_point(colour = "red", alpha = .3) + 
    labs(x = "Taux d’actes en ambulatoire", y= "Logarithme du nombre d'actes")

plot_part_nb

##Selection intervalles

##Colonne pour selectionner les actes

#création colonne log dans CCAM_actes
CCAM_actes$nb_actes_log <- log(CCAM_actes$nb_actes)


###Selection des actes pour maximiser R carré ajusté

CCAM_actes$selection <- ifelse(CCAM_actes$part_ambu >= 15 & CCAM_actes$part_ambu <= 85 & CCAM_actes$nb_actes_log >= 8.5, 1,0)

#Comptage des ates selectionné
table(CCAM_actes$selection)
prop.table(table(CCAM_actes$selection))

#Jointure à la base CCAM
CCAM <- inner_join(CCAM, CCAM_actes[,c("acte","part_ambu","nb_actes_log","selection")], by ="acte")

#Comptage par catégorie
CCAM_save <- CCAM

CCAM <- CCAM[CCAM$selection ==1,]


CCAM$part <- CCAM$nb_sej_0_nuit / CCAM$nb_actes

summary(lm(part ~ cat_libelle + annee  + cat_libelle*annee, CCAM[CCAM$selection==1,], weights =nb_actes))$adj.r.squared

#PLot
plot_part_nb <- plot_part_nb + geom_segment(aes(x = 15, y = 8.5, xend = 85, yend = 8.5))
plot_part_nb <- plot_part_nb + geom_segment(aes(x = 15, y = 8.5, xend = 15, yend = 17))
plot_part_nb <- plot_part_nb + geom_segment(aes(x = 85, y = 8.5, xend = 85, yend = 17))

plot_part_nb


#Etude par catégorie

tri_distincts <- as.data.frame(tri_table(FALSE, "categorie_acte", NA, "acte", FALSE, TRUE, "Nombre d'actes sélectionnées par catégorie", FALSE, TRUE))
tri_distincts$agrega <- row.names(tri_distincts)
row.names(tri_distincts) <-  NULL

tri_nb <- maxima("nb_actes", "categorie_acte", NA, 10,NA, NA, sum, TRUE, FALSE, TRUE, "Liste des 10 premières catégories d'actes les plus réalisés", TRUE)

categorie_select <- inner_join(tri_nb, tri_distincts, by = "agrega")
print(xtable(categorie_select, caption = "Catégories des actes sélectionnés"), include.rownames=FALSE, caption.placement = "top")

##On retrouve l'ancienne base avec tous les actes

CCAM <- CCAM_save

maxima("nb_actes", "acte", "categorie_acte", 10,"selection", 1, sum, TRUE, TRUE, TRUE,"Liste des 10 actes selectionnés les plus réalisés", FALSE)

#Libelle des actes sans catégorie

sans_cate <- data.frame( acte = c("ZZLP0250","AHQJ0210","YYYY0120"))
sans_cate <- inner_join(sans_cate, distinct(CCAM[,c("acte","libelle")]), by = "acte")
print(xtable(sans_cate),include.rownames=FALSE)

##Selection actes chirurgicaux
CCAM$selection_chiru <- ifelse(CCAM$selection == 1 & CCAM$categorie_acte == "Actes chirurgicaux", 1, 0)

maxima("nb_actes", "acte", NA, 10, "selection_chiru", 1, sum, TRUE, TRUE, TRUE,"Liste 10 actes les plus effectués en chirurgie", FALSE)

##Selection actes techniques médicaux diagnostiques
CCAM$selection_diag <- ifelse(CCAM$selection == 1 & CCAM$categorie_acte == "Actes techniques médicaux diagnostiques", 1, 0)

maxima("nb_actes", "acte", NA, 10, "selection_diag", 1, sum, TRUE, TRUE, TRUE,"Liste 10 actes les plus effectués en médecine diagnostique", FALSE)

##Selection Examen immunologique de prélèvement
CCAM$selection_immuno <- ifelse(CCAM$selection == 1 & CCAM$categorie_acte == "Examen immunologique de prélèvement cellulaire ou tissulaire", 1, 0)

maxima("nb_actes", "acte", NA, 10, "selection_immuno", 1, sum, TRUE, TRUE, TRUE,"Liste 10 actes les plus effectués en examens immunologiques", FALSE)

##Selection Actes techniques médicaux thérapeutiques
CCAM$selection_thera <- ifelse(CCAM$selection == 1 & CCAM$categorie_acte == "Actes techniques médicaux thérapeutiques", 1, 0)

maxima("nb_actes", "acte", NA, 10, "selection_thera", 1, sum, TRUE, TRUE, TRUE,"Liste 10 actes les plus effectués en médecine thérapeutique", FALSE)



###Ajout de la part d'acte dans chaque catégorie
CCAM$acte_cat <- paste(CCAM$acte, as.character(CCAM$code_cat),sep="_")
CCAM_actes_cat <- aggregate(CCAM$nb_actes, by=list(CCAM$acte_cat), FUN=sum)
names(CCAM_actes_cat) <- c("acte_cat","nb_actes_cat")
CCAM_actes_cat$acte <- str_sub(CCAM_actes_cat$acte_cat, end = -3)

CCAM_actes_cat2 <- aggregate(CCAM_actes_cat$nb_actes_cat, by=list(CCAM_actes_cat$acte), FUN=sum)
names(CCAM_actes_cat2) <- c("acte","nb_actes_cat_total")
CCAM_actes_cat <- inner_join(CCAM_actes_cat,CCAM_actes_cat2 , by="acte")

CCAM_actes_cat$part_actes_cat <- CCAM_actes_cat$nb_actes_cat / CCAM_actes_cat$nb_actes_cat_total

CCAM <- inner_join(CCAM, CCAM_actes_cat[,c("acte_cat","part_actes_cat")], by= "acte_cat")

CCAM_1 <- CCAM[CCAM$code_cat == 1,c("acte","part_actes_cat")]
CCAM_1 <- CCAM_1 %>% distinct()
names(CCAM_1) <- c("acte","part_actes_cat_1")
CCAM <- left_join(CCAM, CCAM_1, by ="acte")
CCAM$part_actes_cat_1[is.na(CCAM$part_actes_cat_1)] <- 0

CCAM_1 <- CCAM[CCAM$code_cat == 2,c("acte","part_actes_cat")]
CCAM_1 <- CCAM_1 %>% distinct()
names(CCAM_1) <- c("acte","part_actes_cat_2")
CCAM <- left_join(CCAM, CCAM_1, by ="acte")
CCAM$part_actes_cat_2[is.na(CCAM$part_actes_cat_2)] <- 0

CCAM_1 <- CCAM[CCAM$code_cat == 3,c("acte","part_actes_cat")]
CCAM_1 <- CCAM_1 %>% distinct()
names(CCAM_1) <- c("acte","part_actes_cat_3")
CCAM <- left_join(CCAM, CCAM_1, by ="acte")
CCAM$part_actes_cat_3[is.na(CCAM$part_actes_cat_3)] <- 0

CCAM_1 <- CCAM[CCAM$code_cat == 4,c("acte","part_actes_cat")]
CCAM_1 <- CCAM_1 %>% distinct()
names(CCAM_1) <- c("acte","part_actes_cat_4")
CCAM <- left_join(CCAM, CCAM_1, by ="acte")
CCAM$part_actes_cat_3[is.na(CCAM$part_actes_cat_3)] <- 0


##Enregistrement de la base de données
write.csv(CCAM,"C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_analyses.csv", row.names=FALSE)




###########################################################################################################################"
#ANALYSE GLOBALE
#############################################################################################################################################


##Chargement base de données
CCAM <- read_csv("C:/Users/Kilian/Desktop/CCAM/CCAM_ambulatoire/Data/ccam_analyses.csv")

CCAM$annee <- as.character(CCAM$annee)

#La colonne part_ambu correspond à la part d'ambulatoire d'un acte CCAM sur l'ensemble des établissements

names(CCAM)[names(CCAM)=="part_ambu"] <- "part_ambu_tous_etab"

CCAM$part_ambu <- CCAM$nb_sej_0_nuit / CCAM$nb_actes * 100



##Part d'actes en ambulatoire

#Somme des actes en ambu
table_actes_ambu <- description_agrega(sum, "nb_sej_0_nuit", "cat_libelle", "annee", NA, NA, "ID", sum, "test", TRUE)
#Somme des actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", NA, NA, "ID", sum, "test", TRUE)
#Part d'ambu en divisant le 1er tableau par le deuxième
for (x in names(table_actes_ambu)){
  table_actes_ambu[x] <- round(table_actes_ambu[x]/table_actes[x]*100,2)
}
#Conversion lateX
print(xtable(table_actes_ambu, caption = "Pourcentage d'actes effectués en ambulatoire (%)"), caption.placement = "top")



###Durée moyenne de séjour 

#Conversion en numérique
CCAM$dms_globale <- gsub(",",".", CCAM$dms_globale)
CCAM$dms_globale <- as.numeric(CCAM$dms_globale)

#Colonne pour pondérer la dms par le nombres d'actes
CCAM$actes_dms <- CCAM$dms_globale*CCAM$nb_actes
#Somme des actes*dms
table_actes_dms <- description_agrega(sum, "actes_dms", "cat_libelle", "annee", NA, NA, "ID", sum, "test", TRUE)
#Somme des nombres d'actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", NA, NA, "ID", sum, "test", TRUE)
#On divise le premier tableau par le deuxieme
for (x in names(table_actes_dms)){
  table_actes_dms[x] <- table_actes_dms[x]/table_actes[x]
}
#Conversion en latex
print(xtable(table_actes_dms, caption = "Durée moyenne des séjours en hopitaux"), caption.placement = "top")



###Meme chose sur les actes selectionnés
##Part d'actes en ambulatoire

#Somme des actes en ambu
table_actes_ambu <- description_agrega(sum, "nb_sej_0_nuit", "cat_libelle", "annee", "selection", 1, "ID", sum, "test", TRUE)
#Somme des actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", "selection", 1, "ID", sum, "test", TRUE)
#Part d'ambu en divisant le 1er tableau par le deuxième
for (x in names(table_actes_ambu)){
  table_actes_ambu[x] <- round(table_actes_ambu[x]/table_actes[x]*100,2)
}
#Conversion lateX
print(xtable(table_actes_ambu, caption = "Pourcentage d'actes selectionnés effectués en ambulatoire (%)"), caption.placement = "top")



###Durée moyenne de séjour 

#Somme des actes*dms
table_actes_dms <- description_agrega(sum, "actes_dms", "cat_libelle", "annee", "selection", 1, "ID", sum, "test", TRUE)
#Somme des nombres d'actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", "selection", 1, "ID", sum, "test", TRUE)
#On divise le premier tableau par le deuxieme
for (x in names(table_actes_dms)){
  table_actes_dms[x] <- table_actes_dms[x]/table_actes[x]
}
#Conversion en latex
print(xtable(table_actes_dms, caption = "Durée moyenne des séjours en hopitaux"), caption.placement = "top")


#Seuil de participation pour les catégories

#Fonction
acte_tout_cat_parti <- function(part){
  part1 <- CCAM$part_actes_cat_1 >= part
  part2 <- CCAM$part_actes_cat_2 >= part
  part3 <- CCAM$part_actes_cat_3 >= part
  part4 <- CCAM$part_actes_cat_4 >= part
  return(part1 & part2 & part3 & part4)
}
  
#Participation d'au moins 15%
CCAM$acte_cat_15 <- acte_tout_cat_parti(.15) & (CCAM$selection == TRUE)
CCAM$acte_cat_20 <- acte_tout_cat_parti(.2) & (CCAM$selection == TRUE)


###Durée moyenne de séjour 

#Somme des actes*dms
table_actes_dms <- description_agrega(sum, "actes_dms", "cat_libelle", "annee", "acte_cat_20", 1, "ID", sum, "test", TRUE)
#Somme des nombres d'actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", "acte_cat_20", 1, "ID", sum, "test", TRUE)
#On divise le premier tableau par le deuxieme
for (x in names(table_actes_dms)){
  table_actes_dms[x] <- table_actes_dms[x]/table_actes[x]
}
#Conversion en latex
print(xtable(table_actes_dms, caption = "Durée moyenne des séjours en hopitaux (> 20%)"), caption.placement = "top")


##Part d'actes en ambulatoire

#Somme des actes en ambu
table_actes_ambu <- description_agrega(sum, "nb_sej_0_nuit", "cat_libelle", "annee", "acte_cat_20", 1, "ID", sum, "test", TRUE)
#Somme des actes
table_actes <- description_agrega(sum, "nb_actes", "cat_libelle", "annee", "acte_cat_20", 1, "ID", sum, "test", TRUE)
#Part d'ambu en divisant le 1er tableau par le deuxième
for (x in names(table_actes_ambu)){
  table_actes_ambu[x] <- round(table_actes_ambu[x]/table_actes[x]*100,2)
}
#Conversion lateX
print(xtable(table_actes_ambu, caption = "Pourcentage d'actes selectionnés effectués en ambulatoire (%) (> 20%)"), caption.placement = "top")


###Controle sur l'ensemble des actes sélectionnés

CCAM_select <- CCAM[CCAM$selection == 1,]

#On retrouve les moyennes précédentes
summary(lm(part_ambu ~ cat_libelle + annee + annee*cat_libelle, CCAM_select, weights = nb_actes))
summary(lm(part_ambu ~ cat_libelle, CCAM_select, weights = nb_actes))
summary(lm(part_ambu ~ annee, CCAM_select, weights = nb_actes))

#Controle par la part de
summary(lm(dms_globale ~ cat_libelle + annee + part_actes_cat*cat_libelle, CCAM))
#SAns controle
summary(lm(dms_globale ~ cat_libelle + annee , CCAM))
summary(lm(dms_globale ~ cat_libelle + nb_actes, CCAM))

###########################################################################################

###Vérification aprrox nb_actes = nb_sejsea ou nb_sej_0_nuit = nb_acte_ambu

CCAM_18_19 <- CCAM[CCAM$annee == "2018" | CCAM$annee == "2019",]

prop.table(table(CCAM_18_19$nb_sejsea == CCAM_18_19$nb_actes))
#58% de corresponance entre la colonne nb_actes et nb_sejsea

prop.table(table(CCAM_18_19$nb_sej_0_nuit == CCAM_18_19$nb_acte_ambu))
#89% de corresponance entre la colonne nb_sej_0_nuit et nb_acte_ambu

#Conversion en numerique
CCAM_18_19$nb_sej_0_nuit[CCAM_18_19$nb_sej_0_nuit == "."] <- NA
CCAM_18_19$nb_acte_ambu[CCAM_18_19$nb_acte_ambu == "."] <- NA

CCAM_18_19$nb_sej_0_nuit <- as.numeric(CCAM_18_19$nb_sej_0_nuit)
CCAM_18_19$nb_acte_ambu <- as.numeric(CCAM_18_19$nb_acte_ambu)

prop.table(table(CCAM_18_19$nb_sej_0_nuit == CCAM_18_19$nb_acte_ambu))
#75% de corresponance entre la colonne nb_actes et nb_sejsea sans prendre en compte les NA

#Colonne difference entre nb_actes et nb_sejsea
CCAM_18_19$diff_actes_sej <- CCAM_18_19$nb_actes - CCAM_18_19$nb_sejsea

#Colonne difference entre nb_acte_ambu et nb_sej_0_nuit
CCAM_18_19$diff_actes_sej_ambu <- CCAM_18_19$nb_acte_ambu - CCAM_18_19$nb_sej_0_nuit

#Description des colonne de différences
summary(CCAM_18_19$diff_actes_sej)
summary(CCAM_18_19$diff_actes_sej_ambu)


t.test(CCAM_18_19$nb_acte_ambu, CCAM_18_19$nb_sej_0_nuit, alternative = "two.sided", var.equal = TRUE)

t.test(CCAM_18_19[CCAM_18_19$acte =="HMFC0040",]$nb_acte_ambu, CCAM_18_19[CCAM_18_19$acte =="HMFC0040",]$nb_sej_0_nuit, alternative = "two.sided", var.equal = TRUE)

regression <- lm(nb_acte_ambu ~ nb_sej_0_nuit, data = CCAM_18_19[CCAM_18_19$acte =="HMFC0040",])
summary(regression)

regression <- lm(nb_acte_ambu ~ nb_sej_0_nuit, data = CCAM_18_19[CCAM_18_19$acte =="NEKA0200",])
summary(regression)

regression_annee <- lm(nb_acte_ambu ~ nb_sej_0_nuit + annee, data = CCAM_18_19[CCAM_18_19$acte =="HMFC0040",])
summary(regression_annee)

regression_cat <- lm(nb_acte_ambu ~ nb_sej_0_nuit + cat_libelle , data = CCAM_18_19[CCAM_18_19$acte =="HMFC0040",])
summary(regression_cat)


regression_all <- lm(nb_acte_ambu ~ nb_sej_0_nuit + annee + cat_libelle , data = CCAM_18_19[CCAM_18_19$acte =="HMFC0040",])
summary(regression_all)

regression_pas <- lm(nb_acte_ambu ~ nb_sej_0_nuit + PAS , data = CCAM_18_19[CCAM_18_19$acte =="HMFC0040",])
summary(regression_pas)

# ??????
#On effectue la vérification sur les 20 actes les plus réalisés ?

