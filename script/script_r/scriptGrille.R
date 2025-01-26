################################################################################
################################################### EXPLORATIONS CARTOGRAPHIQUES
################################################################################

library(sf)
library(mapsf)
library(tidyverse)
library(data.table)
library(readxl)

############################################################# IMPORT DES DONNEES
surface <- st_read("DATA/GPKG/DEPARTEMENT.gpkg")
grille <- st_read("DATA/GPKG/Grille_1km.gpkg")

#################################################################### TRAITEMENTS
# Calculer la surface des régions
surface$area_surf <- st_area(surface)
# Calculer la surface des carreaux
grille$area_grille <- st_area(grille)

# Découper les régions en fonction des carreaux
surface_decoup <- st_intersection(surface, grille)

# Calculer la surface de chaque morceau de surface découpé
surface_decoup$area_bousurf <- st_area(surface_decoup)
# Part de ce recouvrement
surface_decoup$recouv <- round(surface_decoup$area_bousurf / surface_decoup$area_grille *100, 2)

surf_decoup <- select(surface_decoup, c(id, INSEE_DEP, recouv))

# Convertir le dataframe en un data.table pour réduire le temps de calcul
setDT(surf_decoup)
# Obtenir l'indice de la ligne où recouv est maximal pour chaque groupe d'id
max_recouv <- surf_decoup[, .I[which.max(recouv)], by = id]$V1
# Sélectionne les lignes correspondantes de region_decoup et récupère ID surface
max_carro <- surf_decoup[max_recouv, .(id, INSEE_DEP)]

# Jointure avec les carreaux pour retrouver leur géométrie
carro <- merge(grille, 
               max_carro,
               by.x = "id", 
               by.y = "id", 
               all.x = TRUE)

# Union pour avoir une géométrie par entité
border <- carro %>%
  group_by(INSEE_DEP) %>%
  summarise(geometry = st_union(geometry))
