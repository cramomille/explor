################################################################################
################################# COUCHE GEO DES IRIS AVEC TOUS LES IDENTIFIANTS
################################################################################

library(sf)
library(mapsf)
library(tidyverse)
library(data.table)
library(readxl)

############################################################# IMPORT DES DONNEES
irisGua <- st_read("DATA/GPKG/iris/iris_guadeloupe.gpkg")
irisGuy <- st_read("DATA/GPKG/iris/iris_guyane.gpkg")
irisHex <- st_read("DATA/GPKG/iris/iris_hexagone.gpkg")
irisMar <- st_read("DATA/GPKG/iris/iris_martinique.gpkg")
irisMay <- st_read("DATA/GPKG/iris/iris_mayotte.gpkg")
irisReu <- st_read("DATA/GPKG/iris/iris_reunion.gpkg")

# Reprojection en Lambert 93
irisGuaRepro <- st_transform(irisGua, crs = "EPSG:2154")
irisGuyRepro <- st_transform(irisGuy, crs = "EPSG:2154")
irisHexRepro <- st_transform(irisHex, crs = "EPSG:2154")
irisMarRepro <- st_transform(irisMar, crs = "EPSG:2154")
irisMayRepro <- st_transform(irisMay, crs = "EPSG:2154")
irisReuRepro <- st_transform(irisReu, crs = "EPSG:2154")

# Regroupement de toutes les iris
iris <- rbind(irisGuaRepro, irisGuyRepro, irisHexRepro, irisMarRepro, irisMayRepro, irisReuRepro)
st_write(iris, "IRIS.gpkg")

# Tables d'appartenance pour les communes
tab1 <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                             skip = 5, sheet = "COM")) 
tab2 <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-2024.xlsx",
                             skip = 5, sheet = "COM"))
# Conservation des colonnes qui nous intéressent
tab1.1 <- tab1[, c(1,6,11,12,14,15,16)]
tab2.1 <- tab2[, -c(4,7)]
# Jointure des tables
COM <- merge(tab2.1, tab1.1,
             by.x = "CODGEO", by.y = "CODGEO",
             all.x = TRUE)


# Tables d'appartenance pour les arrondissements municipaux
tab1 <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                             skip = 5, sheet = "ARM"))
tab2 <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-2024.xlsx",
                             skip = 5, sheet = "ARM"))
# Conservation des colonnes qui nous intéressent
tab1.1 <- tab1[, c(1,6,11,12,14,15,16)]
tab2.1 <- tab2[, -c(4,7,14)]
# Jointure des tables
ARM <- merge(tab2.1, tab1.1,
             by.x = "CODGEO", by.y = "CODGEO",
             all.x = TRUE)

# Rassembler communes et arrondissements municipaux
TAB <- rbind(COM, ARM)

IRIS <- merge(iris, COM, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)


# Trouver les lignes avec des valeurs NA dans n'importe quelle colonne
index_lignes_na <- which(apply(IRIS, 1, function(row) anyNA(row)))

# Sélectionner les lignes correspondantes
dfNA <- IRIS[index_lignes_na, ]

st_write(dfNA, "test.gpkg")





# Départements
dep <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$DEP),
                 FUN = sum)
colnames(dep)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "DEP",]
dep <- merge(dep, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
dep <- st_cast(dep, "MULTIPOLYGON")
st_write(dep, dsn = "input/fr/voronoi/dep.geojson")

















tot <- "C20_POP15P"
var <- "C20_POP15P_CS6"

# Sélection des variables définies
irisVar <- irisData[, c("IRIS", "COM", tot, var)]



# Jointure des IRIS avec la table de correspondance
irisJoin <- merge(iris, zon, 
                  by.x = "INSEE_COM", by.y = "CODGEO", 
                  all.x = TRUE)

# Jointure des IRIS avec les données
irisJoin <- merge(irisJoin, irisVar,
                  by.x = "CODE_IRIS", by.y = "IRIS",
                  all.x = TRUE)

# Renommage des colonnes data
colnames(irisJoin)[ncol(irisJoin) -2] <- "tot"
colnames(irisJoin)[ncol(irisJoin) -1] <- "var"

irisJoin$tot <- as.numeric(irisJoin$tot)
irisJoin$var <- as.numeric(irisJoin$var)







# Aggrégation des IRIS en commune
data <- aggregate(data[,c("tot", "var")],
                  by = list(data$COM), 
                  FUN = sum)
colnames(data)[1] <- "CODGEO"
zon <- tab_name[tab_name$NIVGEO == "COM",]
data <- merge(data, zon[,c("CODGEO", "LIBGEO")], 
              by = "CODGEO", 
              all.x = TRUE)
# Calcul du pourcentage de la variable par entité géographique
data$ptc_var <- data$var / data$tot *100







# 5 - Aggregate EPCI, ZEMP, UU, REG, EPCI, DEP ----
zon <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                            skip = 5, sheet = "COM")) 
zon2 <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                             skip = 5, sheet = "ARM")) 

sel <- c("CODGEO", "DEP", "REG", "EPCI", "ZE2020", "AAV2020", "TAAV2017")
zon <- rbind(zon[,sel], zon2[,sel])

zon_name <- data.frame(read_xlsx("input/fr/table-appartenance-geo-communes-22_v2022-09-27.xlsx",
                                 skip = 5, sheet = "Zones_supra_communales")) 


com <- merge(com, zon, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)


# Départements
dep <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$DEP),
                 FUN = sum)
colnames(dep)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "DEP",]
dep <- merge(dep, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
dep <- st_cast(dep, "MULTIPOLYGON")
st_write(dep, dsn = "input/fr/voronoi/dep.geojson")

# Régions
reg <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                 by = list(com$REG),
                 FUN = sum)
colnames(reg)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "REG",]
reg <- merge(reg, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
reg <- st_cast(reg, "MULTIPOLYGON")
st_write(reg, dsn = "input/fr/voronoi/reg.geojson")


# EPCI
epci <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                  by = list(com$EPCI),
                  FUN = sum)
colnames(epci)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "EPCI",]
epci <- merge(epci, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
              all.x = TRUE)
epci <- st_cast(epci, "MULTIPOLYGON")
st_write(epci, dsn = "input/fr/voronoi/epci.geojson")

# ZEMP
zemp <- aggregate(com[,c("POPULATION", "SUPERFICIE")],
                  by = list(com$ZE2020),
                  FUN = sum)
colnames(zemp)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "ZE2020",]
zemp <- merge(zemp, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
              all.x = TRUE)
zemp <- st_cast(zemp, "MULTIPOLYGON")
st_write(zemp, dsn = "input/fr/voronoi/zemp.geojson")

# AAV
com_urb <- com[com$AAV2020 != "000",]
aav <- aggregate(com_urb[,c("POPULATION", "SUPERFICIE")],
                 by = list(com_urb$AAV2020),
                 FUN = sum)
colnames(aav)[1] <- "CODGEO"
zon <- zon_name[zon_name$NIVGEO == "AAV2020",]
aav <- merge(aav, zon[,c("CODGEO", "LIBGEO")], by = "CODGEO",
             all.x = TRUE)
aav <- st_cast(aav, "MULTIPOLYGON")

zon <- aggregate(com_urb[,c("AAV2020", "TAAV2017")],
                 by = list(com_urb$AAV2020),
                 FUN = head, 1)
zon <- st_set_geometry(zon, NULL)
aav <- merge(aav, zon[,c("AAV2020", "TAAV2017")], by.x = "CODGEO", by.y = "AAV2020",
             all.x = TRUE)
st_write(aav, dsn = "input/fr/voronoi/aav.geojson")
