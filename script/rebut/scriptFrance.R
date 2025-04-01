################################################################################
################################ EXPLORATIONS POUR LA CONSTITUTION D'UN MAILLAGE
################################################################################

library(sf)
library(mapsf)
library(tidyverse)
library(data.table)
library(readxl)

############################################################# IMPORT DES DONNEES
irisBrut <- st_read("DATA/GPKG/IRIS.gpkg")
communeBrut <- st_read("DATA/GPKG/COMMUNE.gpkg") 
cantonBrut <- st_read("DATA/GPKG/CANTON.gpkg")
epciBrut <- st_read("DATA/GPKG/EPCI.gpkg")

rpCom2020Brut <- read_excel("DATA/base-cc-evol-struct-pop-2020.xlsx", sheet = "COM_2020", skip = 5)
rpCom2014Brut <- read_excel("DATA/base-cc-evol-struct-pop-2020.xlsx", sheet = "COM_2014", skip = 5)
rpIris2020Brut <- st_read("DATA/base-ic-evol-struct-pop-2020.csv")

###################################################################### NETTOYAGE
iris <- select(irisBrut, -c(1,4,7))
commune <- select(communeBrut, -c(1,3,5,6))
canton <- select(cantonBrut, -c(1))
epci <- select(epciBrut, -c(1))

rpCom2020 <- select(rpCom2020Brut, c(CODGEO, LIBGEO, 
                                     P20_POP, C20_POP15P, C20_POP15P_CS6))
rpCom2014 <- select(rpCom2014Brut, c(CODGEO, 
                                     P14_POP, C14_POP15P, C14_POP15P_CS6))
rpCom <- merge(rpCom2020,
               rpCom2014,
               by.x = "CODGEO",
               by.y = "CODGEO",
               all.x = TRUE)

rpIris2020 <- select(rpIris2020Brut, c(IRIS, COM,
                                       P20_POP, C20_POP15P, C20_POP15P_CS6))

rm(irisBrut, communeBrut, cantonBrut, epciBrut)
rm(rpCom2020Brut, rpCom2014Brut, rpIris2020Brut)

################################################################################
################################################################# LIMITES BRUTES
################################################################################

################################################################################ 1.1
############################################################### ECHELLE DES IRIS
# Jointure du RP avec les iris
mailleIris <- merge(iris,
                    rpIris2020,
                    by.x = "CODE_IRIS",
                    by.y = "IRIS",
                    all.x = TRUE)
# Calcul du pourcentage des CS6 par commune
mailleIris$ptc_CS6 <- as.numeric(mailleIris$C20_POP15P_CS6) / as.numeric(mailleIris$C20_POP15P) *100

################################################################## CARTE IRIS Q6
# Discrétisation
mailleIris$ptc_CS6_cat <- cut(mailleIris$ptc_CS6, 
                                 breaks = quantile(mailleIris$ptc_CS6, 
                                                   probs = c(0, 0.05, 0.275, 0.50, 0.725, 0.95, 1), 
                                                   na.rm = TRUE),
                                 include.lowest = TRUE)
# Carte
ggplot(mailleIris) +
  geom_sf(aes(fill = ptc_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#f6f6f6", "#dadada", "#b2b2b2", "#878787", "#575756", "#000000")) +
  theme_minimal() +
  labs(title = "Part des CS6 dans la population de plus de 15 ans",
       fill = "Discrétisation Q6 (%)") +
  guides(fill = guide_legend(reverse = TRUE))

################################################################## CARTE IRIS QL
# Calcul de la part des CS6 dans la population nationale
meanCS6 <- sum(as.numeric(mailleIris$C20_POP15P_CS6), na.rm = TRUE) / sum(as.numeric(mailleIris$C20_POP15P), na.rm = TRUE) *100
# Calcul du quotient de localisation pour chaque commune
mailleIris$QL_CS6 <- mailleIris$ptc_CS6 / meanCS6
# Discrétisation
mailleIris$QL_CS6_cat <- cut(mailleIris$QL_CS6, 
                             breaks = c(0, 0.7, 1.3, 2, Inf), 
                             na.rm = TRUE,
                             include.lowest = TRUE)
# Carte
ggplot(mailleIris) +
  geom_sf(aes(fill = QL_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#ededed", "#ffffff", "#b7bfdf", "#6984bc")) +
  theme_minimal() +
  labs(title = "Surreprésentation des CS6",
       fill = "Quotient de Localisation") +
  guides(fill = guide_legend(reverse = TRUE))


################################################################################ 1.2
########################################################### ECHELLE DES COMMUNES
# Jointure du RP avec les communes
mailleCommune <- merge(commune,
                       rpCom2020,
                       by.x = "INSEE_COM",
                       by.y = "CODGEO",
                       all.x = TRUE)
# Calcul du pourcentage des CS6 par commune
mailleCommune$ptc_CS6 <- round(mailleCommune$C20_POP15P_CS6 / mailleCommune$C20_POP15P *100, 2)

############################################################### CARTE COMMUNE Q6
# Discrétisation
mailleCommune$ptc_CS6_cat <- cut(mailleCommune$ptc_CS6, 
                                 breaks = quantile(mailleCommune$ptc_CS6, 
                                                   probs = c(0, 0.05, 0.275, 0.50, 0.725, 0.95, 1), 
                                                   na.rm = TRUE),
                                 include.lowest = TRUE)
# Carte
ggplot(mailleCommune) +
  geom_sf(aes(fill = ptc_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#f6f6f6", "#dadada", "#b2b2b2", "#878787", "#575756", "#000000")) +
  theme_minimal() +
  labs(title = "Part des CS6 dans la population de plus de 15 ans",
       fill = "Discrétisation Q6 (%)") +
  guides(fill = guide_legend(reverse = TRUE))

############################################################### CARTE COMMUNE QL
# Calcul de la part des CS6 dans la population nationale
meanCS6 <- sum(mailleCommune$C20_POP15P_CS6, na.rm = TRUE) / sum(mailleCommune$C20_POP15P, na.rm = TRUE) *100
# Calcul du quotient de localisation pour chaque commune
mailleCommune$QL_CS6 <- mailleCommune$ptc_CS6 / meanCS6
# Discrétisation
mailleCommune$QL_CS6_cat <- cut(mailleCommune$QL_CS6, 
                                breaks = c(0, 0.7, 1.3, 2, Inf), 
                                na.rm = TRUE,
                                include.lowest = TRUE)
# Carte
ggplot(mailleCommune) +
  geom_sf(aes(fill = QL_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#ededed", "#ffffff", "#b7bfdf", "#6984bc")) +
  theme_minimal() +
  labs(title = "Surreprésentation des CS6",
       fill = "Quotient de Localisation") +
  guides(fill = guide_legend(reverse = TRUE))


################################################################################ 1.3
############################################################ ECHELLE DES CANTONS
# Jointure du RP avec les communes
mailleCommune <- merge(commune,
                       rpCom2020,
                       by.x = "INSEE_COM",
                       by.y = "CODGEO",
                       all.x = TRUE)
# Création d'une colonne id pour les canton
canton$CODE_CAN <- paste(canton$INSEE_CAN, canton$INSEE_DEP, sep = "")
mailleCommune$CODE_CAN <- paste(mailleCommune$INSEE_CAN, mailleCommune$INSEE_DEP, sep = "")
# Convertir le dataframe en un data.table pour réduire le temps de calcul
setDT(mailleCommune)
# Effectuer le group_by et la somme sur les colonnes numériques
mailleCanton <- mailleCommune[, lapply(.SD, sum),
                        by = CODE_CAN,
                        .SDcols = c("C20_POP15P", "C20_POP15P_CS6")]
# Jointure avec les cantons pour retrouver leur géométrie
mailleCanton <- merge(canton, 
                      mailleCanton,
                      by.x = "CODE_CAN", 
                      by.y = "CODE_CAN", 
                      all.x = TRUE)
# Calcul pourcentage des CS6 par commune
mailleCanton$ptc_CS6 <- round(mailleCanton$C20_POP15P_CS6 / mailleCanton$C20_POP15P *100, 2)

################################################################ CARTE CANTON Q6
# Discrétisation
mailleCanton$ptc_CS6_cat <- cut(mailleCanton$ptc_CS6, 
                                 breaks = quantile(mailleCanton$ptc_CS6, 
                                                   probs = c(0, 0.05, 0.275, 0.50, 0.725, 0.95, 1), 
                                                   na.rm = TRUE),
                                 include.lowest = TRUE)
# Carte
ggplot(mailleCanton) +
  geom_sf(aes(fill = ptc_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#f6f6f6", "#dadada", "#b2b2b2", "#878787", "#575756", "#000000")) +
  theme_minimal() +
  labs(title = "Part des CS6 dans la population de plus de 15 ans",
       fill = "Discrétisation Q6 (%)") +
  guides(fill = guide_legend(reverse = TRUE))

################################################################ CARTE CANTON QL
# Calcul de la part des CS6 dans la population nationale
meanCS6 <- sum(mailleCanton$C20_POP15P_CS6, na.rm = TRUE) / sum(mailleCanton$C20_POP15P, na.rm = TRUE) *100
# Calcul du quotient de localisation pour chaque commune
mailleCanton$QL_CS6 <- mailleCanton$ptc_CS6 / meanCS6
# Discrétisation
mailleCanton$QL_CS6_cat <- cut(mailleCanton$QL_CS6, 
                                breaks = c(0, 0.7, 1.3, 2, Inf), 
                                na.rm = TRUE,
                                include.lowest = TRUE)
# Carte
ggplot(mailleCanton) +
  geom_sf(aes(fill = QL_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#ededed", "#ffffff", "#b7bfdf", "#6984bc")) +
  theme_minimal() +
  labs(title = "Surreprésentation des CS6",
       fill = "Quotient de Localisation") +
  guides(fill = guide_legend(reverse = TRUE))


################################################################################ 1.4
############################################################### ECHELLE DES EPCI
# Jointure du RP avec les communes
mailleCommune <- merge(commune,
                       rpCom2020,
                       by.x = "INSEE_COM",
                       by.y = "CODGEO",
                       all.x = TRUE)
# Convertir le dataframe en un data.table pour réduire le temps de calcul
setDT(mailleCommune)
# Effectuer le group_by et la somme sur les colonnes numériques
mailleEPCI <- mailleCommune[, lapply(.SD, sum),
                            by = SIREN_EPCI,
                            .SDcols = c("C20_POP15P", "C20_POP15P_CS6")]
# Jointure avec les cantons pour retrouver leur géométrie
mailleEPCI <- merge(epci, 
                    mailleEPCI,
                    by.x = "CODE_SIREN", 
                    by.y = "SIREN_EPCI", 
                    all.x = TRUE)
# Calcul pourcentage des CS6 par commune
mailleEPCI$ptc_CS6 <- round(mailleEPCI$C20_POP15P_CS6 / mailleEPCI$C20_POP15P *100, 2)

################################################################## CARTE EPCI Q6
# Discrétisation
mailleEPCI$ptc_CS6_cat <- cut(mailleEPCI$ptc_CS6, 
                                breaks = quantile(mailleEPCI$ptc_CS6, 
                                                  probs = c(0, 0.05, 0.275, 0.50, 0.725, 0.95, 1), 
                                                  na.rm = TRUE),
                                include.lowest = TRUE)
# Carte
ggplot(mailleEPCI) +
  geom_sf(aes(fill = ptc_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#f6f6f6", "#dadada", "#b2b2b2", "#878787", "#575756", "#000000")) +
  theme_minimal() +
  labs(title = "Part des CS6 dans la population de plus de 15 ans",
       fill = "Discrétisation Q6 (%)") +
  guides(fill = guide_legend(reverse = TRUE))

################################################################## CARTE EPCI QL
# Calcul de la part des CS6 dans la population nationale
meanCS6 <- sum(mailleEPCI$C20_POP15P_CS6, na.rm = TRUE) / sum(mailleEPCI$C20_POP15P, na.rm = TRUE) *100
# Calcul du quotient de localisation pour chaque commune
mailleEPCI$QL_CS6 <- mailleEPCI$ptc_CS6 / meanCS6
# Discrétisation
mailleEPCI$QL_CS6_cat <- cut(mailleEPCI$QL_CS6, 
                             breaks = c(0, 0.7, 1.3, 2, Inf), 
                             na.rm = TRUE,
                             include.lowest = TRUE)
# Carte
ggplot(mailleEPCI) +
  geom_sf(aes(fill = QL_CS6_cat), color = NA) +
  scale_fill_manual(values = c("#ededed", "#ffffff", "#b7bfdf", "#6984bc")) +
  theme_minimal() +
  labs(title = "Surreprésentation des CS6",
       fill = "Quotient de Localisation") +
  guides(fill = guide_legend(reverse = TRUE))


################################################################################ 1.5
######################################################################### GRILLE
# Import de grilles pré-créées
grille <- st_read("DATA/GPKG/Grille_10km.gpkg")
# Création d'une grille composée de carreaux de 10km de côté
# grid <- st_make_grid(commune, cellsize = c(10000, 10000), what = "polygons")
# grille <- st_sf(geometry = grid)
# grille$id <- row.names(grille)
# grille <- grille %>%
#   select(id, geometry)

################################################################### RECOUVREMENT
# Jointure du RP avec les communes
mailleCommune <- merge(commune,
                       rpCom,
                       by.x = "INSEE_COM",
                       by.y = "CODGEO",
                       all.x = TRUE)
# Calculer la surface des communes
mailleCommune$areacom <- st_area(mailleCommune)
# Calculer la surface des carreaux
grille$areacaro <- st_area(grille)

# Découper les communes en fonction des carreaux
commune_decoup <- st_intersection(mailleCommune, grille)
# Calculer la surface de chaque morceau de commune découpé
commune_decoup$areaboucom <- st_area(commune_decoup)
# Attribution des données en fonction du taux de recouvrement
commune_decoup$POP_20 <- commune_decoup$P20_POP * (commune_decoup$areaboucom / commune_decoup$areacom)
commune_decoup$POP15_20 <- commune_decoup$C20_POP15P * (commune_decoup$areaboucom / commune_decoup$areacom)
commune_decoup$CS6_20 <- commune_decoup$C20_POP15P_CS6 * (commune_decoup$areaboucom / commune_decoup$areacom)
commune_decoup$POP_14 <- commune_decoup$P14_POP * (commune_decoup$areaboucom / commune_decoup$areacom)
commune_decoup$POP15_14 <- commune_decoup$C14_POP15P * (commune_decoup$areaboucom / commune_decoup$areacom)
commune_decoup$CS6_14 <- commune_decoup$C14_POP15P_CS6 * (commune_decoup$areaboucom / commune_decoup$areacom)

com_decoup <- select(commune_decoup, c(id, 
                                       POP_20, POP15_20, CS6_20, 
                                       POP_14, POP15_14, CS6_14))

# Convertir le dataframe en un data.table pour réduire le temps de calcul
setDT(com_decoup)
# Effectuer le group_by et la somme sur les colonnes numériques
carroAgreg <- com_decoup[, lapply(.SD, sum),
                         by = id,
                         .SDcols = !c("geometry", "id")]
# Calcul pourcentage de la variable par carreau
carroAgreg$explo <- round((carroAgreg$CS6_20 / carroAgreg$POP15_20) * 100, digits = 3)
# Calcul de la part de la variable dans la population nationale
meanExplo <- sum(carroAgreg$CS6_20, na.rm = TRUE) / sum(carroAgreg$POP15_20, na.rm = TRUE) *100
# Jointure avec les carreaux pour retrouver leur géométrie
carroJoin <- merge(grille, 
                   carroAgreg,
                   by.x = "id", 
                   by.y = "id", 
                   all.x = TRUE)

################################################################ CARTE GRILLE Q6
# Discrétisation
carroJoin$explo_Q6_cat <- cut(carroJoin$explo, 
                              breaks = quantile(carroJoin$explo, 
                              probs = c(0, 0.05, 0.275, 0.50, 0.725, 0.95, 1), 
                              na.rm = TRUE),
                              include.lowest = TRUE)
# Carte
ggplot(carroJoin) +
  geom_sf(aes(fill = explo_Q6_cat), color = NA) +
  scale_fill_manual(values = c("#f6f6f6", "#dadada", "#b2b2b2", "#878787", "#575756", "#000000")) +
  theme_minimal() +
  labs(title = "Exploration",
       fill = "Discrétisation Q6 (%)") +
  guides(fill = guide_legend(reverse = TRUE))

################################################################ CARTE GRILLE QL
# Calcul du quotient de localisation pour chaque carreau
carroJoin$explo_QL <- carroJoin$explo / meanExplo
# Discrétisation
carroJoin$explo_QL_cat <- cut(carroJoin$explo_QL, 
                              breaks = c(0, 0.7, 1.3, 2, Inf), 
                              na.rm = TRUE,
                              include.lowest = TRUE)
# Carte
ggplot(carroJoin) +
  geom_sf(aes(fill = explo_QL_cat), color = NA) +
  scale_fill_manual(values = c("#ededed", "#ffffff", "#b7bfdf", "#6984bc")) +
  theme_minimal() +
  labs(title = "Surreprésentation",
       fill = "Quotient de Localisation") +
  guides(fill = guide_legend(reverse = TRUE))



################################################################################
################################################################# LIMITES MIXTES
################################################################################

################################################################################ 2.1
################################################################## CANTON - IRIS
# CANTONS
# Jointure du RP avec les communes
mailleCommune <- merge(commune,
                       rpCom2020,
                       by.x = "INSEE_COM",
                       by.y = "CODGEO",
                       all.x = TRUE)
# Création d'une colonne id pour les cantons
canton$CODE_CAN <- paste(canton$INSEE_CAN, canton$INSEE_DEP, sep = "")
mailleCommune$CODE_CAN <- paste(mailleCommune$INSEE_CAN, mailleCommune$INSEE_DEP, sep = "")
# Convertir le dataframe en un data.table pour réduire le temps de calcul
setDT(mailleCommune)
# Effectuer le group_by et la somme sur les colonnes numériques
mailleCanton <- mailleCommune[, lapply(.SD, sum),
                              by = CODE_CAN,
                              .SDcols = c("P20_POP", "C20_POP15P", "C20_POP15P_CS6")]
# Jointure avec les cantons pour retrouver leur géométrie
mailleCanton <- merge(canton, 
                      mailleCanton,
                      by.x = "CODE_CAN", 
                      by.y = "CODE_CAN", 
                      all.x = TRUE)
# Traitements
mailleCanton$ptc_CS6 <- mailleCanton$C20_POP15P_CS6 / mailleCanton$C20_POP15P * 100
mailleCanton$densite <- mailleCanton$P20_POP / (st_area(mailleCanton)/1000000)

# Export
st_write(mailleCanton, "canton.gpkg")



# IRIS
# Jointure du RP avec les iris
mailleIris <- merge(iris,
                    rpIris2020,
                    by.x = "CODE_IRIS",
                    by.y = "IRIS",
                    all.x = TRUE)
mailleIris <- mailleIris %>% 
  filter(substr(CODE_IRIS, nchar(CODE_IRIS) - 3, nchar(CODE_IRIS)) != "0000") %>% 
  select(c(CODE_IRIS, P20_POP, C20_POP15P, C20_POP15P_CS6)) 
# Traitements
mailleIris$P20_POP <- as.numeric(mailleIris$P20_POP)
mailleIris$C20_POP15P <- as.numeric(mailleIris$C20_POP15P)
mailleIris$C20_POP15P_CS6 <- as.numeric(mailleIris$C20_POP15P_CS6)
mailleIris$ptc_CS6 <- mailleIris$C20_POP15P_CS6 / mailleIris$C20_POP15P * 100
mailleIris$densite <- mailleIris$P20_POP / (st_area(mailleIris)/1000000)
# Export
st_write(mailleIris, "iris.gpkg")

