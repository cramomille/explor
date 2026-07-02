
#                                                TYPOLOGIE DES LIEUX DE TRAVAIL
#                                     BASE DE DONNEES SUBWORK - NOMENCLATURE CP
#                                EXPLORATIONS ET TESTS SUR DES DONNEES A L'IRIS
#
#                                                                Antoine Beroud
#                                                             Nicolas Raimbault
#                                                                  Octobre 2025


# INSTALLATION DES PACKAGES ---------------------------------------------------
# Installation du package remotes pour permettre l'installation de packages 
# presents sur GitLab
# install.packages("remotes")

# Installation des packages asf et subwork
remove.packages("asf")
remotes::install_gitlab("atlas-social-de-la-france/asf", 
                        host = "gitlab.huma-num.fr",
                        build_vignettes = TRUE)   
remotes::install_github("alietteroux/subwork")

library(sf)
library(mapsf)
library(asf)
library(questionr)
library(tidyverse)
library(data.table)
library(readxl)
library(RColorBrewer)
# library(rgdal)
library(SpatialPosition)
library(FactoMineR)
library(factoextra) 
library(Factoshiny)
library(corrplot)
library(subwork)
library(MetBrewer)
library(explor)
library(gt)
library(corrplot) # pour le cacul de la matrice de corrélation

# dossier de travail
# setwd("C:/Users/raimbault-n/Nextcloud/Papier - conf - presentations/Atlas Social de France/Typo CP/ACP CAH 13classes")

# IMPORT DES DONNEES ----------------------------------------------------------
# Emplois au lieu de travail selon cp2 subwork
# Import des donnees "FT810" a l'IRIS dans un data.frame nomme "FT810.data"
FT810.data <- import(code = "FT810", type = "data")
FT712.data <- import(code = "FT712", type = "data")
FD11.data <- import(code = "FD11", type = "data")
variable.names(FT810.data)
variable.names(FT712.data)
variable.names(FD11.data)

# Analyse des données agrégées

## Données nationales (FD11) : comparaison RP18 et BS18 pour la CP2
data.nat <- select (FD11.data, nomenclature, nom.cat.code, RP18_I_Tfmet, BS18_S_Tfmet) 
data.nat <-filter(data.nat, nomenclature == 'CP2')

### analyse de correlation et régression linéaire
cor(data.nat$RP18_I_Tfmet,data.nat$BS18_S_Tfmet, use = "complete.obs")
# R = 0.84 / R² = 0.7
test <- cor(data.nat$RP18_I_Tfmet,data.nat$BS18_S_Tfmet, use = "complete.obs")
dr <- lm(RP18_I_Tfmet ~ BS18_S_Tfmet, data = data.nat)
formule <- paste0("y=",round(dr$coefficients[2],2), "x+", round(dr$coefficients[1],2), " R:", round(cor(data.nat$RP18_I_Tfmet,data.nat$BS18_S_Tfmet),2), " R²:", round(cor(data.nat$RP18_I_Tfmet,data.nat$BS18_S_Tfmet)^2,2))
ggplot(data.nat) + geom_point(aes(x=RP18_I_Tfmet, y=BS18_S_Tfmet)) + geom_smooth(aes(x=RP18_I_Tfmet, y=BS18_S_Tfmet),method="lm", formula=y~x, se=F)+ ggtitle(formule)

## En enlevant les non-salariés
data.nat.sal <-filter(data.nat, BS18_S_Tfmet >= 198529 )
cor(data.nat.sal$RP18_I_Tfmet,data.nat.sal$BS18_S_Tfmet, use = "complete.obs")
# R = 0.84 / R² = 0.7
test <- cor(data.nat.sal$RP18_I_Tfmet,data.nat.sal$BS18_S_Tfmet, use = "complete.obs")
dr <- lm(RP18_I_Tfmet ~ BS18_S_Tfmet, data = data.nat.sal)
formule <- paste0("y=",round(dr$coefficients[2],2), "x+", round(dr$coefficients[1],2), " R:", round(cor(data.nat.sal$RP18_I_Tfmet,data.nat.sal$BS18_S_Tfmet),2), " R²:", round(cor(data.nat.sal$RP18_I_Tfmet,data.nat.sal$BS18_S_Tfmet)^2,2))
ggplot(data.nat.sal) +
  geom_point(aes(x=RP18_I_Tfmet, y=BS18_S_Tfmet)) +
  geom_smooth(aes(x=RP18_I_Tfmet, y=BS18_S_Tfmet),method="lm", formula=y~x, se=F)+
  ggtitle(formule)
# Le coefficient est à peine meilleur

dr <- lm(RP18_I_Tfmet ~ BS18_S_Tfmet, data = data.nat.sal)
data.nat.sal$predicted <- data.nat.sal$RP18_I_Tfmet*dr$coefficients[2]+dr$coefficients[1]
data.nat.sal$residuals <- dr$residuals

### analyse des résidus
ggplot(data.nat.sal, aes(x=RP18_I_Tfmet, y=BS18_S_Tfmet)) +
  geom_smooth(method="lm", se = F, color="lightgrey", formula=y ~ x) +
  geom_segment(aes(xend = RP18_I_Tfmet, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals, size = abs(residuals))) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

## Comparaison des totaux au lieu de travail entre BS18 par IRIS et RP18 par commune pour CP2
## La BS ne permet de prendre en compte les indépendants. Elles semblent augmenter les effectifs des classes pop, surtout des ouv Q de l'indu

sum(FT810.data$BS18_S_T_CP2_C1.Agri, na.rm = TRUE) #3 436 (à enlever)
sum(FT712.data$RP18_I_T_CP2_C1.Agri, na.rm = TRUE) # 359 604.4
sum(FT810.data$BS18_S_T_CP2_C21.Arti, na.rm = TRUE)#7 761 (à enlever)
sum(FT712.data$RP18_I_T_CP2_C21.Arti, na.rm = TRUE) # 795 730.4
sum(FT810.data$BS18_S_T_CP2_C22.Comm, na.rm = TRUE)#43 303 (à enlever)
sum(FT712.data$RP18_I_T_CP2_C22.Comm, na.rm = TRUE) # 662 227.8
sum(FT810.data$BS18_S_T_CP2_C23.Chefs, na.rm = TRUE) #102 265. (à enlever)
sum(FT712.data$RP18_I_T_CP2_C23.Chefs, na.rm = TRUE) # 175 534.5
sum(FT810.data$BS18_S_T_CP2_C31.PLib, na.rm = TRUE) #39 426 (à enlever)
sum(FT712.data$RP18_I_T_CP2_C31.PLib, na.rm = TRUE) # 523 358.8
sum(FT810.data$BS18_S_T_CP2_C32.CPubli, na.rm = TRUE)#1 413 875
sum(FT712.data$RP18_I_T_CP2_C32.CPubli, na.rm = TRUE) #1 598 406
sum(FT810.data$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE) #3 175 419
sum(FT712.data$RP18_I_T_CP2_C36.CPriv, na.rm = TRUE) #2 606 851
sum(FT810.data$BS18_S_T_CP2_C41.PIpub, na.rm = TRUE) #2 548 734
sum(FT712.data$RP18_I_T_CP2_C41.PIpub, na.rm = TRUE) #2 876 299
sum(FT810.data$BS18_S_T_CP2_C46.PIent, na.rm = TRUE) #1 471 121
sum(FT712.data$RP18_I_T_CP2_C46.PIent, na.rm = TRUE) #2 144 950
sum(FT810.data$BS18_S_T_CP2_C47.Techn, na.rm = TRUE) #1 059 321
sum(FT712.data$RP18_I_T_CP2_C47.Techn, na.rm = TRUE) #1 125 886
sum(FT810.data$BS18_S_T_CP2_C48.Contr, na.rm = TRUE) #400 401 (à regrouper au-dessus)
sum(FT712.data$RP18_I_T_CP2_C48.Contr, na.rm = TRUE) #487 501
sum(FT810.data$BS18_S_T_CP2_T511.Admin, na.rm = TRUE) #1 891 564
sum(FT712.data$RP18_I_T_CP2_T511.Admin, na.rm = TRUE) #1 348 173
sum(FT810.data$BS18_S_T_CP2_T512.Comm, na.rm = TRUE) #882 294
sum(FT712.data$RP18_I_T_CP2_T512.Comm, na.rm = TRUE) #490 782
sum(FT810.data$BS18_S_T_CP2_T513.Police, na.rm = TRUE) #192 311
sum(FT712.data$RP18_I_T_CP2_T513.Police, na.rm = TRUE) #365 306.9
sum(FT810.data$BS18_S_T_CP2_T514.Public, na.rm = TRUE) #796 630
sum(FT712.data$RP18_I_T_CP2_T514.Public, na.rm = TRUE) #728 027.9
sum(FT810.data$BS18_S_T_CP2_T515.Sante, na.rm = TRUE) #751 400 
sum(FT712.data$RP18_I_T_CP2_T515.Sante, na.rm = TRUE) #719 094.7
sum(FT810.data$BS18_S_T_CP2_T521.Care, na.rm = TRUE) #779 248
sum(FT712.data$RP18_I_T_CP2_T521.Care, na.rm = TRUE) #952 431
sum(FT810.data$BS18_S_T_CP2_T522.Vente, na.rm = TRUE) #1 979 965
sum(FT712.data$RP18_I_T_CP2_T522.Vente, na.rm = TRUE) #1 131 486
sum(FT810.data$BS18_S_T_CP2_T523.Gard, na.rm = TRUE) #185 730 (à regrouper avec autres)
sum(FT712.data$RP18_I_T_CP2_T523.Gard, na.rm = TRUE) #157 824.9
sum(FT810.data$BS18_S_T_CP2_T524.Autres, na.rm = TRUE) #1 148 266
sum(FT712.data$RP18_I_T_CP2_T524.Autres, na.rm = TRUE) #980 411.5
sum(FT810.data$BS18_S_T_CP2_T611.Indus.Q, na.rm = TRUE) #2 136 630
sum(FT712.data$RP18_I_T_CP2_T611.Indus.Q, na.rm = TRUE) #852 338.2
sum(FT810.data$BS18_S_T_CP2_T612.Arti.Q, na.rm = TRUE) #1 524 216
sum(FT712.data$RP18_I_T_CP2_T612.Arti.Q, na.rm = TRUE) #1 101 681
sum(FT810.data$BS18_S_T_CP2_T621.Indus.PQ, na.rm = TRUE) # 690 468
sum(FT712.data$RP18_I_T_CP2_T621.Indus.PQ, na.rm = TRUE) #666 228.3
sum(FT810.data$BS18_S_T_CP2_T622.Arti.PQ, na.rm = TRUE) #885 605
sum(FT712.data$RP18_I_T_CP2_T622.Arti.PQ, na.rm = TRUE) #779 961.1
sum(FT810.data$BS18_S_T_CP2_T631.Transp, na.rm = TRUE) #1 855 977
sum(FT712.data$RP18_I_T_CP2_T631.Transp, na.rm = TRUE) #1 353 506
sum(FT810.data$BS18_S_T_CP2_T632.Agri, na.rm = TRUE) #338 082
sum(FT712.data$RP18_I_T_CP2_T632.Agri, na.rm = TRUE) #177 019.7


# Production d'un tableau data.cp2.
# Evolution : Production d'un tableau data.cp2. avec SEULEMENT LES SALARIES car la source n'est pas fiable pour les independants
# Source des donnees : base tous salaries que l'on a regroupe a l'IRIS

data.cp2 <- select(FT810.data, IRIS, IRIS_LIB, COM, BS18_S_T, BS18_S_T_CP2_C32.CPubli, 
                   BS18_S_T_CP2_C36.CPriv, BS18_S_T_CP2_C41.PIpub, BS18_S_T_CP2_C46.PIent, BS18_S_T_CP2_C47.Techn, BS18_S_T_CP2_C48.Contr,
                   BS18_S_T_CP2_T511.Admin, BS18_S_T_CP2_T512.Comm, BS18_S_T_CP2_T513.Police, BS18_S_T_CP2_T514.Public, BS18_S_T_CP2_T515.Sante, BS18_S_T_CP2_T521.Care, BS18_S_T_CP2_T522.Vente, BS18_S_T_CP2_T523.Gard, BS18_S_T_CP2_T524.Autres,
                   BS18_S_T_CP2_T611.Indus.Q, BS18_S_T_CP2_T612.Arti.Q, BS18_S_T_CP2_T621.Indus.PQ, BS18_S_T_CP2_T622.Arti.PQ, BS18_S_T_CP2_T631.Transp, BS18_S_T_CP2_T632.Agri)

# IMPORT DU MAILLAGE ---------------------------------------------------------- 
# mise a jour juillet 2025 avec nouveau format de la fonction d'Aliette : 

# Donnees sur le maillage des IRIS regroupees (vérifier ma bonne prise en compte de la dernière MAJ)
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_f")

# tabl <- asf_mar(maille = "irisf", geom = FALSE)

# Jointure du code des iris de reference (irisf) avec les iris subwork et les donnees data.cp2 associees
dataf <- merge(tabl[, c("IRIS_CODE", "IRISF_CODE")], data.cp2, 
               by.x = "IRIS_CODE", by.y = "IRIS")

# Verification du nombre d'iris differentes : 48638 pour data.cp2 puis 48637 pour dataf (jointure)
sum(length(unique(data.cp2$IRIS)))
sum(length(unique(dataf$IRIS_CODE))) # je perds 1 IRIS...

# On perd l'IRIS de la commune de Plottes (71353)
id <- setdiff(data.cp2$IRIS, dataf$IRIS_CODE)
data.cp2$IRIS_LIB[data.cp2$IRIS == id]


# Code des iris regroupes pour une analyse synchronique : VERIFIER SI LA MAILLE IRIS_R2 (2000 hab) CONVIENT BIEN. Quid IRIS_R5 (5000 hab) ?
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r2")

# Jointure du code des iris de reference (irisf) avec les iris regroupes 2000 HAB (irisrd)
datar <- merge(tabl[, c("IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB")], dataf, 
               by = "IRISF_CODE")

sum(data.cp2$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE)
sum(dataf$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE)
sum(datar$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE) #il y a des doublons dans datar

# Voir les lignes en doublon sur la colonne id
doublon <- datar[duplicated(datar$IRIS_CODE) | duplicated(datar$IRIS_CODE, fromLast = TRUE), ]

# Suppression des doublons
datar <- datar[!duplicated(datar$IRIS_CODE), ]

sum(data.cp2$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE)
sum(dataf$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE)
sum(datar$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE) # les totaux sont bons, il n'y a plus de doublons dans datar



# AGREGATION DES DONNEES ------------------------------------------------------
# Agregation simple
sum <- rowsum(datar[, c(7:28)], group = datar$IRISrD_CODE, na.rm = TRUE)
datar <- data.frame(IRISrD_CODE = rownames(sum), sum, row.names = NULL)

sum(datar$BS18_S_T_CP2_C36.CPriv, na.rm = TRUE) # le total reste le même après agrégation


# JOINTURE AVEC LE FOND GEOGRAPHIQUE ------------------------------------------
# Import du fond d'Aliette a partir du package asf
mar <- asf_mar(md = "iris_xxxx", ma = "iris_r2", geom = TRUE)

tabl <- mar$tabl
geom <- mar$geom

# Agregation des iris en IRIS regroupees S
fond <- asf_fond(geom, 
                 tabl, 
                 by = "IRISF_CODE", 
                 maille = "IRISrD_CODE", 
                 keep = c("IRISrD_LIB", "DEP", "OM_CODE"))

# Repositionnement des DROM
fond <- asf_drom(fond, 
                 id = "IRISrD_CODE")

# Creation de zooms
z <- asf_zoom(fond, 
              places = c("4", "5"), 
              r = 20000)

## A voir
# z <- asf_zoom(fond,
#               places = c("Paris", "Nantes", "Marseille", "Lyon"), 
#               coords = c(5.721, 45.182), labels = "Grenoble", 
#               r = 10000)

zoom <- z$zooms
label <- z$labels

# Recuperation des limites departementales
dep <- asf_borders(f = fond, by = "DEP", keep = 0.01)


# Jointure entre le fond et les donnees agregees
fondata <- asf_fondata(f = fond,
                       z = zoom, 
                       d = datar, 
                       by = "IRISrD_CODE")

variable.names(fondata)

# Creation d'une carte de test : cela fonctionne bien mais on pourrait en profiter pour supprimer les DROM (pas de données Subwork)
mf_map(fondata, 
       var = "BS18_S_T_CP2_C36.CPriv", 
       type = "choro",
       border = NA)
mf_map(dep, 
       type = "base",
       col = "white",
       lwd = 1.5,
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


# CALCUL DES TAUX -------------------------------------------------------------

# Construction des colonnes en parts avec regroupements de 2 catégories CP2 dans le tableau datar (datar ci-dessous)
datar$P_T_CP2_C36.CPriv <- datar$BS18_S_T_CP2_C36.CPriv/datar$BS18_S_T*100
datar$P_T_CP2_C32.CPubli <- datar$BS18_S_T_CP2_C32.CPubli/datar$BS18_S_T*100
datar$P_T_CP2_C41.PIpub <- datar$BS18_S_T_CP2_C41.PIpub/datar$BS18_S_T*100
datar$P_T_CP2_C46.PIent <- datar$BS18_S_T_CP2_C46.PIent/datar$BS18_S_T*100
datar$P_T_CP2_C47.C48.Techn.Contr <- (datar$BS18_S_T_CP2_C47.Techn+datar$BS18_S_T_CP2_C48.Contr)/datar$BS18_S_T*100
datar$P_T_CP2_T511.Admin <- datar$BS18_S_T_CP2_T511.Admin/datar$BS18_S_T*100
datar$P_T_CP2_T512.Comm <- datar$BS18_S_T_CP2_T512.Comm/datar$BS18_S_T*100
datar$P_T_CP2_T513.Police <- datar$BS18_S_T_CP2_T513.Police/datar$BS18_S_T*100
datar$P_T_CP2_T514.Public <- datar$BS18_S_T_CP2_T514.Public/datar$BS18_S_T*100
datar$P_T_CP2_T515.Sante <- datar$BS18_S_T_CP2_T515.Sante/datar$BS18_S_T*100
datar$P_T_CP2_T521.Care <- datar$BS18_S_T_CP2_T521.Care/datar$BS18_S_T*100
datar$P_T_CP2_T522.Vente <- datar$BS18_S_T_CP2_T522.Vente/datar$BS18_S_T*100
datar$P_T_CP2_T523.T524.Autres.Gard <- (datar$BS18_S_T_CP2_T523.Gard+datar$BS18_S_T_CP2_T524.Autres)/datar$BS18_S_T*100
datar$P_T_CP2_T611.Indus.Q <- datar$BS18_S_T_CP2_T611.Indus.Q/datar$BS18_S_T*100
datar$P_T_CP2_T612.Arti.Q <- datar$BS18_S_T_CP2_T612.Arti.Q/datar$BS18_S_T*100
datar$P_T_CP2_T621.Indus.PQ <- datar$BS18_S_T_CP2_T621.Indus.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T622.Arti.PQ <- datar$BS18_S_T_CP2_T622.Arti.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T631.Transp <- datar$BS18_S_T_CP2_T631.Transp/datar$BS18_S_T*100
datar$P_T_CP2_T632.Agri <- datar$BS18_S_T_CP2_T632.Agri/datar$BS18_S_T*100

summary(datar)

datar$verif.P <- datar$P_T_CP2_C36.CPriv + datar$P_T_CP2_C32.CPubli + datar$P_T_CP2_C41.PIpub + datar$P_T_CP2_C46.PIent +
  datar$P_T_CP2_C47.C48.Techn.Contr + datar$P_T_CP2_T511.Admin + datar$P_T_CP2_T512.Comm + datar$P_T_CP2_T513.Police + datar$P_T_CP2_T514.Public + datar$P_T_CP2_T515.Sante +
  datar$P_T_CP2_T521.Care + datar$P_T_CP2_T522.Vente + datar$P_T_CP2_T523.T524.Autres.Gard + datar$P_T_CP2_T611.Indus.Q + datar$P_T_CP2_T612.Arti.Q + datar$P_T_CP2_T621.Indus.PQ +
  datar$P_T_CP2_T622.Arti.PQ + datar$P_T_CP2_T631.Transp + datar$P_T_CP2_T632.Agri

summary(datar$verif.P) # verif ok car pas d'IS > 100 Mais 2 NA qui correspondent à des valeurs 0
#IRIS 940330111 = Bois Cadet 2
#IRIS 570320103 = Sud
fondata$IRISrD_LIB[fondata$IRISrD_CODE == 940330111]
fondata$IRISrD_LIB[fondata$IRISrD_CODE == 570320103]

# ACP CP2 ---------------------------------------------------------------------

## Tableau ACP a partir de datar ----
acp.cp2 <- select(datar,  IRISrD_CODE, starts_with("P_T_CP2")) |> 
  column_to_rownames(var = "IRISrD_CODE")

# note : 2 IRIS avec valeur NA : 940330111 (Fontenay sous bois) et 570320103 (Sud), cf supra + des NA dans les cases. A remplacer par un chiffre aléatoire entre 

## ACP
res.acp <- PCA(acp.cp2, graph=F)

d.acp.res <- acp.cp2 |> rownames_to_column(var = "IRISrD_CODE") |> 
  left_join(as.data.frame(res.acp$ind$coord) |> 
              rownames_to_column(var = "IRISrD_CODE"), by = "IRISrD_CODE")

## Mise en forme des résultats de l'ACP

### % d’inertie expliquée par chaque axe
fviz_eig(res.acp, addlabels = TRUE)

### Plan factoriel des variables
fviz_pca_var(res.acp, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

fviz_pca_var(res.acp, axes=c(3,4), col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

fviz_pca_var(res.acp, axes=c(1,5), col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

### Carto des coordonnées sur les axes 1, 2 et 3 pour l'analyse avec ggplot : comment ajouter les départements et les labels ? Ou les faire via mf_map
fondata |> filter(OM_CODE == "FXX") |>
  left_join(d.acp.res, by = c("IRISrD_CODE" = "IRISrD_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.1), col = NA) +
  scale_fill_gradient2(low = "blue2", high = "red3", mid = "white",
                       midpoint = 0) + theme_void()

fondata |> filter(OM_CODE == "FXX") |> 
  left_join(d.acp.res, by = c("IRISrD_CODE" = "IRISrD_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.2), col = NA) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0) + theme_void()

fondata |> filter(OM_CODE == "FXX") |> 
  left_join(d.acp.res, by = c("IRISrD_CODE" = "IRISrD_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.3), col = NA) +
  scale_fill_gradient2(low = "blue2", high = "red2", mid = "white",
                       midpoint = 0) + theme_void()

fondata |> filter(OM_CODE == "FXX") |> 
  left_join(d.acp.res, by = c("IRISrD_CODE" = "IRISrD_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.4), col = NA) +
  scale_fill_gradient2(low = "blue2", high = "red4", mid = "white",
                       midpoint = 0) + theme_void()

# CAH CP2 : CAH à 11 clusters sur l’ACP sur les 7 premières dimensions (61,1% variance) --------------------------------------------------------------
# (après  comparaison et vis-à-vis des gains d'inertie, c'est la typo qui me semble la plus riche)

## NOTES
## CAH A 12 clusters = un articare en plus
## CAH A 13 clusters = un indus en plus = CHOIX FINAL
## CAH A 14 clusters = 2 agri-arti-care
## CAH A 15 clusters = 2 arti-care
## CAH A 16 clusters = 2 indu-arti

# CAH A 13 clusters VERSION FINALE

n.clust <- 13
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

## graphe des gains gains d'inertie
plot(res.cah, choice="bar") 
# Note : 13 classes semblent un bon compromis

## Dendogramme : version de base ==> toujours dommage de ne pas visualiser les numéros des classes...
## Note : Version fviz ne fonctionne pas, alors que cela permettrait de gérer les couleurs pour retrouver les classes...)

plot(res.cah, choice = "tree", ind.names=F)

## Nombre d'individus par clusters
summary(d.acp.cah.res$clust)
# 1    2    3    4    5    6    7    8    9   10   11   12   13 
# 1091  512 1528 1395 2342 2400 2102 1318 2110 1442 1422 1529 1351 

## Classes sur le plan factoriel : dim 1,2,3,4,5.
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(1,5), ind.names = F, draw.tree = F)


# construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## Renumérotation des clusters avec leur nom final

### Créer une colonne avec la renumérotation des clusters (classes)

final.d.acp.cah.res <- final.d.acp.cah.res %>%
  mutate(classes = recode(clust,
                         "1" = "1",
                         "3" = "2",
                         "2" = "3",
                         "8" = "4",
                         "13" = "5",
                         "11" = "6",
                         "9" = "7",
                         "4" = "8", 
                         "7" = "9",
                         "5" = "10", 
                         "10" = "11",
                         "12" = "12",
                         "6"="13")) %>%
  arrange(final.d.acp.cah.res, classes)

### créer une colone avec un code nominal des classes

final.d.acp.cah.res <- final.d.acp.cah.res %>%
  mutate(code = recode(classes,
                      "1" = "pub1",
                      "2" = "pub2",
                      "3" = "pub-indu",
                      "4" = "pub-indu2",
                      "5" = "tertiaire_sup",
                      "6" = "comm",
                      "7" = "servprox", 
                      "8" = "servproxpub",
                      "9" = "articare", 
                      "10" = "agriarticare", 
                      "11" = "induarti",
                      "12" = "ZAE1",
                      "13" = "ZAE2"))

### créer une colonne avec le nom des clusters

final.d.acp.cah.res <- final.d.acp.cah.res %>%
  mutate(nom = recode(classes,
                       "1" = "services publics 1",
                       "2" = "services publics 2",
                       "3" = "emplois publics et industriels 1",
                      "4" = "emplois publics et industriels 2",
                      "5" = "tertiaire supérieur",
                       "6" = "espaces commerciaux",
                       "7" = "services de proximités", 
                       "8" = "services de proximité notamment publics",
                       "9" = "salariat artisanal et care",
                      "10" = "salariat agricole et care",
                      "11" = "zones industrielles et artisanales",
                       "12" = "zone industrielles et logistiques 1", 
                       "13" = "zone industrielles et logistiques 2"))

# Tableau de description des profils des clusters ------------------------------

## agrégation par cluster version classes

tab_clust <- final.d.acp.cah.res %>% group_by (classes) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100
tab_clust[order(tab_clust$classes),]

## description du profil des clusters : calcul des moyennes et écart-types

### Tableau avec classes organisé en ordre croissant pour la visualisation des profils
tab_clust$classes <- factor(tab_clust$classes, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))

### calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-classes) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(classes, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=classes),stat="identity")+  
  scale_fill_manual(values = c("purple4", "purple2", "darkorange2",  "orange", "darkblue", "dodgerblue2", "yellow", "greenyellow","aquamarine3", "darkolivegreen4", "orangered", "red2", "red4" )) +
  coord_flip()+  
  facet_wrap(~classes) +   
  theme(legend.position="none")

# ### si besoin : export du tableau des moy et et par cluster
# write.table(profils, file="MoyTypo14.csv",
#             sep=";",dec=",",row.names=F)

## Tableau final de description des clusters

### Construction du tableau final des % (à faire ensuite : ET) avec gt [trouver un moyen de calculer facilement la moyenne nationale]

#### Créer un tableau en ajoutant  une colonne avec le nom complet des classes
tab_clust <- tab_clust %>%
  mutate(nom_classes = recode(classes,
                      "1" = "1.pub1",
                      "2" = "2.pub2",
                      "3" = "3.pub-indu",
                      "4" = "4.pub-indu2",
                      "5" = "5.tertiaire_sup",
                      "6" = "6.comm",
                      "7" = "7.servprox", 
                      "8" = "8.servproxpub",
                      "9" = "9.articare",
                      "10" = "10.agriarticare", 
                      "11" = "11.induarti", 
                      "12" = "12.ZAE1", 
                      "13" = "13.ZAE2"))

Tab1 <- select(tab_clust, nom_classes, nb, BS18_S_T, starts_with("P_T_"))

#### renommer les colonnes

colnames(Tab1)[1:22] <- c("Classes", "Nombre", "emplois totaux", "cadres public", "cadres privé", "prof int pub", 
                          "prof int entreprises", "Techniciens contremaîtres", "Empl admin", "Empl commerce", "Policiers",
                          "Empl public" , "Empl sante", "Empl care", "Empl vente", "Gardiens et autres empl",
                          "Ouv indu qualif", "Ouv arti qualif", "Ouv indu peu qualif", "Ouv arti peu qualif", "Ouv transp log", "Ouv agri")
Tab1 <- Tab1[order(Tab1$Classes),]
# 
# ## si besoin, pour finir le tableau sur excell
# write.table(Tab1, file="Tab1.csv",
#             sep=";",dec=",",row.names=F)

#### mise en page du tableau avec les grandes catégories d'individus et de variables et les moyennes

Tab1 <- gt(Tab1, rowname_col = "Classes") %>%
  tab_header(
    title = "Profils des classes (% des types d'emploi sur le total des emplois au lieu de travail par classe)",
    subtitle = "selon une CAH en 13 classes") %>% 
  tab_source_note("Source: Subwork database, 2023 - https://nakala.fr/10.34847/nkl.c8caljc9") %>% 
  grand_summary_rows (
    columns = 1:2,
    fns = list(label = "Total", fn = "sum"))  %>% 
  grand_summary_rows (
    columns = 3:22,
    fns = list(label = "Total", fn = "mean"))  %>% 
  fmt_number (columns=1:2,decimals = 0) %>%
  fmt_number (columns=3:22, decimals = 1) %>%
  tab_spanner(
    label = "Parts des cadres",
    columns = c("cadres public", "cadres privé")) %>%
  tab_spanner(
    label = "Parts des prof. interm",
    columns = c("prof int pub", "prof int entreprises", "Techniciens contremaîtres")) %>%
  tab_spanner(
    label = "Parts des employé-es",
    columns = c("Empl admin", "Empl commerce", "Policiers","Empl public", "Empl sante", "Empl care", "Empl vente", "Gardiens et autres empl")) %>%
  tab_spanner(
    label = "Parts des ouvrier-es",
    columns = c("Ouv indu qualif", "Ouv arti qualif", "Ouv indu peu qualif", "Ouv arti peu qualif", "Ouv transp log", "Ouv agri"))
   
Tab1
Tab1 %>% gtsave("tab_1.docx")
Tab1 %>% gtsave("tab_1.html")

# Carto mf_map VF : 1 carte avec toute la typologie puis 1 carte par catégorie de classes----------------------------------------

## Tableau avec seulement classes et organisé en ordre croissant
d.classes <- select(final.d.acp.cah.res, "IRISrD_CODE",  starts_with("P_T"), starts_with("Dim"), classes)
d.classes$classes <- factor(d.classes$classes, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))

## Carte 1 de l'ensemble de la typo avec palette maison, version mf_map

fondata <- fondata %>% left_join(d.classes, by = c("IRISrD_CODE" = "IRISrD_CODE"))

mf_map(x=fondata,  var = "classes", type = "typo",
       pal = c("purple4", "purple2", "darkorange2",  "orange", "darkblue", "dodgerblue2", "yellow", "greenyellow","aquamarine3", "darkolivegreen4", "orangered", "red2", "red4" ),
       border = NA)
mf_map(dep, 
       type = "base", 
       col = "white", 
       lwd = 1,5, 
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)

## Carte 2 des centralités publiques

### filtre sur classes avec les seules types centralités publiques

d.classes_centra_pub <- filter(d.classes, classes =="1" | classes =="2" | classes =="3" | classes =="4" )
profils_centra_pub <- filter(profils, classes =="1" | classes =="2" | classes =="3" | classes =="4" )

### carto

fondata <- fondata %>% left_join(d.classes_centra_pub, by = c("IRISrD_CODE" = "IRISrD_CODE"))

mf_map(x=fondata,  var = "classes", type = "typo", 
       pal = c("purple4", "purple2", "darkorange2",  "orange"), 
       col_na = "grey", 
       border = NA)
mf_map(dep, 
       col = "white", 
       lwd = 1, 5, 
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)

### graphes des profils des types centralités publiques

profils_centra_pub  %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=classes),stat="identity")+  
  scale_fill_manual(values = c("purple4", "purple2", "darkorange2",  "orange")) +
  coord_flip()+  
  facet_wrap(~classes) +   
  theme(legend.position="none")

## Carté 3 des centralités tertiaires et commerciales

### filtre sur d.classes avec les seules types centralités tertiaires

d.classes_centra_tert <- filter(d.classes, classes =="5" | classes =="6")
profils_centra_tert <- filter(profils, classes =="5" | classes =="6")

### carto

fondata <- fondata %>% left_join(d.classes_centra_tert, by = c("IRISrD_CODE" = "IRISrD_CODE"))

mf_map(x=fondata,  var = "classes", type = "typo",
       pal = c("darkblue", "dodgerblue3"),
       col_na = "grey",
       border = NA)
mf_map(dep, 
       col = "white",
       lwd = 1,5,
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


### graphes des profils des types centralités tertiaires

profils_centra_tert %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=classes),stat="identity")+  
  scale_fill_manual(values = c("darkblue", "dodgerblue3")) +
  coord_flip()+  
  facet_wrap(~classes) +   
  theme(legend.position="none")

## Carte 4 des centralités artisanales, care et agricoles

### filtre sur d.classes avec les seules types artisanales, care et agricoles

d.classes_centra_com <- filter(d.classes, classes =="7" | classes =="8" | classes =="9" | classes =="10" )
profils_centra_com <- filter(profils, classes =="7" | classes =="8" | classes =="9" | classes =="10" )

### carto

fondata <- fondata %>% left_join(d.classes_centra_com, by = c("IRISrD_CODE" = "IRISrD_CODE"))

mf_map(x=fondata,  var = "classes", type = "typo",
       pal = c("yellow", "greenyellow","aquamarine3", "darkolivegreen4"),
       col_na = "grey",
       border = NA)
mf_map(dep, 
       col = "white",
       lwd = 1,5,
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)

### graphes des profils des types centralités artisanales, care et agricoles

profils_centra_com  %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=classes),stat="identity")+  
  scale_fill_manual(values = c("yellow", "greenyellow","aquamarine3", "darkolivegreen4")) +
  coord_flip()+  
  facet_wrap(~classes) +   
  theme(legend.position="none")

## Carte 5 des espaces productifs

### filtre sur classes avec les seules types espaces productifs

d.classes_esp_prod <- filter(d.classes, classes =="11" | classes =="12" | classes =="13")
profils_esp_prod <- filter(profils, classes =="11" | classes =="12" | classes =="13")


### carto

fondata <- fondata %>% left_join(d.classes_esp_prod, by = c("IRISrD_CODE" = "IRISrD_CODE"))

mf_map(x=fondata,  var = "classes", type = "typo",
       pal = c("orangered", "red2", "red4"),
       col_na = "grey",
       border = NA)
mf_map(dep, 
       col = "white",
       lwd = 1,
       add = TRUE)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


### graphes des profils des types espaces productifs

profils_esp_prod  %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=classes),stat="identity")+  
  scale_fill_manual(values = c("orangered", "red2", "red4")) +
  coord_flip()+  
  facet_wrap(~classes) +   
  theme(legend.position="none")

#######################################################################################
####### ANNEXE 1 : construction progressive de la typo CAH de 3 à 16 clusters##########

#CAH à 3 clusters sur l’ACP sur les 7 premières dimensions pour mieux comprendre la construction du tableau (= découpe automatique proposé par HCPC)

res.cah3 <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = 3, graph = F)

d.acp.cah.res3 <- d.acp.res |> 
  left_join(as.data.frame(res.cah3$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

summary(d.acp.cah.res3$clust)

## Dendogramme : version de base ==> toujours dommage de ne pas visualiser les numéros des classes...

plot(res.cah3, choice = "tree", ind.names=F)
plot(res.cah3, choice="bar") 
plot(res.cah3, choice="3D.map", ind.names=F)

## Classes sur le plan factoriel : dim 1,2,3,4,5.
plot(res.cah3, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah3, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)
plot(res.cah3, choice = "map", axes = c(1,5), ind.names = F, draw.tree = F)

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
tab.res3 <- left_join(d.acp.cah.res3, taba, by = "IRISrD_CODE")

## agrégation par cluster version clust

tab_clust <- tab.res3  %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=3, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")


#CAH à 5 clusters sur l’ACP sur les 7 premières dimensions pour mieux comprendre la construction du tableau

res.cah5 <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = 5, graph = F)

d.acp.cah.res5 <- d.acp.res |> 
  left_join(as.data.frame(res.cah5$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

summary(d.acp.cah.res5$clust)

## Dendogramme : version de base ==> toujours dommage de ne pas visualiser les numéros des classes...

plot(res.cah5, choice = "tree", ind.names=F)
plot(res.cah5, choice="3D.map", ind.names=F)

## Classes sur le plan factoriel : dim 1,2,3,4,5.
plot(res.cah5, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah5, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)
plot(res.cah5, choice = "map", axes = c(1,5), ind.names = F, draw.tree = F)

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
tab.res5 <- left_join(d.acp.cah.res5, taba, by = "IRISrD_CODE")

## agrégation par cluster version clust

tab_clust <- tab.res5  %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=5, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")

## CAH à 7 clusters sur l’ACP sur les 7 premières dimensions
n.clust <- 7
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

summary(d.acp.cah.res$clust)

plot(res.cah, choice = "tree", ind.names=F)
plot(res.cah, choice="3D.map", ind.names=F)

## Classes sur le plan factoriel : dim 1 et  2
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## agrégation par cluster version clust

tab_clust <- final.d.acp.cah.res %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=7, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")

## CAH à 8 clusters sur l’ACP sur les 7 premières dimensions : on passe à 2 types public

## CAH à 9 clusters sur l’ACP sur les 7 premières dimensions : on passe à 2 types public

n.clust <- 9
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

summary(d.acp.cah.res$clust)

plot(res.cah, choice = "tree", ind.names=F)

## Classes sur le plan factoriel : dim 1 et  2
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## agrégation par cluster version clust

tab_clust <- final.d.acp.cah.res %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=9, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")


## CAH à 10 clusters sur l’ACP sur les 7 premières dimensions : on passe à 2 types public

n.clust <- 10
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

summary(d.acp.cah.res$clust)

plot(res.cah, choice = "tree", ind.names=F)

## Classes sur le plan factoriel : dim 1 et  2
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## agrégation par cluster version clust

tab_clust <- final.d.acp.cah.res %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=10, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")


## CAH A 11 clusters

n.clust <- 11
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

### graphe des gains gains d'inertie
plot(res.cah, choice="bar") 
# Note : 12 classes semblent un bon compromis
# (13 : un type "commerce en plus ; 14 : un type "tertiaire" en plus ; 15 : un type "artisan" en plus ; 16 : un type "agricare" en plus)

## Dendogramme : version de base ==> toujours dommage de ne pas visualiser les numéros des classes...
## Note : Version fviz ne fonctionne pas, alors que cela permettrait de gérer les couleurs pour retrouver les classes...)

plot(res.cah, choice = "tree", ind.names=F)
plot(res.cah, choice="3D.map", ind.names=F) # illisible

## Nombre d'individus par clusters
summary(d.acp.cah.res$clust)
# 1    2    3    4    5    6    7    8    9   10   11 
# 1091  512 1528 2342 3497 3842 1318 2110 1422 1529 1351 

## Classes sur le plan factoriel : dim 1,2,3,4,5.
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(1,5), ind.names = F, draw.tree = F)

## Analyse des résultats bruts directement sur les variables, les dimensions, les individus

res.cah$desc.var

res.cah$desc.axes$quanti

res.cah$desc.ind$para

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## construction d'un tableau de description des clusters avec leur numéro provisoire

### agrégation par cluster version clust

tab_clust <- final.d.acp.cah.res %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=11, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")

## CAH A 12 clusters = un articare en plus
## CAH A 13 clusters = un indus en plus = CHOIX FINAL

n.clust <- 13
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")

### graphe des gains gains d'inertie
plot(res.cah, choice="bar") 
# Note : 12 classes semblent un bon compromis
# (13 : un type "commerce en plus ; 14 : un type "tertiaire" en plus ; 15 : un type "artisan" en plus ; 16 : un type "agricare" en plus)

## Dendogramme : version de base ==> toujours dommage de ne pas visualiser les numéros des classes...
## Note : Version fviz ne fonctionne pas, alors que cela permettrait de gérer les couleurs pour retrouver les classes...)

plot(res.cah, choice = "tree", ind.names=F)
plot(res.cah, choice="3D.map", ind.names=F) # illisible

## Nombre d'individus par clusters
summary(d.acp.cah.res$clust)
# 1    2    3    4    5    6    7    8    9   10   11 
# 1091  512 1528 2342 3497 3842 1318 2110 1422 1529 1351 

## Classes sur le plan factoriel : dim 1,2,3,4,5.
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(1,5), ind.names = F, draw.tree = F)

## Analyse des résultats bruts directement sur les variables, les dimensions, les individus

res.cah$desc.var

res.cah$desc.axes$quanti

res.cah$desc.ind$para

## construction du tableau avec les valeurs en effectifs

taba <- select (datar,  IRISrD_CODE, starts_with("BS18"))
final.d.acp.cah.res <- left_join(d.acp.cah.res, taba, by = "IRISrD_CODE")

## construction d'un tableau de description des clusters avec leur numéro provisoire

### agrégation par cluster version clust

tab_clust <- final.d.acp.cah.res %>% group_by (clust) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C32.CPubli),
  sum(BS18_S_T_CP2_C36.CPriv),
  sum(BS18_S_T_CP2_C41.PIpub),
  sum(BS18_S_T_CP2_C46.PIent),
  sum(BS18_S_T_CP2_C47.Techn),
  sum(BS18_S_T_CP2_C48.Contr),
  sum(BS18_S_T_CP2_T511.Admin),
  sum(BS18_S_T_CP2_T512.Comm),
  sum(BS18_S_T_CP2_T513.Police),
  sum(BS18_S_T_CP2_T514.Public),
  sum(BS18_S_T_CP2_T515.Sante),
  sum(BS18_S_T_CP2_T521.Care),
  sum(BS18_S_T_CP2_T522.Vente),
  sum(BS18_S_T_CP2_T523.Gard),
  sum(BS18_S_T_CP2_T524.Autres),
  sum(BS18_S_T_CP2_T611.Indus.Q),
  sum(BS18_S_T_CP2_T612.Arti.Q),
  sum(BS18_S_T_CP2_T621.Indus.PQ),
  sum(BS18_S_T_CP2_T622.Arti.PQ),
  sum(BS18_S_T_CP2_T631.Transp),
  sum(BS18_S_T_CP2_T632.Agri),
  nb = n())

variable.names(tab_clust)
colnames(tab_clust)[2:23] <- c("BS18_S_T", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                               "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                               "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                               "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")
relocate (tab_clust, nb, 2)
## Construction des colonnes en parts avec regroupements des CP

tab_clust$P_T_CP2_C32.CPubli <- tab_clust$CP2_C32.CPubli/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C36.CPriv <- tab_clust$CP2_C36.CPriv/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C41.PIpub <- tab_clust$CP2_C41.PIpub/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C46.PIent <- tab_clust$CP2_C46.PIent/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_C47.C48.Techn.Contr <- (tab_clust$CP2_C47.Techn+tab_clust$CP2_C48.Contr)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T511.Admin <- tab_clust$CP2_T511.Admin/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T512.Comm <- tab_clust$CP2_T512.Comm/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T513.Police <- tab_clust$CP2_T513.Police/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T514.Public <- tab_clust$CP2_T514.Public/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T515.Sante <- tab_clust$CP2_T515.Sante/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T521.Care <- tab_clust$CP2_T521.Care/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T522.Vente <- tab_clust$CP2_T522.Vente/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T523.T524.Autres.Gard <- (tab_clust$CP2_T523.Gard+tab_clust$CP2_T524.Autres)/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T611.Indus.Q <- tab_clust$CP2_T611.Indus.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T612.Arti.Q <- tab_clust$CP2_T612.Arti.Q/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T621.Indus.PQ <- tab_clust$CP2_T621.Indus.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T622.Arti.PQ <- tab_clust$CP2_T622.Arti.PQ/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T631.Transp <- tab_clust$CP2_T631.Transp/tab_clust$BS18_S_T*100
tab_clust$P_T_CP2_T632.Agri <- tab_clust$CP2_T632.Agri/tab_clust$BS18_S_T*100

## calcul des variables et ggplot pour le graphe en barre

moy_var_ens <- tab_clust %>% select(-clust) |> 
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- tab_clust %>%   
  pivot_longer (cols=starts_with("P_T_CP2"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust, variable) %>% 
  summarise(moy_clust=mean(valeur),et_clust=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot()+  
  geom_bar(aes(x=variable, y=ecart_moyenne, fill=clust),stat="identity")+  
  scale_fill_manual(values=met.brewer("Troy", n=13, type="continuous")) + #attention nb de n
  coord_flip()+  
  facet_wrap(~clust) +   
  theme(legend.position="none")

## CAH A 14 clusters = 2 agri-arti-care
## CAH A 15 clusters = 2 arti-care
## CAH A 16 clusters = 2 indu-arti

n.clust <- 16
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)
d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrD_CODE") |> 
              select(IRISrD_CODE,clust), by = "IRISrD_CODE")
plot(res.cah, choice = "tree", ind.names=F)


##### Annexe 2

## dendogramme en choisissant les couleurs
# fviz_dend(res.cah, k=14, k_colors = c("darkblue", "dodgerblue3", "skyblue2", "darkorchid1", "darkorchid3", "purple4", "yellow", "orange", "darkorange2", "red1","red3", "red4", "darkolivegreen3", "greenyellow") , show_labels = FALSE)
