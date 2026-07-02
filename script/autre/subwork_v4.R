
#                                                TYPOLOGIE DES LIEUX DE TRAVAIL
#                                     BASE DE DONNEES SUBWORK - NOMENCLATURE CP
#                                EXPLORATIONS ET TESTS SUR DES DONNEES A L'IRIS
#
#                                                                Antoine Beroud
#                                                             Nicolas Raimbault
#                                                                     Aout 2025


# INSTALLATION DES PACKAGES ---------------------------------------------------
# # Installation du package remotes pour permettre l'installation de packages 
# # presents sur GitLab
# install.packages("remotes")
# 
# # Installation du package asf
# remotes::install_gitlab("atlas-social-de-la-france/asf", 
#                         host = "gitlab.huma-num.fr",
#                         build_vignettes = TRUE)  
# 
# install.packages("devtools") # pour les donnees Subwork
# devtools::install_github("alietteroux/subwork")

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


# IMPORT DES DONNEES ----------------------------------------------------------
# Emplois au lieu de travail selon cp2 subwork
# Import des donnees "FT810" a l'IRIS dans un data.frame nomme "FT810.data"
FT810.data <- import(code = "FT810", type = "data")

# Production d'un tableau data.cp2. avec les cp1 des independants (car 
# effectifs detailles des artisants de la CP2 trop petit)

# Source des donnees : base tous salaries que l'on a regroupe a l'IRIS
data.cp2 <- select(FT810.data, IRIS, IRIS_LIB, COM, BS18_S_T, BS18_S_T_CP1_C2.Arti, starts_with("BS18_S_T_CP2"))


# IMPORT DU MAILLAGE ---------------------------------------------------------- 
# Mise a jour juillet 2025 avec nouveau format de la fonction d'Aliette

# Donnees sur le maillage des IRIS regroupees
mar <- asf_mar(md = "iris", ma = "irisr", geom = TRUE)

tabl <- mar$tabl
geom <- mar$geom

datar <- asf_data(data.cp2, 
                  tabl, 
                  by.x = "IRIS", 
                  by.y = "IRIS_CODE", 
                  maille = "IRISrD_CODE", vars = c(4:32), 
                  funs = "sum")

# Verification des sommes calculees
sum(data.cp2$BS18_S_T, na.rm = TRUE) - sum(datar$BS18_S_T, na.rm = TRUE) # les habitantes de Plottes
sum(data$BS18_S_T, na.rm = TRUE) - sum(datar$BS18_S_T, na.rm = TRUE)


# JOINTURE AVEC LE FOND GEOGRAPHIQUE ------------------------------------------
# Agregation des iris en iris regroupes
fond <- asf_fond(geom, 
                 tabl, 
                 by = "IRISF_CODE", 
                 maille = "IRISrD_CODE", 
                 keep = c("IRISrD_LIB", "DEP", "OM_CODE"))

# Repositionnement des DROM
fond <- asf_drom(fond, 
                 id = "IRISrD_CODE")

# # Creation de zooms
# z <- asf_zoom(fond, 
#               places = c("4", "5"), 
#               r = 20000)

# # Creation de zooms
z <- asf_zoom(fond,
              places = c("Paris", "Nantes", "Marseille", "Lyon"), 
              coords = c(5.721, 45.182), labels = "Grenoble", 
              r = 10000)

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

# Creation d'une carte de test
mf_map(fondata, 
       var = "BS18_S_T_CP2_T632.Agri", 
       type = "choro",
       border = NA)
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)


# CALCUL DES TAUX -------------------------------------------------------------

## Analyse des totaux par CP2 ----
sum(data.cp2$BS18_S_T_CP2_C1.Agri, na.rm = TRUE) #3 436
sum(data.cp2$BS18_S_T_CP2_C21.Arti, na.rm = TRUE)#7 761
sum(data.cp2$BS18_S_T_CP2_C22.Comm, na.rm = TRUE)#43 303
sum(data.cp2$BS18_S_T_CP2_C23.Chefs, na.rm = TRUE) #102 265. Hypothèse : des chefs sont artisans ou commerçants ?
sum(data.cp2$BS18_S_T_CP2_C31.PLib, na.rm = TRUE) #39 426
sum(data.cp2$BS18_S_T_CP2_C32.CPubli, na.rm = TRUE)#1 413 875
sum(data.cp2$BS18_S_T_CP2_C36.PLib, na.rm = TRUE) #3 175 419
sum(data.cp2$BS18_S_T_CP2_C41.PIpub, na.rm = TRUE) #2 548 734
sum(data.cp2$BS18_S_T_CP2_C46.PIent, na.rm = TRUE) #1 471 121
sum(data.cp2$BS18_S_T_CP2_C47.Techn, na.rm = TRUE) #1 059 321
sum(data.cp2$BS18_S_T_CP2_C48.Contr, na.rm = TRUE) #400 401 (à regrouper au-dessus)
sum(data.cp2$BS18_S_T_CP2_T511.Admin, na.rm = TRUE) #1 891 564
sum(data.cp2$BS18_S_T_CP2_T512.Comm, na.rm = TRUE) #1 891 564
sum(data.cp2$BS18_S_T_CP2_T513.Police, na.rm = TRUE) #882 294 (à regrouper en-dessous)
sum(data.cp2$BS18_S_T_CP2_T514.Public, na.rm = TRUE) #796 630 (à regrouper en-dessous)
sum(data.cp2$BS18_S_T_CP2_T515.Sante, na.rm = TRUE) #751 400 
sum(data.cp2$BS18_S_T_CP2_T521.Care, na.rm = TRUE) #779 248
sum(data.cp2$BS18_S_T_CP2_T522.Vente, na.rm = TRUE) #1 979 965
sum(data.cp2$BS18_S_T_CP2_T523.Gard, na.rm = TRUE) #185 730 (à regrouper avec care)
sum(data.cp2$BS18_S_T_CP2_T524.Autres, na.rm = TRUE) #1 148 266
sum(data.cp2$BS18_S_T_CP2_T611.Indus.Q, na.rm = TRUE) #2 136 630
sum(data.cp2$BS18_S_T_CP2_T612.Arti.Q, na.rm = TRUE) #1 524 216
sum(data.cp2$BS18_S_T_CP2_T621.Indus.PQ, na.rm = TRUE) # 690 468
sum(data.cp2$BS18_S_T_CP2_T622.Arti.PQ, na.rm = TRUE) #885 605
sum(data.cp2$BS18_S_T_CP2_T631.Transp, na.rm = TRUE) #1 855 977
sum(data.cp2$BS18_S_T_CP2_T632.Agri, na.rm = TRUE) #338 082

# Construction des colonnes en parts avec regroupements de certaines catégories CP2 dans le tableau datar (datar ci-dessous)
datar$P_T_CP2_C1.C2.Agri.Arti <- (datar$BS18_S_T_CP2_C1.Agri+datar$BS18_S_T_CP1_C2.Arti)/datar$BS18_S_T*100
datar$P_T_CP2_C31.PLib <- datar$BS18_S_T_CP2_C31.PLib/datar$BS18_S_T*100
datar$P_T_CP2_C36.CPriv <- datar$BS18_S_T_CP2_C36.CPriv/datar$BS18_S_T*100
datar$P_T_CP2_C32.CPubli <- datar$BS18_S_T_CP2_C32.CPubli/datar$BS18_S_T*100
datar$P_T_CP2_C41.PIpub <- datar$BS18_S_T_CP2_C41.PIpub/datar$BS18_S_T*100
datar$P_T_CP2_C46.PIent <- datar$BS18_S_T_CP2_C46.PIent/datar$BS18_S_T*100
datar$P_T_CP2_C47.C48.Techn.Contr <- (datar$BS18_S_T_CP2_C47.Techn+datar$BS18_S_T_CP2_C48.Contr)/datar$BS18_S_T*100
datar$P_T_CP2_T511.Admin <- datar$BS18_S_T_CP2_T511.Admin/datar$BS18_S_T*100
datar$P_T_CP2_T512.Comm <- datar$BS18_S_T_CP2_T512.Comm/datar$BS18_S_T*100
datar$P_T_CP2_T513.Police <- datar$BS18_S_T_CP2_T513.Police/datar$BS18_S_T*100
datar$P_T_CP2_T514.T515.Public.Sante <- (datar$BS18_S_T_CP2_T514.Public+datar$BS18_S_T_CP2_T515.Sante)/datar$BS18_S_T*100
datar$P_T_CP2_T521.Care <- datar$BS18_S_T_CP2_T521.Care/datar$BS18_S_T*100
datar$P_T_CP2_T522.Vente <- datar$BS18_S_T_CP2_T522.Vente/datar$BS18_S_T*100
datar$P_T_CP2_T523.T524.Autres.Gard <- (datar$BS18_S_T_CP2_T523.Gard+datar$BS18_S_T_CP2_T524.Autres)/datar$BS18_S_T*100
datar$P_T_CP2_T611.Indus.Q <- datar$BS18_S_T_CP2_T611.Indus.Q/datar$BS18_S_T*100
datar$P_T_CP2_T612.Arti.Q <- datar$BS18_S_T_CP2_T612.Arti.Q/datar$BS18_S_T*100
datar$P_T_CP2_T621.Indus.PQ <- datar$BS18_S_T_CP2_T621.Indus.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T622.Arti.PQ <- datar$BS18_S_T_CP2_T622.Arti.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T631.Transp <- datar$BS18_S_T_CP2_T631.Transp/datar$BS18_S_T*100
datar$P_T_CP2_T632.Agri <- datar$BS18_S_T_CP2_T632.Agri/datar$BS18_S_T*100

variable.names(datar)
summary(datar)

datar$verif.P <- datar$P_T_CP2_C1.C2.Agri.Arti + datar$P_T_CP2_C31.PLib + datar$P_T_CP2_C36.CPriv + datar$P_T_CP2_C32.CPubli + datar$P_T_CP2_C41.PIpub + datar$P_T_CP2_C46.PIent +
  datar$P_T_CP2_C47.C48.Techn.Contr + datar$P_T_CP2_T511.Admin + datar$P_T_CP2_T512.Comm + datar$P_T_CP2_T513.Police + datar$P_T_CP2_T514.T515.Public.Sante +
  datar$P_T_CP2_T521.Care + datar$P_T_CP2_T522.Vente + datar$P_T_CP2_T523.T524.Autres.Gard + datar$P_T_CP2_T611.Indus.Q + datar$P_T_CP2_T612.Arti.Q + datar$P_T_CP2_T621.Indus.PQ +
  datar$P_T_CP2_T622.Arti.PQ + datar$P_T_CP2_T631.Transp + datar$P_T_CP2_T632.Agri

summary(datar$verif.P) # verif ok car pas d'IS > 100 Mais 2 NA qui correspondent à des valeurs 0

# Construction des colonnes en parts avec regroupements de certaines catégories CP2 dans le tableau datar
datar$P_T_CP2_C1.C2.Agri.Arti <- (datar$BS18_S_T_CP2_C1.Agri+datar$BS18_S_T_CP1_C2.Arti)/datar$BS18_S_T*100
datar$P_T_CP2_C31.PLib <- datar$BS18_S_T_CP2_C31.PLib/datar$BS18_S_T*100
datar$P_T_CP2_C36.CPriv <- datar$BS18_S_T_CP2_C36.CPriv/datar$BS18_S_T*100
datar$P_T_CP2_C32.CPubli <- datar$BS18_S_T_CP2_C32.CPubli/datar$BS18_S_T*100
datar$P_T_CP2_C41.PIpub <- datar$BS18_S_T_CP2_C41.PIpub/datar$BS18_S_T*100
datar$P_T_CP2_C46.PIent <- datar$BS18_S_T_CP2_C46.PIent/datar$BS18_S_T*100
datar$P_T_CP2_C47.C48.Techn.Contr <- (datar$BS18_S_T_CP2_C47.Techn+datar$BS18_S_T_CP2_C48.Contr)/datar$BS18_S_T*100
datar$P_T_CP2_T511.Admin <- datar$BS18_S_T_CP2_T511.Admin/datar$BS18_S_T*100
datar$P_T_CP2_T512.Comm <- datar$BS18_S_T_CP2_T512.Comm/datar$BS18_S_T*100
datar$P_T_CP2_T513.Police <- datar$BS18_S_T_CP2_T513.Police/datar$BS18_S_T*100
datar$P_T_CP2_T514.T515.Public.Sante <- (datar$BS18_S_T_CP2_T514.Public+datar$BS18_S_T_CP2_T515.Sante)/datar$BS18_S_T*100
datar$P_T_CP2_T521.Care <- datar$BS18_S_T_CP2_T521.Care/datar$BS18_S_T*100
datar$P_T_CP2_T522.Vente <- datar$BS18_S_T_CP2_T522.Vente/datar$BS18_S_T*100
datar$P_T_CP2_T523.T524.Autres.Gard <- (datar$BS18_S_T_CP2_T523.Gard+datar$BS18_S_T_CP2_T524.Autres)/datar$BS18_S_T*100
datar$P_T_CP2_T611.Indus.Q <- datar$BS18_S_T_CP2_T611.Indus.Q/datar$BS18_S_T*100
datar$P_T_CP2_T612.Arti.Q <- datar$BS18_S_T_CP2_T612.Arti.Q/datar$BS18_S_T*100
datar$P_T_CP2_T621.Indus.PQ <- datar$BS18_S_T_CP2_T621.Indus.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T622.Arti.PQ <- datar$BS18_S_T_CP2_T622.Arti.PQ/datar$BS18_S_T*100
datar$P_T_CP2_T631.Transp <- datar$BS18_S_T_CP2_T631.Transp/datar$BS18_S_T*100
datar$P_T_CP2_T632.Agri <- datar$BS18_S_T_CP2_T632.Agri/datar$BS18_S_T*100

variable.names(datar)
summary(datar)

datar$verif.P <- datar$P_T_CP2_C1.C2.Agri.Arti + datar$P_T_CP2_C31.PLib + datar$P_T_CP2_C36.CPriv + datar$P_T_CP2_C32.CPubli + datar$P_T_CP2_C41.PIpub + datar$P_T_CP2_C46.PIent +
  datar$P_T_CP2_C47.C48.Techn.Contr + datar$P_T_CP2_T511.Admin + datar$P_T_CP2_T512.Comm + datar$P_T_CP2_T513.Police + datar$P_T_CP2_T514.T515.Public.Sante +
  datar$P_T_CP2_T521.Care + datar$P_T_CP2_T522.Vente + datar$P_T_CP2_T523.T524.Autres.Gard + datar$P_T_CP2_T611.Indus.Q + datar$P_T_CP2_T612.Arti.Q + datar$P_T_CP2_T621.Indus.PQ +
  datar$P_T_CP2_T622.Arti.PQ + datar$P_T_CP2_T631.Transp + datar$P_T_CP2_T632.Agri

summary(datar$verif.P) # verif ok car pas d'IS > 100 Mais 2 NA qui correspondent à des valeurs 0



# ACP CP2 ---------------------------------------------------------------------

## Tableau ACP a partir de datar ----
acp.cp2 <- select(datar,  IRISrD_CODE, starts_with("P_T_CP2")) |> 
  column_to_rownames(var = "IRISrD_CODE")

# note : 2 IRIS avec valeur NA : 940330111 (Fontenay sous bois) et 570320103 (?)
## version code Jean
res.acp <- PCA(acp.cp2, graph=F)

d.acp.res <- acp.cp2 |> rownames_to_column(var = "IRISrD_CODE") |> 
  left_join(as.data.frame(res.acp$ind$coord) |> 
              rownames_to_column(var = "IRISrS_CODE"), by = "IRISrS_CODE")

# % d’inertie expliquée par chaque axe
fviz_eig(res.acp, addlabels = TRUE)

# Plan factoriel des variables
fviz_pca_var(res.acp, col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

fviz_pca_var(res.acp, axes=c(3,4), col.var = "contrib",
             gradient.cols = c("white", "blue", "red"),
             ggtheme = theme_minimal())

# Outil de mise en forme si besoin
explor(res.acp)

# Coordonnées sur les axes 1, 2 et 3 : à revoir
fondata |> filter(OM_CODE == "FXX") |>
  left_join(d.acp.res, by = c("IRISrS_CODE" = "IRISrS_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.1), col = NA) +
  scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white",
                       midpoint = 0) + theme_void()

fondata |> filter(OM_CODE == "FXX") |> 
  left_join(d.acp.res, by = c("IRISrS_CODE" = "IRISrS_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.2), col = NA) +
  scale_fill_gradient2(low = "#2874a6", high = "#ec7063", mid = "white",
                       midpoint = 0) + theme_void()

fondata |> filter(OM_CODE == "FXX") |> 
  left_join(d.acp.res, by = c("IRISrS_CODE" = "IRISrS_CODE")) |> 
  ggplot() + geom_sf(aes(fill = Dim.4), col = NA) +
  scale_fill_gradient2(low = "blue", high = "darkred", mid = "white",
                       midpoint = 0) + theme_void()


# CAH à 14 clusters sur l’ACP sur les 7 premières dimensions
n.clust <- 14
res.cah <- HCPC(res.acp, ncp = 7, consol = F, nb.clust = n.clust, graph = F)

d.acp.cah.res <- d.acp.res |> 
  left_join(as.data.frame(res.cah$data.clust) |> 
              rownames_to_column(var = "IRISrS_CODE") |> 
              select(IRISrS_CODE,clust), by = "IRISrS_CODE")
variable.names(d.acp.cah.res)
summary(d.acp.cah.res$clust)

# Dendogramme 
plot(res.cah, choice = "tree", ind.names=F)

# Classes sur le plan factoriel :
plot(res.cah, choice = "map", ind.names = F, draw.tree = F)
plot(res.cah, choice = "map", axes = c(3,4), ind.names = F, draw.tree = F)

# Créer une colonne avec la renumérotation des clusters : vérifier la renomérotation des clust
d.acp.cah.res <- d.acp.cah.res %>%
  mutate(clust2 = recode(clust,
                         "1" = "2",
                         "2" = "14",
                         "3" = "7",
                         "4" = "8",
                         "5" = "5",
                         "6" = "9",
                         "7" = "12",
                         "8" = "10", 
                         "9" = "3",
                         "10" = "11", 
                         "11" = "13",
                         "12" = "6", 
                         "13" = "4", 
                         "14" = "1")) %>%
        arrange(d.acp.cah.res, clust2)

# Tableau avec seulement clust2 et organisé en ordre croissant
d.clust2 <- select(d.acp.cah.res, "IRISrS_CODE",  starts_with("P_T"), starts_with("Dim"), clust2)
d.clust2$clust2 <- factor(d.clust2$clust2, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))

# Carto des classes avec palette MET
fondata |> left_join(d.acp.cah.res, by = c("IRISrS_CODE" = "IRISrS_CODE")) |>
  ggplot() + geom_sf(aes(fill = clust), color = NA) +
  scale_fill_manual(values = met.brewer("Troy", n = 14, type = "continuous")) +
  theme_void()

# Carto des classes avec palette Jean Rivière (pb les clust2 ne sont pas dans l'ordre...)
fondata |> left_join(d.clust2, by = c("IRISrS_CODE" = "IRISrS_CODE")) |>
  ggplot() + geom_sf(aes(fill = clust2), color = NA) +
  # scale_fill_manual(values=c("red4", "red2", "darkorange2", "orange", "yellow", "greenyellow","aquamarine3", "paleturquoise", "skyblue2", "darkolivegreen4", "dodgerblue3", "darkolivegreen2", "darkorchid1", "purple4"))+ theme_void()
  scale_fill_manual(values = c("red4", "red2", "darkorange2", "orange", "yellow", "greenyellow" ,"aquamarine3", "darkolivegreen2" , "skyblue2", "dodgerblue3", "darkblue", "darkorchid1", "purple4",  "darkolivegreen4")) +  theme_void()


# Profils des classes : cela ne marche plus avec mes clust2... (attention à adapter entre x_agreg_eff et y_agreg_eff)

## Creation d'un tableau des individus avec toutes les variables et les lib, qui me servira aussi pour l'export
x_agreg_eff <- select (datar,  IRISrS_CODE, starts_with("BS18"))
final.d.clust2 <- left_join(d.clust2, x_agreg_eff, by = "IRISrS_CODE")
tabl_lib <- select(tabl, "IRISrS_CODE","IRISrS_LIB" ) # pour récupérer les libelles de la table d'Aliette
final.d.clust2 <- inner_join(final.d.clust2, tabl_lib, by = "IRISrS_CODE") #pb de nombre d'individus statistique à la fin

# Export du tableau des individus avec les clusters, les dimensions, les lib, les parts et les eff par variable
write.table(
  final.d.clust2,
  file = "Table_Indiv_Clust2_14.csv",
  sep = ";",
  dec = ",",
  row.names = F
)

## code pour le profil ET
moy_var_ens <- final.d.clust2 %>% select(-clust2) |> 
  pivot_longer (cols=starts_with("BS18"), 
                names_to = "variable", values_to = "valeur" ) %>%  
  group_by(variable) %>% 
  summarise(moy_ens=mean(valeur),et_ens=sd(valeur),.groups = "drop")

profils <- final.d.clust2  %>%   
  pivot_longer (cols=starts_with("BS18"), 
                names_to="variable", values_to="valeur") %>% 
  group_by(clust2, variable) %>% 
  summarise(moy_clust2=mean(valeur),et_clust2=sd(valeur),.groups = 'drop') %>% 
  inner_join(moy_var_ens,by="variable") %>% 
  mutate(ecart_moyenne=(moy_clust2-moy_ens)/et_ens)
rm(moy_var_ens)

profils %>% ggplot() +
  geom_bar(aes(x = variable, y = ecart_moyenne, fill = clust2), stat = "identity") +  
  # scale_fill_manual(values=met.brewer("Troy", n=14, type="continuous")) +
  # scale_fill_manual(values=c("red4", "red2", "darkorange2", "orange", "yellow", "greenyellow","aquamarine3", "paleturquoise", "skyblue2", "darkolivegreen4", "dodgerblue3", "darkolivegreen2", "darkorchid1", "purple4") )+
  scale_fill_manual(values=c("red4", "red2", "darkorange2", "orange", "yellow", "greenyellow" ,"aquamarine3", "darkolivegreen2" , "skyblue2", "dodgerblue3", "darkblue", "darkorchid1", "purple4",  "darkolivegreen4")) + theme_void() +
  coord_flip() +
  facet_wrap(~ clust2) +
  theme(legend.position = "none")


# Tableau de description des clusters
variable.names(final.d.clust2)

tab <- final.d.clust2 %>% group_by (clust2) %>% summarise (
  sum(BS18_S_T),
  sum(BS18_S_T_CP2_C1.Agri),
  sum(BS18_S_T_CP1_C2.Arti),
  sum(BS18_S_T_CP2_C31.PLib),
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
  nb = n()
)
variable.names(tab)
colnames(tab)[2:26] <- c("BS18_S_T", "CP2_C1.Agri", "CP1_C2.Arti", "CP2_C31.PLib", "CP2_C32.CPubli", "CP2_C36.CPriv", "CP2_C41.PIpub", 
                         "CP2_C46.PIent", "CP2_C47.Techn", "CP2_C48.Contr", "CP2_T511.Admin", "CP2_T512.Comm", "CP2_T513.Police",
                         "CP2_T514.Public", "CP2_T515.Sante", "CP2_T521.Care", "CP2_T522.Vente", "CP2_T523.Gard", "CP2_T524.Autres",
                         "CP2_T611.Indus.Q", "CP2_T612.Arti.Q", "CP2_T621.Indus.PQ", "CP2_T622.Arti.PQ", "CP2_T631.Transp", "CP2_T632.Agri")

# tableau final avec les parts puis mise en page avec totaux

# Construction des colonnes en parts avec regroupements
tab$P_T_CP2_C1.C2.Agri.Arti <- (tab$CP2_C1.Agri+tab$CP1_C2.Arti)/tab$BS18_S_T*100
tab$P_T_CP2_C31.PLib <- tab$CP2_C31.PLib/tab$BS18_S_T*100
tab$P_T_CP2_C32.CPubli <- tab$CP2_C32.CPubli/tab$BS18_S_T*100
tab$P_T_CP2_C36.CPriv <- tab$CP2_C36.CPriv/tab$BS18_S_T*100
tab$P_T_CP2_C41.PIpub <- tab$CP2_C41.PIpub/tab$BS18_S_T*100
tab$P_T_CP2_C46.PIent <- tab$CP2_C46.PIent/tab$BS18_S_T*100
tab$P_T_CP2_C47.C48.Techn.Contr <- (tab$CP2_C47.Techn+tab$CP2_C48.Contr)/tab$BS18_S_T*100
tab$P_T_CP2_T511.Admin <- tab$CP2_T511.Admin/tab$BS18_S_T*100
tab$P_T_CP2_T512.Comm <- tab$CP2_T512.Comm/tab$BS18_S_T*100
tab$P_T_CP2_T513.Police <- tab$CP2_T513.Police/tab$BS18_S_T*100
tab$P_T_CP2_T514.T515.Public.Sante <- (tab$CP2_T514.Public+tab$CP2_T515.Sante)/tab$BS18_S_T*100
tab$P_T_CP2_T521.Care <- tab$CP2_T521.Care/tab$BS18_S_T*100
tab$P_T_CP2_T522.Vente <- tab$CP2_T522.Vente/tab$BS18_S_T*100
tab$P_T_CP2_T523.T524.Autres.Gard <- (tab$CP2_T523.Gard+tab$CP2_T524.Autres)/tab$BS18_S_T*100
tab$P_T_CP2_T611.Indus.Q <- tab$CP2_T611.Indus.Q/tab$BS18_S_T*100
tab$P_T_CP2_T612.Arti.Q <- tab$CP2_T612.Arti.Q/tab$BS18_S_T*100
tab$P_T_CP2_T621.Indus.PQ <- tab$CP2_T621.Indus.PQ/tab$BS18_S_T*100
tab$P_T_CP2_T622.Arti.PQ <- tab$CP2_T622.Arti.PQ/tab$BS18_S_T*100
tab$P_T_CP2_T631.Transp <- tab$CP2_T631.Transp/tab$BS18_S_T*100
tab$P_T_CP2_T632.Agri <- tab$CP2_T632.Agri/tab$BS18_S_T*100

head(tab)

# Construction du tableau final des % (à faire ensuite : ET) avec gt [trouver un moyen de calculer facilement la moyenne nationale]
Tab1 <- select(tab, clust2, starts_with("P_T_"))

Tab1 <- gt(Tab1, rowname_col = "clust2") %>%
  tab_header(
    title = "Profils des classes",
    subtitle = "selon une CAH en 14 classes") %>% 
  tab_source_note("Source: Subwork database, 2023 - https://nakala.fr/10.34847/nkl.c8caljc9") %>% 
  grand_summary_rows (
    columns = 1:21,
    fns = list(label = "Total", fn = "mean"))  %>% 
  fmt_number (columns=1:21, decimals = 1)

Tab1
Tab1 %>% gtsave("tab_1.docx")


