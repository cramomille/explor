
#                                EXPLORATIONS ET TESTS SUR DES DONNEES A L'IRIS
#
#                                                                antoine beroud
#                                                             nicolas raimbault
#                                                                    avril 2025


###############################################################################
################################################### INSTALLATION DU PACKAGE ASF

# Installation du package remotes pour permettre l'installation de packages sur GitLab
# install.packages("remotes")

# Installation du package asf
# remotes::install_gitlab("atlas-social-de-la-france/asf", host = "gitlab.huma-num.fr")

# Lancement des packages utilises dans le code
library(sf)
library(mapsf)
library(asf)


###############################################################################
############################################################ IMPORT DES DONNEES

# install.packages("devtools")
# devtools::install_github("alietteroux/subwork")

library(subwork)

# Import des donnees d'interet
FT810.data <- import(code = "FT810", type = "data")
FT711.data <- import(code = "FT711", type = "data")

# Definition du tableau de donnees et des colonnes
data <- FT810.data
cols <- "F_R_CP1"

data <- data[, c(1:3, grep(cols, names(data)))]


###############################################################################
######################################################### ID MAILLAGE COMPOSITE

# Import des donnees d'Aliette a partir du package asf
mar <- asf_mar(sf = FALSE)

# Code des iris de reference
tabl <- mar$ar01$d.irisf.pass
dataf <- merge(tabl[, c("IRIS_CODE", "IRISF_CODE")], data, 
               by.x = "IRIS_CODE", by.y = "IRIS")

# Verification du nombre d'iris differentes
sum(length(unique(data$IRIS)))
sum(length(unique(dataf$IRIS_CODE)))

# Code des iris regroupes pour une analyse synchronique
tabl <- mar$ar02$d.irisr.pass
datar <- merge(tabl[, c("IRISF_CODE", "IRISrS_CODE", "IRISrS_LIB")], dataf, 
               by = "IRISF_CODE")

# Verification du nombre d'iris differentes
sum(length(unique(dataf$IRIS_CODE)))
sum(length(unique(datar$IRIS_CODE)))

table(duplicated(dataf$IRIS_CODE))
table(duplicated(datar$IRIS_CODE))

# Suppression des doublons
datar <- datar[!duplicated(datar$IRIS_CODE), ]

###############################################################################
######################################################## AGREGATION DES DONNEES

# Agregation simple
sum <- rowsum(datar[, c(7:24)], group = datar$IRISrS_CODE, na.rm = TRUE)
x_aggreg <- data.frame(IRISrS_CODE = rownames(sum), sum, row.names = NULL)

# Agregation avec le package asf
tabl <- mar$ar02$d.irisr.pass
y_aggreg <- asf_data(tabl,
                     dataf,
                     vars = c(5:22),
                     funs = c("sum"),
                     id = c("IRISF_CODE", "IRISF_CODE"),
                     maille = "IRISrS_CODE")

# Verification de la coherence des resultats
sum(data$RP18_F_R_CP1_C1.Agri, na.rm = TRUE)
sum(x_aggreg$RP18_F_R_CP1_C1.Agri, na.rm = TRUE)
sum(y_aggreg$RP18_F_R_CP1_C1.Agri, na.rm = TRUE)


###############################################################################
############################################################# FOND GEOGRAPHIQUE

# Import des donnees d'Aliette a partir du package asf
mar <- asf_mar()

# Couche geographique des iris regroupes
fond <- mar$ar02$sf.irisr.s

fond <- asf_drom(fond, 
                 id = "IRISrS_CODE")

mf_map(fond)

# Creation de zooms
z <- asf_zoom(fond,
              villes = c("Paris", "Avignon", "Mulhouse"), 
              lon = c(5.721), 
              lat = c(45.182),
              buffer = 10000)

zoom <- z$zoom
label <- z$label

mf_map(zoom)


# Jointure entre le fond et les donnees agregees
fondata <- asf_fondata(y_aggreg, fond, zoom, id = c("IRISrS_CODE", "IRISrS_CODE"))

mf_map(fondata, 
       var = "RP18_F_R_CP1_C3.Cadres", 
       type = "choro",
       border = NA)

mf_label(label, 
         var = "nom", 
         col = "#000000", 
         font = 1)






