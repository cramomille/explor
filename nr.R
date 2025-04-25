
#                                EXPLORATIONS ET TESTS SUR DES DONNEES A L'IRIS
#
#                                                                antoine beroud
#                                                             nicolas raimbault
#                                                                    avril 2025


###############################################################################
################################################### INSTALLATION DU PACKAGE ASF

# Installation du package remotes pour permettre l'installation de packages sur GitLab
install.packages("remotes")

# Installation du package asf
remotes::install_gitlab("atlas-social-de-la-france/asf", host = "gitlab.huma-num.fr")

# Lancement des packages utilises dans le code
library(sf)
library(mapsf)
library(asf)


# install.packages("devtools")
# devtools::install_github("alietteroux/subwork")

library(subwork)

# Import des donnees "FT810" dans un data.frame nomme "FT810.data"
FT810.data <- import(code = "FT810", type = "data")
FT711.data <- import(code = "FT711", type = "data")

cols <- "F_R_CP1"
data <- FT810.data

data <- data[, c(1:3, grep(cols, names(data)))]


###############################################################################
######################################################### ID MAILLAGE COMPOSITE

# Import des donnees d'Aliette a partir du package asf
mar <- asf_mar(sf = FALSE)

# Code des iris de reference
tabl <- mar$ar01$d.irisf.pass
irisf <- merge(tabl[, c("IRIS_CODE", "IRISF_CODE")], data, by.x = "IRISF_CODE", by.y = "IRIS")

# Code des iris regroupes pour une analyse synchronique
tabl <- mar$ar02$d.irisr.pass
irisr <- merge(tabl[, c("IRISF_CODE", "IRISrS_CODE", "IRISrS_LIB")], irisf, by = "IRISF_CODE")

id_i <- unique(irisr$IRIS_CODE)
id_if <- unique(irisr$IRISF_CODE)
id_ir <- unique(irisr$IRISrS_CODE)

# Agregation simple
test <- rowsum(irisr[, c(7:24)], group = irisr$IRISrS_CODE)


# Code des iris regroupes pour une analyse synchronique
tabl <- mar$ar02$d.irisr.pass

# Agregation des colonnes
irisr <- asf_data(tabl,
                  irisf,
                  vars = c(5:22),
                  funs = c("sum"),
                  id = c("IRISF_CODE", "IRISF_CODE"),
                  maille = "IRISrS_CODE")

sum(irisr$RP18_F_R_CP1_C1.Agri)
sum(irisr$RP18_F_R_CP1_C1.Agri)





###############################################################################
############################################################# FOND GEOGRAPHIQUE

# Import des donnees d'Aliette a partir du package asf
mar <- asf_mar()

# Couche geographique des iris regroupes
fond <- mar$ar02$sf.irisr.s

# Jointure entre le fond et les donnees agregees
fondata <- merge(fond, irisr, by = "IRISrS_CODE")

# Test de carte
mf_map(fondata, var = "RP18_F_R_CP1_C1.Agri", type = "prop")


