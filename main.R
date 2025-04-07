
#                           EXPLORATIONS POUR LE TRAITEMENT DES DONNEES DU CASD
#
#                                                                antoine beroud
#                                                                  janvier 2025

f <- list.files("script/function",
                pattern = "\\.R$",
                full.names = TRUE)

invisible(sapply(f, source))


library(sf)
library(asf)


###############################################################################
################################################# CREATION DE FICHIERS .PARQUET

# Telechargement d'un fichier de donnees test
data <- read.csv("input/iris/base-ic-evol-struct-pop-2020.CSV", sep = ";")
data$dep <- substr(data$COM, 1, 2)
data_plus <- do.call(rbind, replicate(21, data, simplify = FALSE))
data_plus <- data_plus[1:1000000, ]

# Conversion en fichier.sas7bdat
write_sas(data_plus, "output/sas/data1.sas7bdat")

# Transformation de fichiers .sas7bdat en .parquet
convert_sas_parquet(sas_files = c("output/sas/data1.sas7bdat",
                                  "output/sas/data2.sas7bdat"),
                    parquet_dir = "output/parquet/",
                    chunk_size = 100000)


###############################################################################
################################################ OUVERTURE DE FICHIERS .PARQUET

# Definition du nom du dossier qui contient les chunks
f <- "data1"

# Definition du chemin vers ce dossier
dir <- paste0("output/parquet/", f, "/")

# Ouverture du fichier dans R
data <- open_parquet(dir = dir,
                     id = c("COM"),
                     col = c("P20_POP"))


###############################################################################
############################################################ SECRET STATISTIQUE

# Data.frame d'exemple
x <- data.frame(
  commune = c("com1", "com2", "com3", "com4", "com5"),
  tot = c(100, 100, 50, 60, 20),
  ca1 = c(20, 40, 10, 0, 0),
  ca2 = c(10, 10, 10, 20, 0),
  ca3 = c(30, 10, 10, 20, 0),
  ca4 = c(40, 40, 10, 20, 20)
)

test <- secret_data(x, cols = c(3:6), limit = 11, unique = FALSE)

# # Test de la fonction sur un dataframe de 35 000 entites
# test <- secret_df(data, cols = c(3:12), limit = 11)
# 
# sum(complete.cases(test))



###############################################################################
########################################################################## TEST
















