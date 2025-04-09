
#                           EXPLORATIONS POUR LE TRAITEMENT DES DONNEES DU CASD
#
#                                                                antoine beroud
#                                                                  janvier 2025

f <- list.files("script/function",
                pattern = "\\.R$",
                full.names = TRUE)

invisible(sapply(f, source))


library(sf)
library(arrow)
library(duckdb)
library(dplyr)


###############################################################################
################################################# CREATION DE FICHIERS .PARQUET

# Creation de fichiers de donnees test
set.seed(123)
n <- 1e6

df <- data.frame(
  id = sprintf("%07d", 1:n),
  value1 = sample(10:100000, n, replace = TRUE),
  value2 = sample(10:100000, n, replace = TRUE)
)

DF <- data.frame(
  ID = sprintf("%07d", 1:n),
  VALUE1 = sample(10:100000, n, replace = TRUE),
  VALUE2 = sample(10:100000, n, replace = TRUE)
)

# Conversion en fichier.sas7bdat
write_sas(df, "output/sas/data1.sas7bdat")
write_sas(DF, "output/sas/data2.sas7bdat")

# Transformation de fichiers .sas7bdat en .parquet
convert_sas_parquet(sas_files = c("output/sas/data1.sas7bdat",
                                  "output/sas/data2.sas7bdat"),
                    parquet_dir = "output/parquet/",
                    chunk_size = 100000)


###############################################################################
################################################ OUVERTURE DE FICHIERS .PARQUET

# Ouvertur d'un fichier .parquet
data1 <- open_parquet(dir = "output/parquet/data1",
                      id = "id",
                      vars = c("value1", "value2"))

# Ouverture de deux fichiers .parquet
result <- open_parquets(dir = "output/parquet/", 
                        folder = c("data1", "data2"), 
                        id = c("id", "ID"), 
                        vars = list(c("value1", "VALUE1"), c("value2", "VALUE2")))
 
data1 <- result[[1]]
data2 <- result[[2]]












































































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

AR01 <- load("input/mar/donnees/AR01_geog_constante.RData")
AR02 <- load("input/mar/donnees/AR02_maille_IRISr.RData")































# Ouverture du fichier avec les irisr
iris <- st_read("input/mar/donnees/shapefiles/AR02_sf_irisr.shp")
iris <- st_as_sf(iris)
iris <- st_transform(iris, 2154)

iris <- iris[, c(1,2,5,6,7)]
colnames(iris) <- c("IRIS_CODE", "IRIS_LIB", "COMF_CODE", "COMF_LIB", "P21_POP", "geometry")

# Ouverture du fichier avec toutes les iris pour ajouter Mayotte
mayo <- st_read("input/mar/donnees/shapefiles/AR01_sf_irisf.shp")
mayo <- st_as_sf(mayo)
mayo <- st_transform(mayo, 2154)

mayo <- mayo[grepl("^976", mayo$IRISF_CODE), ]

mayo$COMF_LIB <- NA
mayo$P21_POP <- NA

mayo$COMF_LIB <- as.numeric(mayo$COMF_LIB)
mayo$P21_POP <- as.numeric(mayo$P21_POP)

mayo <- mayo[, c(1,2,4,7,8)]
colnames(mayo) <- c("IRIS_CODE", "IRIS_LIB", "COMF_CODE", "COMF_LIB", "P21_POP", "geometry")

# Collage des deux data.frames
fond <- rbind(iris, mayo)

# Utilisation de la fonction pour deplacer les geometries des DROM
fond_created <- create_fond(fond)

# Export
st_write(fond_created, "output/irisar.gpkg")


















library(sf)
library(readxl)

dir_aav <- "input/aav/AAV2020_au_01-01-2023.xlsx"
dir_geo <- "output/irisar.gpkg"

# Lecture des fichiers aav
aav_typ <- read_xlsx(dir_aav, sheet = 1, skip = 5)
aav_com <- read_xlsx(dir_aav, sheet = 2, skip = 5)

aav <- merge(aav_com[, -c(2)], aav_typ[, -c(2)], by = "AAV2020", all.x = TRUE)

# Lecture des couches geographiques
geo <- st_read(dir_geo)

test <- merge(geo, aav, by.x = "COMF_CODE", by.y = "CODGEO", all.x = TRUE)














# Ajout des arrondissements aux aav
arr_aav <- aav[grepl("75056|13055|69123", aav$CODGEO), ]
arr_geo <- geo[grepl("^751|^132|^6938", geo$COMF_CODE), ]

arr_geo <- arr_geo[, c(1)]
arr_geo$geometry <- NULL

arr_aav$id <- substr(arr_aav$CODGEO, 1, 2)
arr_geo$id <- substr(arr_geo$CODGEO, 1, 2)

x <- merge(arr_aav[, -c(2)], arr_geo, by = "id")
x <- x[, -c(1)]

x <- x[, colnames(aav)]

# Fichier des aav avec arrondissements
aav_join <- rbind(aav, x)

write.csv(aav_join, "aav_2020.csv")

# # Fichier des aav avec une geometrie
# y <- merge(com, aav_join, by = "CODGEO")
# 
# # Agregation des geometries en aav
# z <- aggregate(y, by = list(y$AAV2020), FUN = function(x) x[1])
# z <- z[, -c(1)]
# 
# st_write(z, "aav_2020.gpkg")


