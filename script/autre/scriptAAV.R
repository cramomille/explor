#                                                             Traitement des AAV
#
#                                                                 antoine beroud
#                                                                   janvier 2025

library(sf)
library(readxl)


# Lecture des fichiers aav
aav_typ <- read_xlsx("data/aav/AAV2020_au_01-01-2020/AAV2020_au_01-01-2020_v1.xlsx", sheet = 1, skip = 5)
aav_com <- read_xlsx("data/aav/AAV2020_au_01-01-2020/AAV2020_au_01-01-2020_v1.xlsx", sheet = 2, skip = 5)

aav <- merge(aav_com[, -c(2)], aav_typ[, -c(2)], by = "AAV2020", all.x = TRUE)

# Lecture des couches geographiques
iris <- st_read("data/fond_2019.gpkg")

com <- aggregate(iris, by = list(iris$INSEE_COM), FUN = function(x) x[1])
colnames(com)[1] <- "CODGEO"
com <- com[, c(1,3)]

# Ajout des arrondissements aux aav
arr_aav <- aav[grepl("75056|13055|69123", aav$CODGEO), ]
arr_com <- com[grepl("^751|^132|^6938", com$CODGEO), ]

arr_com <- arr_com[, c(1)]
arr_com$geometry <- NULL

arr_aav$id <- substr(arr_aav$CODGEO, 1, 2)
arr_com$id <- substr(arr_com$CODGEO, 1, 2)

x <- merge(arr_aav[, -c(2)], arr_com, by = "id")
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