
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

fond <- st_read("input/mar/donnees/shapefiles/AR02_sf_irisr.shp")

  
# Creation d'une couche avec les contours de l'hexagone
met <- sf::st_union(fond[!grepl("^98|^97", fond$COMF_CO),])
met <- sf::st_as_sf(met)

# Calcul des limites de la zone de l'hexagone
bbox <- sf::st_bbox(met)
xmin_met <- as.numeric(bbox[1])
ymin_met <- as.numeric(bbox[2])
xmax_met <- as.numeric(bbox[3])
ymax_met <- as.numeric(bbox[4])

# Espace entre les encarts et l'hexagone
space <- (xmax_met - xmin_met) *0.05

# Taille des encarts par rapport a la taille de l'hexagone
ptc_met <- 0.18

# Definition du positionnement de chaque encart
# Guadeloupe
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
boxes <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
boxes <- sf::st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Guadeloupe"

# Martinique
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *1)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique"
boxes <- rbind(boxes, xx)

# Guyane
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *2)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 3
xx$name <- "Guyane"
boxes <- rbind(boxes, xx)

# Reunion
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *3)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 4
xx$name <- "Reunion"
boxes <- rbind(boxes, xx)

# Mayotte
xmax <- xmin_met - space
xmin <- xmax - ((ymax_met - ymin_met) *(ptc_met))
ymax <- ymax_met - (((ymax_met - ymin_met) *(ptc_met)) *4)
ymin <- ymax - ((ymax_met - ymin_met) *(ptc_met))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:2154"))
xx <- sf::st_as_sf(xx)
xx$id <- 5
xx$name <- "Mayotte"
boxes <- rbind(boxes, xx)

# Coordonnees veritables des zones ou se situent les DROM 
boxes$target <- list(c(-62.05, 15.64, -60.99, 16.71), #xmin, ymin, xmax, ymax
                     c(-61.44, 14.19, -60.6, 15.09),
                     c(-55.5, 1.8, -50.8, 6),
                     c(54.99,-21.61, 56.06,-20.64),
                     c(44.8, -13.2, 45.5, -12.5)
)

# EPSG local pour chaque zone
boxes$epsg_loc <- c(5490, 5490, 2972, 2975, 4471)
sf::st_geometry(boxes) <- "geometry"
sf::st_crs(boxes) <- 2154

# Creation des encarts
input <- iris
input <- sf::st_transform(input, crs = "EPSG:4326")
met <- sf::st_transform(met, crs = "EPSG:4326")
inter <- sf::st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- sf::st_transform(out, crs = "EPSG:2154")

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- sf::st_as_sfc(sf::st_bbox(bb, crs = "EPSG:4326"))
  inter <- sf::st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- sf::st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- sf::st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- mapinsetr::m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}

fond <- sf::st_transform(out, crs = "EPSG:2154")















