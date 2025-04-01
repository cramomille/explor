## Imports
  
### Import des packages
library(sf)
library(mapsf)
library(tidyverse)
library(data.table)
library(readxl)

library(mapinsetr)


### Import de données géoréférencées et de tableaux
iris <- st_read("DATA/GPKG/IRIS.gpkg", quiet = TRUE)
irisData <- st_read("DATA/base-ic-evol-struct-pop-2020.csv", quiet = TRUE)

tab <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-2024.xlsx", 
                            skip = 5, sheet = "COM")) 
tab2 <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-2024.xlsx", 
                             skip = 5, sheet = "ARM")) 
tab_name <- data.frame(read_xlsx("DATA/table-appartenance-geo-communes-2024.xlsx", 
                                 skip = 5, sheet = "Zones_supra_communales"))

### Définition des variables

tot <- "C20_POP15P"
var <- "C20_POP15P_CS6"

# Sélection des variables définies
irisVar <- irisData[, c("IRIS", "COM", tot, var)]

# Traitements sur les tables de correspondance
sel <- c("CODGEO", "DEP", "REG", "EPCI", "CANOV")
zon <- rbind(tab[, sel], tab2[, sel])

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



# Zooms avec définition de coordonnées

# irisJoin <- filter(irisJoin, REG == 11)

# Création d'un dataframe pour stocker les coordonnées
point <- data.frame(latitude = c(4.88),
                    longitude = c(-52.31))

point <- st_as_sf(point, coords = c("longitude", "latitude"), crs = "EPSG:4326")
point <- st_transform(point, crs = "EPSG:2154")

# Faire le buffer autour du point
zoom <- st_buffer(point, dist = 500000)

# Créer le masque
box <- create_mask(bb = zoom, "EPSG:2154")

# st_write(point, "point.gpkg")
# st_write(zoom, "zoom.gpkg")
# st_write(box, "box.gpkg")

# Calcul des limites de la zone de zoom
bbox_zoom <- st_bbox(irisJoin)
xmin <- bbox_zoom[1]
ymin <- bbox_zoom[2]
xmax <- bbox_zoom[3]
ymax <- bbox_zoom[4]

# Décalage horizontal et vertical par rapport à la carte principale
offset_x1 <- xmax + (xmax - xmin) *0.1
offset_y1 <- ymax - (ymax - ymin)
offset_x2 <- xmax + (xmax - xmin) *1.1
offset_y2 <- ymax - (ymax - ymin)

# Création du zoom à côté de la carte principale
zoom <- move_and_resize(x = irisJoin, mask = box, xy = c(offset_x1, offset_y1), 2154, k = 15)

# Fusion de la carte principale et du zoom
irisFinal <- inset_rbinder(list(irisJoin, zoom))

# Affichage
plot(st_geometry(zoom), col = "grey80", lwd = 0.2)


# Zooms DROM

mask <- st_read("DATA/GPKG/voronoi/mask.geojson")
mask <- st_transform(mask, 2154)

met <- mask[5,]

# Calcul des limites de la zone de zoom
bbox <- st_bbox(met)
xmin_met <- as.numeric(bbox[1])
ymin_met <- as.numeric(bbox[2])
xmax_met <- as.numeric(bbox[3])
ymax_met <- as.numeric(bbox[4])

space <- (xmax_met - xmin_met) *0.05
ptc_met <- 0.3

## Guadeloupe
xmax <- xmin_met - space
xmin <- xmax - ((xmax_met - xmin_met) *(ptc_met/2))
ymax <- ymax_met - space
ymin <- ymax - ((xmax_met - xmin_met) *(ptc_met/2))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
boxes <- st_as_sfc(st_bbox(bb, crs = 2154))
boxes <- st_as_sf(boxes)
boxes$id <- 1
boxes$name <- "Guadeloupe"

# Martinique
xmax <- xmin_met - (space *2) - (xmax_met - xmin_met) *(ptc_met/2)
xmin <- xmax - ((xmax_met - xmin_met) *(ptc_met/2))
ymax <- ymax_met - space
ymin <- ymax - ((xmax_met - xmin_met) *(ptc_met/2))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 2
xx$name <- "Martinique"
boxes <- rbind(boxes, xx)

# Guyane
xmax <- xmin_met - space
xmin <- xmax - ((xmax_met - xmin_met) *(ptc_met)) - space
ymax <- ymax_met - ((xmax_met - xmin_met) *(ptc_met)) - (space *3)
ymin <- ymax - ((xmax_met - xmin_met) *(ptc_met)) - space
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 3
xx$name <- "Guyane"
boxes <- rbind(boxes, xx)

# Mayotte
xmax <- xmin_met - space
xmin <- xmax - ((xmax_met - xmin_met) *(ptc_met/2))
ymax <- ymax_met - ((xmax_met - xmin_met) *(ptc_met/2)) - (space *2)
ymin <- ymax - ((xmax_met - xmin_met) *(ptc_met/2))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 4
xx$name <- "Mayotte"
boxes <- rbind(boxes, xx)

# Réunion
xmax <- xmin_met - (space *2) - (xmax_met - xmin_met) *(ptc_met/2)
xmin <- xmax - ((xmax_met - xmin_met) *(ptc_met/2))
ymax <- ymax_met - ((xmax_met - xmin_met) *(ptc_met/2)) - (space *2)
ymin <- ymax - ((xmax_met - xmin_met) *(ptc_met/2))
bb <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
xx <- st_as_sfc(st_bbox(bb, crs = 2154))
xx <- st_as_sf(xx)
xx$id <- 5
xx$name <- "Réunion"
boxes <- rbind(boxes, xx)

# Set input parameters of box (target in WGS84, local EPSG)
boxes$target <- list(c(-62.05, 15.64, -60.99, 16.71), #xmin, ymin, xmax, ymax
                     c(-61.44, 14.19, -60.6, 15.09),
                     c(-55.5, 1.8, -50.8, 6),
                     c(44.8, -13.2, 45.5, -12.5),
                     c(54.99,-21.61, 56.06,-20.64)
)

boxes$epsg_loc <- c(5490, 5490,  2972,  4471, 2975)
st_geometry(boxes) <- "geometry"
st_crs(boxes) <- 2154

# Communes / create insets
input <- st_read("DATA/GPKG/IRIS.gpkg")
input <- st_transform(input, 4326)
met <- st_transform(met, 4326)
inter <- st_intersects(input, met, sparse = FALSE)
out <- input[inter,]
out <- st_transform(out, 2154)

for (i in 1 : nrow(boxes)){
  box <- boxes[i,]
  bb <- as.vector(unlist(box[,"target"]))
  bb <- c(xmin = bb[1], ymin = bb[2], xmax = bb[3], ymax = bb[4])
  mask <- st_as_sfc(st_bbox(bb, crs = 4326))
  inter <- st_intersects(input, mask, sparse = FALSE)
  x <- input[inter,]
  mask <- st_transform(mask, box[,"epsg_loc", drop = T][1])
  x <- st_transform(x, box[,"epsg_loc", drop = T][1])
  inset <- m_r(x = x, mask = mask,  y = box)
  out <- rbind(out, inset)
}
out <- st_transform(out, 4326)

plot(st_geometry(out), col = "grey80", border = NA)


# boxes$target <- NULL
# st_write(boxes, "boxedrom.gpkg")
# st_write(out, "irisdrom.gpkg")



