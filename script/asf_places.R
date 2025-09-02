# liste des communes pre-renseignees pour la fonction asf_zoom

library(asf)
library(sf)
library(mapsf)
library(readxl)

aav <- read_xlsx("input/aav/AAV2020_au_01-01-2023.xlsx", sheet = "Composition_communale", skip = 5)
mar <- asf_mar()

tabl <- mar$ar01$d.comf.pass
com <- mar$ar01$sf.comf

com <- st_transform(com, 2154)

aav <- merge(tabl[, c(2, 5)], aav, by.x = "COM_CODE", by.y = "CODGEO")

aav <- merge(aav[, -1], com, by.x = "COMF_CODE", by.y = "COMFA_CODE", all.x = TRUE)

aav_geom <- st_as_sf(aav, sf_column_name = "geom")

aav_geom <- aav_geom[aav_geom$CATEAAV2020 == "11", ] 
aav_geom <- aav_geom[!is.na(st_geometry(aav_geom)) & !st_is_empty(aav_geom), ]

setdiff(aav$AAV2020, aav_geom$AAV2020)

arr <- com[grepl("75056|69123|13055", com$COMFC_CODE), ]
comarr <- aggregate(arr, 
                    by = list(arr$COMFC_CODE),
                    FUN = function(x) x[1])
comarr <- comarr[, c(1, 2)]

names(comarr)[1] <- "LIBGEO"
names(comarr)[2] <- "AAV2020"
st_geometry(comarr) <- "geom"

comarr$LIBGEO[1] <- "Marseille"
comarr$LIBGEO[2] <- "Lyon"
comarr$LIBGEO[3] <- "Paris"

comarr$AAV2020[1] <- "003"
comarr$AAV2020[2] <- "002"
comarr$AAV2020[3] <- "001"

aav_geom <- aav_geom[, c(2, 3)]

fond <- rbind(aav_geom, comarr)

aav <- read_xlsx("input/aav/AAV2020_au_01-01-2023.xlsx", sheet = "AAV2020", skip = 5)

fond <- merge(fond, aav[, c(1, 3)], by = "AAV2020")


fond <- st_transform(fond, crs = 4326)
centro <- st_centroid(fond)

coords <- st_coordinates(centro)

fond$lon <- round(coords[, "X"], 3)
fond$lat <- round(coords[, "Y"], 3)





tmp <- fond[, c(2, 3, 5, 6)]
tmp$geom <- NULL
tmp <- tmp[tmp$TAAV2017 != "1", ]
names(tmp)[1] <- "lab"
names(tmp)[2] <- "aav"

# Concatène tout le texte en un seul vecteur
tout_texte <- paste(tmp$lab, collapse = "")

# Sépare en caractères individuels
chars <- strsplit(tout_texte, "")[[1]]

# Garde les caractères non alphanumériques
speciaux <- chars[!grepl("[A-Za-z0-9]", chars)]

# Liste unique et triée
speciaux_uniques <- sort(unique(speciaux))

# Affiche
print(speciaux_uniques)

accents    <- c("à", "â", "ç", "é", "É", "è", "ê", "ë", "î", "Î", "ô", "œ", "û")
sansaccent <- c("a", "a", "c", "e", "E", "e", "e", "e", "i", "I", "o", "oe", "u")

for (i in seq_along(accents)) {
  tmp$lab <- gsub(accents[i], sansaccent[i], tmp$lab, fixed = TRUE)
}

head(tmp, 5)

tmp <- tmp[order(tmp$lab), ]

cat(
  apply(tmp, 1, function(row) {
    sprintf('"%s", %d, %.3f, %.3f,', 
            row[1], 
            as.integer(row[2]), 
            as.numeric(row[3]), 
            as.numeric(row[4]))
  }),
  sep = "\n"
)
