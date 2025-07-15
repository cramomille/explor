library(sf)

load("C:/Users/Antoine Beroud/Desktop/mar/AR01_geog_constante.RData")
load("C:/Users/Antoine Beroud/Desktop/mar/AR02_maille_IRISr.RData")

write.csv(d.comf.app, "d.comf.app.csv", row.names = FALSE)
write.csv(d.comf.pass, "d.comf.pass.csv", row.names = FALSE)
write.csv(d.irisf.pass, "d.irisf.pass.csv", row.names = FALSE)
write.csv(d.irisr.app, "d.irisr.app.csv", row.names = FALSE)
write.csv(d.irisr.etapes, "d.irisr.etapes.csv", row.names = FALSE)
write.csv(d.irisr.pass, "d.irisr.pass.csv", row.names = FALSE)

sf.comf <- st_transform(sf.comf, crs = 2154)
sf.irisf <- st_transform(sf.irisf, crs = 2154)
sf.irisr.d <- st_transform(sf.irisr.d, crs = 2154)
sf.irisr.s <- st_transform(sf.irisr.s, crs = 2154)

st_write(sf.comf, "sf.comf.gpkg")
st_write(sf.irisf, "sf.irisf.gpkg")
st_write(sf.irisr.d, "sf.irisr.d.gpkg")
st_write(sf.irisr.s, "sf.irisr.s.gpkg")

################################################################### COM en COMF
d.comf.pass <- mar$ar01$d.comf.pass
d.comf.app <- mar$ar01$d.comf.app

nb <- unique(d.comf.pass$COM_CODE)

comf <- merge(d.comf.pass[, c(2,5)], d.comf.app, by = "COMF_CODE", all.x = TRUE)
names(comf)[5] <- "COM_TYPE"

na <- comf[apply(is.na(comf), 1, any), ]

comf <- comf[, c(
  "COM_CODE",
  "COM_TYPE",
  "COMF_CODE",
  "COMF_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf")))

################################################################### COM en COMR
d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(6,7)]

# Decomposition des identifiants agreges en une liste
id_list <- strsplit(d.irisr.app$COMF_CODE_MULTI, " \\| ")

# Creation d'une table d'association entre chaque commune et son COMF_CODE_MULTI
id_tabl <- data.frame(
  COMF_CODE = unlist(id_list),
  COMR_CODE = rep(d.irisr.app$COMF_CODE_MULTI, sapply(id_list, length))
)
id_tabl <- id_tabl[!duplicated(id_tabl$COMF_CODE), ]

summary(nchar(d.irisr.app$COMF_CODE_MULTI))
summary(nchar(id_tabl$COMR_CODE))

tmp <- comf[, c(1:4)]
nb <- unique(comf$COM_CODE)

comr <- merge(tmp, id_tabl, by = "COMF_CODE", all.x = TRUE)

d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(6,7, 11:26)]
d.irisr.app <- d.irisr.app[!duplicated(d.irisr.app$COMF_CODE_MULTI), ]

comr <- merge(comr, d.irisr.app, by.x = "COMR_CODE", by.y = "COMF_CODE_MULTI", all.x = TRUE)
names(comr)[5] <- "COMF_LIB"
names(comr)[6] <- "COMR_LIB"

comr <- comr[, c(
  "COM_CODE",
  "COM_TYPE",
  # "COMF_CODE",
  # "COMF_LIB",
  "COMR_CODE",
  "COMR_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr")))

################################################################# IRIS en IRISF
d.irisf.pass <- mar$ar01$d.irisf.pass
nb <- unique(d.irisf.pass$IRIS_CODE)

tmp <- comf[, -c(1,2)]
tmp <- tmp[!duplicated(tmp$COMF_CODE), ]

irisf <- merge(d.irisf.pass[, -1], tmp, by = "COMF_CODE")
nb <- unique(irisf$IRIS_CODE)

irisf <- irisf[, c(
  "IRIS_CODE",
  "IRISF_CODE",
  "IRISF_LIB",
  "COMF_CODE",
  "COMF_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr", "irisf")))

################################################################# IRIS en IRISR
d.irisr.pass <- mar$ar02$d.irisr.pass
d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(2,6,7,11:26)]

irisr <- merge(d.irisr.pass[, -1], d.irisr.app, by = "IRISrD_CODE")
names(irisr)[8] <- "COMR_CODE"
names(irisr)[9] <- "COMR_LIB"

irisr <- irisr[, c(
  "IRIS_CODE",
  "IRISF_CODE",
  "IRISrS_CODE",
  "IRISrS_LIB",
  "IRISrD_CODE",
  "IRISrD_LIB",
  # "COMF_CODE",
  "COMR_CODE",
  "COMR_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr", "irisf", "irisr")))

####################################################################### EXPORTS
library(sf)

comf[] <- lapply(comf, as.character)
str(comf)
comr[] <- lapply(comr, as.character)
str(comr)
irisf[] <- lapply(irisf, as.character)
str(irisf)
irisr[] <- lapply(irisr, as.character)
str(irisr)

comf <- comf[order(comf$COM_CODE), ]
comr <- comr[order(comr$COM_CODE), ]
irisf <- irisf[order(irisf$IRIS_CODE), ]
irisr <- irisr[order(irisr$IRIS_CODE), ]

sf.irisf <- mar$ar01$sf.irisf
sf.irisf <- sf.irisf[, c(1,2)]
st_write(sf.irisf, "sf.irisf.gpkg")
write.csv(comf, "df.comf.csv")
write.csv(comr, "df.comr.csv")
write.csv(irisf, "df.irisf.csv")
write.csv(irisr, "df.irisr.csv")
