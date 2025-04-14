
#                                           EXPLORATIONS ET TEST DU PACKAGE ASF
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
############################################ CREATION DE FICHIERS .PARQUET TEST

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
########################################### OUVERTURE DE FICHIERS .PARQUET TEST

# recuperation des noms d'un fichier parquet
names <- get_parquet_names(dir = "output/parquet/data1")
names <- get_parquet_names(dir = "output/parquet/data2")
names <- get_parquet_names(dir = "output/parquet/data3_chunk01.parquet")

# Ouverture de fichiers .parquet
result <- open_parquet(dir = "output/parquet/",
                       file = c("data1", "data2", "data3_chunk01.parquet"),
                       cols = list(c("id", "ID"),
                                   c("value1", "VALUE1"),
                                   c("value2", "VALUE2")))
 
data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]


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

y <- dput(x) # test

test <- secret_data(x, cols = c(3:6), limit = 11, unique = FALSE)


###############################################################################
######################################## TEST MAILLAGE COMPOSITE D'ALIETTE ROUX

sf_comf <- st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=xuOVdkak43uv0V4wcbi4QL1AjqJVIjKT", "&mode=grid&download=1"))
sf_irisf <- st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=NWLnoKioP7mdamWjnRl5GfkWsq9CUdzD", "&mode=grid&download=1"))
sf_irisrd <- st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=QpH4uxaOcEMEc0u1mb05tRRTuXHs7VQI", "&mode=grid&download=1"))
sf_irisrs <- st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=vT4HOEYWJ5BiPxwoEDYrSG42woMrNw5D", "&mode=grid&download=1"))

pass_comf <- read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=96yNI32NYbEjogu3lleLp52uW8gpEWqW", "&mode=grid&download=1"))
pass_irisf <- read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=WYxbO42GJ8bHATDGH87MZ2wmSWNq1iv5", "&mode=grid&download=1"))
pass_irisr <- read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=4M74RRTg22Ewvy49YhJsEXXyjyncuNVU", "&mode=grid&download=1"))

app_comf <- read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=lQKxMDHb1zWmh4zvK9oC3ll8LJdY1GrA", "&mode=grid&download=1"))
app_irisr <- read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=wgwMU6hEhn42zAIZcmu1Q95aiIgL8VQP", "&mode=grid&download=1"))


fond <- sf_irisf
col <- "COMF_CODE"

test <- aggregate(fond,
                 by = list(fond[[col]]),
                 FUN = function(x) x[1])












# Reprojection des DROM dans les geographies a facon
iris <- create_fond(fond = sf.irisf, id = "IRISF_CODE")

com <- aggreg_fond(tabl = d.irisf.pass,
                   fond = iris,
                   id = c("IRISF_CODE", "IRISF_CODE"), 
                   maille = "COMF_CODE")



iris <- sf.irisr
iris <- st_as_sf(iris)
iris <- st_transform(iris, 2154)

iris <- iris[, c(1,2,5,6,7)]
colnames(iris) <- c("IRIS_CODE", "IRIS_LIB", "COMF_CODE", "COMF_LIB", "P21_POP", "geometry")
 
mayo <- sf.irisf
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

# Reprojection des DROM
irisar <- create_fond(fond = fond, id = "IRIS_CODE")


st_write(fond, "fond.gpkg")
st_write(irisar, "irisar.gpkg")









# install.packages("devtools")
devtools::install_github("alietteroux/subwork")

library(subwork)

# importer les données "FT810" dans un dataframe nommé "FT810.data"
FT810.data <- import(code = "FT810", type = "data")

# FT711.data pour echelle com
# base to salariers



data <- aggreg_data(tabl = d.irisR.pass,
                    data = FT810.data, 
                    id = c("IRIS"))
