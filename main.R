
#                                           EXPLORATIONS ET TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                  janvier 2025

invisible(sapply(list.files("script/function", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))

library(sf)
library(asf)
library(mapsf)
# options(error = NULL)

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
parquet_convert(sas = c("output/sas/data1.sas7bdat",
                        "output/sas/data2.sas7bdat"),
                parquet = "output/parquet/",
                chunk = 100000)


###############################################################################
########################################### OUVERTURE DE FICHIERS .PARQUET TEST

# recuperation des noms d'un fichier parquet
names <- parquet_colname(dir = "output/parquet/data1")
names <- parquet_colname(dir = "output/parquet/data2")
names <- parquet_colname(dir = "output/parquet/data3.parquet")

# Ouverture de fichiers .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
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


# 
# # install.packages("devtools")
# devtools::install_github("alietteroux/subwork")
# 
# library(subwork)
# 
# # importer les données "FT810" dans un dataframe nommé "FT810.data"
# FT810.data <- import(code = "FT810", type = "data")
# 
# # FT711.data pour echelle com
# # base to salariers
# 
# 
# 
# data <- aggreg_data(tabl = d.irisR.pass,
#                     data = FT810.data, 
#                     id = c("IRIS"))
###############################################################################
######################################## TEST MAILLAGE COMPOSITE D'ALIETTE ROUX

# Ouverture du fichier des iris
mar <- asf_mar()

# Selection des iris
iris <- mar$geom$irisrs
iris <- iris[, c(1,2,7)]
colnames(iris) <- c("irisrs_code", "irisrs_lib", "p21_pop", "geometry")
st_geometry(iris) <- "geometry"

# Selection des iris de Mayotte
mayo <- mar$geom$irisf
mayo <- mayo[grepl("^976", mayo$IRISF_CODE), ]
mayo$P21_POP <- NA
mayo$P21_POP <- as.numeric(mayo$P21_POP)
mayo <- mayo[, c(1,2,7)]
colnames(mayo) <- c("irisrs_code", "irisrs_lib", "p21_pop", "geometry")
st_geometry(mayo) <- "geometry"

# Collage des deux objets sf/data.frames
f <- rbind(iris, mayo)

# Repositionnement des geometries des DROM
fond <- asf_drom(iris, id = "irisrs_code")
mf_map(fond)
fond <- asf_drom(f, id = "irisrs_code")
mf_map(fond)


# tabl <- mar$pass$irisr
# 
# data <- read.csv("input/csp_2020.csv")


x <- c(1,2,3,4,5)
max(x)

length(fond)




if (is.numeric(vars)) {
  vars <- names(data)[vars]
}

data <- data[, c(id[2], vars)]