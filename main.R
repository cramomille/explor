
#                                                        EXPLORATIONS ET TEST R
#
#                                                                antoine beroud
#                                                                  janvier 2025

invisible(sapply(list.files("script/function", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))

library(sf)
library(mapsf)
library(asf)


# CREATION DE FICHIERS .PARQUET -----------------------------------------------
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


# OUVERTURE DE FICHIERS .PARQUET ----------------------------------------------
# Recuperation des noms d'un fichier parquet
names <- parquet_colname(dir = "output/parquet/data1")
names <- parquet_colname(dir = "output/parquet/data2")
names <- parquet_colname(dir = "output/parquet/data3.parquet")

# Ouverture d'un fichier .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = "data3.parquet",
                       cols = c(1:3))

data3 <- result[[1]]

# Ouverture d'un fichier .parquet a partir du dossier contenant les chunks qui 
# le composent 
result <- parquet_open(dir = "output/parquet/",
                       file = "data1",
                       cols = c(1:3))

data1 <- result[[1]]

# Ouverture de plusieurs fichiers .parquet (avec des colonnes equivalentes mais 
# nommees differemment dans les differents fichiers)
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
                       cols = list(c("id", "ID"),
                                   c("value1", "VALUE1"),
                                   c("value2", "VALUE2")))
 
data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]


# SECRET STATISTIQUE ----------------------------------------------------------
# Creation d'un data.frame d'exemple
x <- data.frame(
  commune = c("com1", "com2", "com3", "com4", "com5"),
  tot = c(100, 100, 50, 60, 20),
  ca1 = c(20, 40, 10, 0, 0),
  ca2 = c(10, 10, 10, 20, 0),
  ca3 = c(30, 10, 10, 20, 0),
  ca4 = c(40, 40, 10, 20, 20)
)

y <- dput(x) # test de cette fonction

test <- secret_data(x, cols = c(3:6), limit = 11, unique = FALSE)


# TABLEAU CROISE --------------------------------------------------------------
# Creation d'un data.frame d'exemple
set.seed(1)

x <- data.frame(
  csp  = sample(c("agriculteurice", "ouvriere", "cadre"), 20, replace = TRUE),
  sexe = sample(c("homme", "femme"), 20, replace = TRUE),
  poids = sample(1:5, 20, replace = TRUE)
)

t <- create_xtab(d, "csp", "sexe", "poids")

u <- create_xtab(d, "csp", "sexe")


