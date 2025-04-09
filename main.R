
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




library(arrow)
library(duckdb)
library(dplyr)

open_parquet <- function(dir, folder, id, vars) {
  
  # Création de la connexion à DuckDB pour exécuter des requêtes SQL
  con <- dbConnect(duckdb())
  
  # Initialisation de la liste pour stocker les données
  all_data <- list()
  
  # Boucle pour traiter chaque dossier
  for (f in folder) {
    # Création du chemin vers le dossier
    folder_dir <- paste0(dir, f, "/")
    
    # Récupération des noms de tous les fichiers .parquet dans le dossier
    parquet_files <- list.files(folder_dir, pattern = "\\.parquet$", full.names = TRUE)
    
    if (length(parquet_files) == 0) {
      stop(paste("Aucun fichier .parquet trouvé dans le dossier:", folder_dir))
    }
    
    # Création de la requête SQL pour agréger les fichiers .parquet
    query <- paste0(
      "CREATE OR REPLACE VIEW all_data AS ",
      paste0("SELECT * FROM read_parquet('", parquet_files, "')", collapse = " UNION ALL ")
    )
    dbExecute(con, query)
    
    # Chargement de la vue comme table DuckDB
    tbl_duckdb <- tbl(con, "all_data")
    
    # Sélectionner les colonnes d'intérêt pour ce dossier (en fonction de `vars` pour ce dossier)
    vars_for_folder <- vars[[which(folder == f)]]  # Choisir les colonnes spécifiées pour ce dossier
    
    # Test de toutes les combinaisons possibles entre id et vars_for_folder
    combinaison <- expand.grid(id = id, col = vars_for_folder, stringsAsFactors = FALSE)
    
    # Boucle pour tester les différentes combinaisons
    for (i in seq_len(nrow(combinaison))) {
      comb <- combinaison[i, ]
      
      tryCatch({
        # Sélection et collecte des données d'intérêt avec dplyr
        selected_data <- tbl_duckdb %>%
          select(all_of(c(comb$id, comb$col))) %>%
          collect()
        
        # Renommage des colonnes pour éviter les conflits
        names(selected_data) <- c(id[1], comb$col)
        
        # Stockage des données dans la liste
        all_data[[f]] <- selected_data
        print(paste("Combinaison : id =", comb$id, "/ col =", comb$col, "dans le dossier", f))
        break  # Sort de la boucle si tout est correct
      }, error = function(e) {
        # Si une erreur survient, on passe à la prochaine combinaison
        next
      })
    }
  }
  
  # Déconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  # Combine toutes les données de tous les dossiers en une seule table
  final_data <- bind_rows(all_data)
  
  return(final_data)
}

# Exemple d'utilisation
dir <- "output/parquet/"
folder <- c("data1", "data2")
id <- c("id", "ID")  # Liste des colonnes identifiantes
vars <- list(c("col1", "COL1"), c("col2", "COL2"))  # Liste des colonnes d'intérêt pour chaque dossier

# Appel de la fonction
final_data <- open_parquet(dir = dir, folder = folder, id = id, vars = vars)

# Affichage du résultat
print(final_data)
































































# Definition du nom du dossier qui contient les chunks
f <- "data1"

# Definition du chemin vers ce dossier
dir <- paste0("output/parquet/", f, "/")

# Ouverture du fichier dans 
data <- open_parquet(dir = dir,
                     id = "COM",
                     vars = c("P20_POP", "C20_POP15P_CS1", "C20_POP15P_CS2"))


library(arrow)
library(duckdb)
library(dplyr)

open_parquet <- function(dir, id, vars) {
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Recuperation des noms de tous les fichiers .parquet
  parquet <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  
  # Verification de la presence de fichiers
  if (length(parquet) == 0) {
    stop("Aucun fichier .parquet trouve dans le dossier.")
  }
  
  # Creation d'une vue SQL pour agreger les fichiers .parquet
  query <- paste0(
    "CREATE OR REPLACE VIEW all_data AS ",
    paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
  )
  dbExecute(con, query)
  
  # Chargement de la vue comme table DuckDB
  tbl_duckdb <- tbl(con, "all_data")
  
  # Selection et collecte des donnees d'interet avec dplyr
  data <- tbl_duckdb %>% 
    select(all_of(c(id, vars))) %>% 
    collect()
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(data)
}


f <- c("data1", "data2")

base_dir <- paste0("output/parquet/")

all_data <- lapply(f, function(f) {
  dir <- paste0(base_dir, f, "/")
  open_parquet(dir = dir,
               id = "COM",
               vars = c("P20_POP", "C20_POP15P_CS1", "C20_POP15P_CS2"))
})
















































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


