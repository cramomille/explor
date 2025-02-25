
#                            EXPLORATIONS POUR LE TRAITEMENT DES DONNEES DU CASD
#
#                                                                 antoine beroud
#                                                                   janvier 2025

f <- list.files("script/fonction", 
                pattern = "\\.R$", 
                full.names = TRUE)

sapply(f, source)


library(haven)
library(arrow)
library(duckdb)
library(dplyr)


################################################################################
################################################## CREATION DE FICHIERS .PARQUET

# # Telechargement d'un fichier de donnees test
# data <- read.csv("test/input/base-ic-evol-struct-pop-2020.CSV", sep = ";")
# data$dep <- substr(data$COM, 1, 2)
# data_plus <- do.call(rbind, replicate(21, data, simplify = FALSE))
# data_plus <- data_plus[1:1000000, ]
# 
# # Conversion en fichier.sas7bdat
# write_sas(data_plus, "test/parquet/data.sas7bdat")
# 
# sas_parquet(sas_files = c("test/parquet/data1.sas7bdat", 
#                           "test/parquet/data2.sas7bdat"), 
#             parquet_dir = "test/parquet/export/",
#             chunk_size = 100000)


################################################################################
################################################# OUVERTURE DE FICHIERS .PARQUET

# METHODE 1 --------------------------------------------------------------------
# Creation de la connexion a DuckDB pour executer des requetes SQL
con <- dbConnect(duckdb())

# Direction du dossier qui contient les fichiers .parquet
parquet_dir <- "test/parquet/export/data1/"

# Recuperation des noms de tous les fichiers .parquet
parq <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

# Creation d'une vue SQL pour agreger les fichiers .parquet
query <- paste0(
  "CREATE OR REPLACE VIEW all_data AS ",
  paste0("SELECT * FROM read_parquet('", parq, "')", collapse = " UNION ALL ")
)
dbExecute(con, query)

# Chargement de la vue comme table DuckDB
tbl_duckdb <- tbl(con, "all_data")

# Inspections des colonnes disponibles
tbl_duckdb$lazy_query$vars

# Selection et collecte des donnees d'interet avec dplyr
data <- tbl_duckdb %>%
  # filter(dep == "55") %>%
  select(c(1:2, 45:53)) %>%
  collect()

# Deconnexion de DuckDB
dbDisconnect(con, shutdown = TRUE)


# METHODE 2 --------------------------------------------------------------------
# # Ouverture des fichiers .parquet d'un dossier avec Arrow
# tabl <- arrow::open_dataset("test/parquet/export/data1/")
# 
# # Selection et collecte des donnees d'interet avec dplyr
# data <- tabl %>% 
#   # filter(dep == "69") %>%
#   select(c(1:2, 5:15)) %>% 
#   collect()


# Agregation des iris en communes
data <- data %>%
  group_by(COM) %>%
  summarise(
    across(.cols = where(is.numeric), .fns = sum, .names = "{.col}"),
    .groups = "drop"
  )


################################################################################
############################################################# SECRET STATISTIQUE

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



################################################################################

# Fonction pour generer des data.frames avec des valeurs aleatoires
create_df <- function(names) {
  
  # Initialisation d'une liste vide pour stocker les data.frames generes
  dfs <- list()
  
  # Creation d'un data.frame pour chaque nom dans le vecteur, 
  for (name in names) {
    df <- data.frame(
      commune = c("com1", "com2", "com3", "com4", "com5"),
      q1 = sample(1:20, 5, replace = TRUE),
      q2 = sample(0:40, 5, replace = TRUE),
      q3 = sample(0:20, 5, replace = TRUE),
      q4 = sample(0:30, 5, replace = TRUE)
    )
    
    # Ajout du data.frame a la liste
    dfs[[name]] <- df
  }
  
  return(dfs)
}


# Appeler la fonction pour générer les data.frames
dfs_list <- generate_df(names = c("2000", "2010", "2020", "2030"))

# Test de la fonction sur les exemples
resultat <- calcul_tvam(
  dataframes = dfs_list,
  annees = c(2000, 2010, 2020, 2030),
  id = "commune"
)



























