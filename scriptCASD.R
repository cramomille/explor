
#                            EXPLORATIONS POUR LE TRAITEMENT DES DONNEES DU CASD
#
#                                                                 antoine beroud
#                                                                   janvier 2025

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

# Fonction de creation de fichiers .parquet a partir de fichiers .sas7bdat
sas_parquet <- function(sas_files,
                        parquet_dir,
                        chunk_size = 1000000) {
  
  for (x in sas_files) {
    sas_name <- sub("\\.sas7bdat$", "", basename(x))
    
    # Creation d'un dossier specifique pour ce fichier .sas
    output_dir <- file.path(parquet_dir, sas_name)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
      
    # Definition des parametres des chunks
    count <- 1
    row <- 0
    total_time <- 0
    
    # Boucle principale
    repeat {
      
      start_time <- Sys.time() # -----------------------------------------------
      
      # Lecture d'un chunk du fichier .sas
      chunk <- tryCatch(
        read_sas(x, skip = row, n_max = chunk_size),
        error = function(e) NULL
      )
      
      if (is.null(chunk) || nrow(chunk) == 0) break
      
      # Ecriture du fichier .parquet dans le dossier specifique au fichier .sas traite
      output <- file.path(output_dir, paste0(sas_name, "_chunk", sprintf("%02d", count), ".parquet"))
      write_parquet(chunk, output, compression = "snappy")
      
      end_time <- Sys.time() # -------------------------------------------------
      
      # Calcul du temps de calcul et d'ecriture du chunk
      chunk_time <- as.numeric(end_time - start_time, units = "secs")
      total_time <- total_time + chunk_time
      m <- floor(chunk_time / 60)
      s <- round(chunk_time %% 60)
      
      # Mise a jour de la premiere ligne du prochain chunk et formatage du message
      row <- row + nrow(chunk)
      row_text <- formatC(row, format = "d", big.mark = " ")
      
      # Message d'avancement du traitement du fichier .sas
      cat(sprintf("Chunk %02d : total de %10s lignes traitées [time %02d:%02d]\n", 
                  count, row_text, m, s))
      
      # Incrementation du compteur de chunks
      count <- count + 1
    }
    
    # Message final pour chaque fichier .sas entierement traite
    total_m <- floor(total_time / 60)
    total_s <- round(total_time %% 60)
    cat(sprintf("\nFichier '%s' traité [time %02d:%02d]\n\n", 
                sas_name, total_m, total_s))
  }
}


# Test de la fonction
sas_parquet(sas_files = c("test/parquet/data1.sas7bdat", 
                          "test/parquet/data2.sas7bdat"), 
            parquet_dir = "test/parquet/export/",
            chunk_size = 100000)


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
# Ouverture des fichiers .parquet d'un dossier avec Arrow
tabl <- arrow::open_dataset("test/parquet/export/data1/")

# Selection et collecte des donnees d'interet avec dplyr
data <- tabl %>% 
  # filter(dep == "69") %>%
  select(c(1:2, 5:15)) %>% 
  collect()


# # Agregation des iris en communes
# data <- data %>%
#   group_by(COM) %>%
#   summarise(
#     across(.cols = where(is.numeric), .fns = sum, .names = "{.col}"),
#     .groups = "drop"
#   )


################################################################################
############################################################# SECRET STATISTIQUE

# Sur la ligne, toutes les colonnes comprennent une valeur >= a la limite :
# pas de secretisation des valeurs
# 25   64   42   32   24   84   53
# 25   64   42   32   24   84   53

# Sur la ligne, toutes les colonnes comprennent une valeur < a la limite :
# secretisation des valeurs
# 10   15   17   12   14   11   10
# NA   NA   NA   NA   NA   NA   NA

# Sur la ligne, toutes les colonnes sauf une comprennent une valeur = 0  :
# secretisation des valeurs
#  0   64    0    0    0    0    0
# NA   NA   NA   NA   NA   NA   NA

# Sur la ligne, toutes les colonnes sauf une comprennent une valeur >= a la limite :
# secretisation de cette valeur et de la deuxieme plus petite valeur
# 25   64   15   32   24   84   53
# 25   64   NA   32   NA   84   53


# Fonction de secretisation d'un data.frame
secret_df <- function(dataframe, 
                      cols,
                      limit = 11,
                      unique = TRUE) {
  
  # unique = TRUE  si toutes les colonnes composent un tout
  # unique = FALSE si les colonnes ne composent pas un tout
  
  # Fonction interne pour secretiser une ligne
  secret_row <- function(row) {
    
    # Conversion des valeurs en numerique
    values <- as.numeric(row)
    
    # Cas ou une seule colonne contient en realite tous les individus
    if (unique && length(values) > 1) {
      # Verification que toutes les colonnes sauf une contiennent un 0 ou un NA
      x <- which(values > 0 & !is.na(values))
      # Si une seule colonne contient une valeur non nulle, la secretiser
      if (length(x) == 1) {
        values[x] <- NA
      }
    }
    
    # Remplacement des valeurs inferieures a la limit par des valeurs NA
    values_secret <- ifelse(values < limit, NA, values)
    
    # Verification du nombre de valeurs secretisees
    secret_indices <- which(is.na(values_secret))
    
    if (length(secret_indices) == 1) {
      # Trouver l'indice de la deuxieme plus petite valeur
      values_no_secret <- values[!is.na(values_secret)]
      second_min <- min(values_no_secret)
      second_min_index <- which(values == second_min)[1]
      values_secret[second_min_index] <- NA
    }
    
    return(values_secret)
  }
  
  # Application de la secretisation ligne par ligne sur les colonnes specifiees
  dataframe[cols] <- t(apply(dataframe[cols], 1, secret_row))
  
  return(dataframe)
}



# Data.frame d'exemple
df <- data.frame(
  commune = c("com1", "com2", "com3", "com4", "com5"),
  tot = c(100, 100, 50, 60, 20),
  ca1 = c(20, 40, 10, 0, 0),
  ca2 = c(10, 10, 10, 20, 0),
  ca3 = c(30, 10, 10, 20, 0),
  ca4 = c(40, 40, 10, 20, 20)
)

# Test de la fonction sur l'exemple
test <- secret_df(df, cols = c(3:6), limit = 11, unique = FALSE)

# # Test de la fonction sur un dataframe de 35 000 entites
# test <- secret_df(data, cols = c(3:12), limit = 11)
# 
# sum(complete.cases(test))



################################################################################
################################################################# CALCUL DE TVAM

# Fonction pour calculer le TVAM entre deux data.frames identiques 
calcul_tvam <- function(dataframes, 
                        annees, 
                        id) {
  
  # Vérification que le nombre de dataframes correspond au nombre d'années données
  if (length(dataframes) != length(annees)) {
    stop("Le nombre de dataframes doit être égal au nombre d'années fournies.")
  }
  
  # Vérification que tous les dataframes contiennent la colonne identifiant spécifiée
  if (!all(sapply(dataframes, function(df) id %in% colnames(df)))) {
    stop("Tous les dataframes doivent contenir la colonne identifiant spécifiée.")
  }
  
  # Initialisation du dataframe final avec la colonne identifiant du premier dataframe
  df_resultat <- dataframes[[1]][, id, drop = FALSE]
  
  # Parcourir les paires consecutives de dataframes
  for (i in seq_along(dataframes)[-length(dataframes)]) {
    # Dataframes et années courantes
    df1 <- dataframes[[i]]
    df2 <- dataframes[[i + 1]]
    annee1 <- annees[i]
    annee2 <- annees[i + 1]
    
    # Nombre d'annees entre les deux périodes
    nb_years <- annee2 - annee1
    
    # Calcul des TVAM pour chaque variable (sauf colonne identifiant)
    tvam <- lapply(names(df1)[!names(df1) %in% id], function(var) {
      valeur1 <- df1[[var]]
      valeur2 <- df2[[var]]
      taux <- ifelse(valeur1 > 0,
                     round(((valeur2 / valeur1)^(1 / nb_years) - 1) * 100, 2),
                     NA) # NA si valeur initiale est 0
      return(taux)
    })
    
    # Convertir en dataframe
    df_tvam <- as.data.frame(tvam)
    colnames(df_tvam) <- paste0("tvam_", annee1, "_", annee2, "_", names(df1)[!names(df1) %in% id])
    
    # Ajouter au dataframe final
    df_resultat <- cbind(df_resultat, df_tvam)
  }
  
  return(df_resultat)
}


# Fonction pour generer des data.frames avec des valeurs aleatoires
generate_df <- function(names) {
  
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


