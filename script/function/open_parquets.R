#' @title Ouverture d'un fichier .parquet dans R
#' @description
#' Cette fonction permet de selectionner des colonnes d'interet presentes dans un
#' fichier .parquet et de les charger dans R
#' 
#' @param dir le chemin vers le dossier qui contient les chunks .parquet du fichier
#' @param id le vecteur avec le nom de la colonne identifiant que l'on souhaite conserver
#' @param vars le vecteur avec le nom des colonne d'interet que l'on souhaite conserver
#' 
#' @return 
#' La fonction ouvre un objet data.frame
#' 
#' @examples
#' data <- open_parquets(dir = "test/parquet/export/",
#'                       folder = c("data1", "data2")
#'                       id = c("id", "ID"),
#'                       vars = list(c("value1", "VALUE1"), c("value2", "VALUE2")))

library(arrow)
library(duckdb)
library(dplyr)

open_parquets <- function(dir, 
                          folder, 
                          id, 
                          vars) {
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Initialisation de la liste pour stocker les donnees
  result <- list()
  
  # Boucle pour traiter chaque dossier
  for (f in folder) {
    
    # Creation du chemin vers le dossier
    folder_dir <- paste0(dir, f, "/")
    
    # Recuperation des noms de tous les fichiers .parquet
    parquet <- list.files(folder_dir, pattern = "\\.parquet$", full.names = TRUE)
    
    if (length(parquet) == 0) {
      stop(paste("Aucun fichier .parquet trouve dans le dossier:", folder_dir))
    }
    
    # Creation d'une vue SQL pour agreger les fichiers .parquet
    query <- paste0(
      "CREATE OR REPLACE VIEW all_data AS ",
      paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
    )
    dbExecute(con, query)
    
    # Chargement de la vue en tant que table DuckDB
    tbl_duckdb <- tbl(con, "all_data")
    
    
    # Generation d'une liste des noms des variables
    vars_names <- paste0("var", seq_along(vars))
    
    # Generation de toutes les combinaisons de noms de colonnes possibles
    combinaison <- do.call(expand.grid, c(list(id = id), setNames(vars, vars_names)))
    
    
    # Boucle pour tester les différentes combinaisons possibles
    for (i in seq_len(nrow(combinaison))) {
      
      comb <- combinaison[i, ]
      
      tryCatch({
        # Selection d'une combinaison de noms des colonnes a tester
        select_col <- unlist(lapply(comb, as.character))
        
        # Selection et collecte des donnees d'interet avec dplyr
        selected_data <- tbl_duckdb %>%
          select(all_of(select_col)) %>%
          collect()
        
        colnames(selected_data) <- select_col
        
        if (nrow(selected_data) > 0) {
          # Stockage des donnees dans la liste
          result[[length(result) + 1]] <- selected_data
          print(paste(f, ":", paste0(select_col, collapse = " | ")))
          break
        }
      }, error = function(e) {
      })
    }
  }
  
  # Déconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(result)
}