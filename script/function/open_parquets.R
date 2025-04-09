#' @title Ouverture de plusieurs fichiers .parquet dans R
#' @description
#' Cette fonction permet de selectionner des colonnes d'interet presentes dans des
#' fichiers .parquet et de les charger dans R
#' 
#' @param dir le chemin vers le dossier qui contient les dossiers 'folder'
#' @param folder le vecteur avec les noms des dossiers qui contiennent les chunks .parquet
#' @param col le vecteur avec le ou les noms des colonnes du fichier .parquet que l'on souhaite conserver
#' 
#' @return 
#' La fonction creer un objet list contenant des objets data.frame
#' 
#' @examples
#' data <- open_parquets(dir = "test/parquet/export/",
#'                       folder = c("data1", "data2")
#'                       col = list(c("id", "ID"), 
#'                                  c("value1", "VALUE1"), 
#'                                  c("value2", "VALUE2")))

library(arrow)
library(duckdb)
library(dplyr)

open_parquets <- function(dir, 
                          folder, 
                          col) {
  
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
    col_names <- paste0("col", seq_along(col))
    
    # Generation de toutes les combinaisons de noms de colonnes possibles
    combinaison <- do.call(expand.grid, c(setNames(col, col_names)))
    
    # Boucle pour tester les differentes combinaisons possibles
    for (i in seq_len(nrow(combinaison))) {
      
      comb <- combinaison[i, ]
      
      tryCatch({
        # Selection d'une combinaison de noms des colonnes a tester
        select_col <- unlist(lapply(comb, as.character))
        
        # Selection et collecte des colonnes choisies avec dplyr
        selected_data <- tbl_duckdb %>%
          select(all_of(select_col)) %>%
          collect()
        
        # Conversion en data.farme et rennomage des colonnes
        selected_data <- as.data.frame(selected_data)
        colnames(selected_data) <- select_col
        
        # Stockage des donnees dans la liste
        if (nrow(selected_data) > 0) {
          result[[length(result) + 1]] <- selected_data
          print(paste(f, ":", paste0(select_col, collapse = " | ")))
          break # on arrete quand la comb valide est trouvee
        }
      }, error = function(e) {
      })
    }
  }
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(result)
}