#' @title Ouverture de plusieurs fichiers .parquet dans R
#' @description
#' Cette fonction permet de selectionner des colonnes presentes dans
#' des fichiers .parquet et de les charger dans R
#' 
#' @param dir le chemin vers le dossier qui contient les elements de 'file'
#' @param file le vecteur avec les noms des fichiers .parquet et/ou des dossiers 
#' qui contiennent les chunks .parquet
#' @param cols le vecteur avec le ou les noms des colonnes du fichier .parquet 
#' que l'on souhaite conserver
#' 
#' @return
#' La fonction creer un objet list contenant des objets data.frame
#' 
#' @examples
#' data <- parquet_open(dir = "test/parquet/export/",
#'                      file = c("data1", "data2", "data3_chunk01.parquet")
#'                      col = list(c("id", "ID"), 
#'                                 c("value1", "VALUE1"), 
#'                                 c("value2", "VALUE2")))

library(arrow)
library(duckdb)
library(dplyr)

parquet_open <- function(dir, 
                         file, 
                         cols) {
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Initialisation de la liste pour stocker les donnees
  result <- list()
  
  # Boucle pour chaque element de 'file' --------------------------------------
  for (f in file) {
    
    # Verification pour savoir si l'element est un fichier individuel ou un dossier
    if (grepl("\\.parquet$", f)) {
      parquet <- file.path(dir, f)
      
    } else {
      folder_dir <- file.path(dir, f)
      parquet <- list.files(folder_dir, pattern = "\\.parquet$", full.names = TRUE)
      
      if (length(parquet) == 0) {
        stop(paste("Aucun fichier .parquet dans le dossier:", folder_dir))
      }
    }
    
    # Creation d'une vue SQL pour agreger les fichiers .parquet
    query <- paste0(
      "CREATE OR REPLACE VIEW all_data AS ",
      paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
    )
    dbExecute(con, query)
    
    # Chargement de la vue en tant que table DuckDB
    tbl_duckdb <- tbl(con, "all_data")
    
    # Recuperation des noms des colonnes que l'on souhaite conserver
    cols_names <- paste0("col", seq_along(cols))
    
    # Generation de toutes les combinaisons de noms de colonnes possibles
    combinaison <- do.call(expand.grid, c(setNames(cols, cols_names)))
    
    # Boucle de test des differentes combinaisons possibles -------------------
    for (i in seq_len(nrow(combinaison))) {
      comb <- combinaison[i, ]
      
      tryCatch({
        # Selection de la combinaison de noms des colonnes a tester
        selected_cols <- unlist(lapply(comb, as.character))
        
        # Selection et collecte des colonnes choisies avec dplyr
        selected_data <- tbl_duckdb %>%
          select(all_of(selected_cols)) %>%
          collect()
        
        # Conversion en data.farme et rennomage des colonnes
        selected_data <- as.data.frame(selected_data)
        colnames(selected_data) <- selected_cols
        
        if (nrow(selected_data) > 0) {
          # Stockage des donnees dans la liste
          result[[length(result) + 1]] <- selected_data
          
          # Message d'information sur la combinaison utilisee
          print(paste(f, ":", paste0(selected_cols, collapse = " | ")))
          
          # Arret des test des que la combinaison valide est trouvee
          break
        }
      }, error = function(e) {
      })
    }
  }
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(result)
}