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
#'                      cols = list(c("id", "ID"), 
#'                                  c("value1", "VALUE1"), 
#'                                  c("value2", "VALUE2")))

library(arrow)
library(duckdb)
library(dplyr)
library(dbplyr)

parquet_open <- function(dir, 
                         file, 
                         cols) {
  
  # Verification du param 'cols'
  mode_combo <- is.list(cols) && all(sapply(cols, is.character))
  mode_simpl <- is.character(cols) || is.numeric(cols)
  
  if (!mode_combo && !mode_simpl) {
    stop("'cols' doit etre un vecteur de noms ou d'indices de colonnes, ou une liste de combinaisons")
  }
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Initialisation de la liste pour stocker les donnees selectionnees
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
        stop(paste("aucun fichier .parquet dans le dossier :", folder_dir))
      }
    }
    
    # Creation d'une requete SQL
    query <- paste0(
      "CREATE OR REPLACE VIEW all_data AS ",
      paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
    )
    
    # Creation d'une vue SQL des fichiers .parquet agreges
    dbExecute(con, query)
    
    # Chargement de la vue en tant que table DuckDB
    tbl_duckdb <- tbl(con, "all_data")
    
    # Traitement differencie en fonction du mode de 'cols' --------------------
    if (mode_simpl) {
      if (is.numeric(cols)) {
        selected_cols <- colnames(tbl_duckdb)[cols]
      } else {
        selected_cols <- cols
      }
      
      # Selection des colonnes dans la table DuckDB
      df <- tbl_duckdb %>%
        select(all_of(selected_cols)) %>%
        collect() %>%
        as.data.frame()
      
      # Renommage des colonnes
      colnames(df) <- selected_cols
      
      # Ajout a la liste et message informatif
      result[[length(result) + 1]] <- df
      message(f, " : ", paste(selected_cols, collapse = " | "))
      
    } else if (mode_combo) {
      combinaison <- do.call(expand.grid, setNames(cols, paste0("col", seq_along(cols))))
      
      # Test des combinaisons jusqu'a trouver la bonne
      for (i in seq_len(nrow(combinaison))) {
        comb <- unlist(combinaison[i, ])
        
        tryCatch({
          # Selection des colonnes dans la table DuckDB
          df <- tbl_duckdb %>%
            select(all_of(comb)) %>%
            collect() %>%
            as.data.frame()
          
          # Renommage des colonnes
          colnames(df) <- comb
          
          # Si des donnees ont bien ete conservees, on garde cette version et on passe a la colonne suivante
          if (nrow(df) > 0) {
            result[[length(result) + 1]] <- df
            message(f, " : ", paste(comb, collapse = " | "))
            break
          }
        }, error = function(e) {})
      }
    }
  }
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(result)
}