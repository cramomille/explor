#' @title Ouverture d'un tableau de donnees dans R a partir d'un fichier .parquet
#' @description
#' Cette fonction permet de selectionner des colonnes d'interet presentes dans un
#' fichier .parquet et de les charger dans R
#' 
#' @param dir le chemin vers le dossier qui contient les chunk.parquet du fichier
#' @param id le vecteur avec le nom de la colonne identifiant que l'on souhaite conserver
#' @param col le vecteur avec le nom de la colonne d'interet que l'on souhaite conserver
#' 
#' @return 
#' La fonction ouvre un objet data.frame
#' 
#' @examples
#' data <- open_parquet(dir = "test/parquet/export/data1/",
#'                      id = c("IRIS"),
#'                      col = c("P20_POP"))

library(arrow)
library(duckdb)
library(dplyr)

open_parquet <- function(dir, id, col) {
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Recuperation des noms de tous les fichiers .parquet
  parquet <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  
  # Creation d'une vue SQL pour agreger les fichiers .parquet
  query <- paste0(
    "CREATE OR REPLACE VIEW all_data AS ",
    paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
  )
  dbExecute(con, query)
  
  # Chargement de la vue comme table DuckDB
  tbl_duckdb <- tbl(con, "all_data")
  
  # Test de toutes les combinaisons possibles entre les differents noms
  combinaison <- expand.grid(id = id, col = col, stringsAsFactors = FALSE)
  
  # Creation du tableau final
  data <- data.frame()
  
  # Boucle pour tester les differentes combinaisons
  for (i in seq_len(nrow(combinaison))) {
    comb <- combinaison[i, ]
    tryCatch({
      
      # Selection et collecte des donnees d'interet avec dplyr
      data <- tbl_duckdb %>% 
        select(all_of(c(comb$id, comb$col))) %>% 
        collect()
      
      names(data) <- c(id[1], col[1])
      
      print(paste("combinaison :",
                  "id =", comb$id,
                  "/ col =", comb$col))
      break
    }, error = function(e) {
    })
  }
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(data)
}