#' @title Ouverture d'un fichier .parquet dans R
#' @description
#' Cette fonction permet de selectionner des colonnes d'interet presentes dans un
#' fichier .parquet et de les charger dans R
#' 
#' @param dir le chemin vers le dossier qui contient les chunks .parquet du fichier
#' @param col le vecteur avec le nom des colonnes du fichier .parquet que l'on souhaite conserver
#' 
#' @return 
#' La fonction creer un objet data.frame
#' 
#' @examples
#' data <- open_parquet(dir = "test/parquet/export/data1/",
#'                      col = c("id", "value1"))

library(arrow)
library(duckdb)
library(dplyr)

open_parquet <- function(dir,
                         col) {
  
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
  
  # Selection et collecte des colonnes choisies avec dplyr
  data <- tbl_duckdb %>% 
    select(all_of(col)) %>% 
    collect()
  
  # Conversion en data.frame
  data <- as.data.frame(data)
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(data)
}