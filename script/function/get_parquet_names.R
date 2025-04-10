#' @title Recuperation des noms des colonnes d'un fichier .parquet
#' @description
#' Cette fonction permet de recuperer les noms des colonnes presentes dans un
#' fichier .parquet
#' 
#' @param dir le chemin vers le fichier .parquet ou le dossier qui contient les 
#' chunks .parquet
#' 
#' @return 
#' La fonction creer un vecteur
#' 
#' @examples
#' names <- get_parquet_names(dir = "test/parquet/export/data1/")

library(arrow)
library(duckdb)
library(dplyr)

get_parquet_names <- function(dir) {
  
  # Creation de la connexion a DuckDB pour executer des requetes SQL
  con <- dbConnect(duckdb())
  
  # Verification pour savoir si l'element est un fichier individuel ou un dossier
  if (grepl("\\.parquet$", dir)) {
    parquet <- file.path(dir)
    
  } else {
    parquet <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    
    if (length(parquet) == 0) {
      stop(paste("Aucun fichier .parquet dans le dossier:", dir))
    }
  }
  
  # Creation d'une vue SQL pour agreger les fichiers .parquet
  query <- paste0(
    "CREATE OR REPLACE VIEW all_data AS ",
    paste0("SELECT * FROM read_parquet('", parquet, "')", collapse = " UNION ALL ")
  )
  dbExecute(con, query)
  
  # Chargement de la vue comme table DuckDB
  tbl_duckdb <- tbl(con, "all_data")
  
  # Selection et collecte des noms des colonnes du fichier .parquet
  col_names <- colnames(tbl_duckdb)
  print(names)
  
  # Deconnexion de DuckDB
  dbDisconnect(con, shutdown = TRUE)
  
  return(col_names)
}