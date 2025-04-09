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
#' La fonction creer un objet data.frame
#' 
#' @examples
#' data <- open_parquet(dir = "test/parquet/export/data1/",
#'                      id = "ID",
#'                      vars = c("VALUE1"))

library(arrow)
library(duckdb)
library(dplyr)

open_parquet <- function(dir,
                         id, 
                         vars) {
  
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