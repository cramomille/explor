
#                            EXPLORATIONS POUR LE TRAITEMENT DES DONNEES DU CASD
#
#                                                                 antoine beroud
#                                                                   janvier 2025

library(haven)
library(arrow)
library(duckdb)
library(dplyr)












################################################################################
################################################################# CALCUL DE TVAM




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


