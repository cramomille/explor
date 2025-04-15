#' @title 
#' @description
#' 
#' @param
#' 
#' @return 
#' 
#' 
#' @examples
#' 

# Fonction pour calculer le TVMA entre deux data.frames identiques 
calc_tvma <- function(dataframes, 
                      annees, 
                      id) {
  
  # Verification que le nombre de data.frames correspond au nombre d'annees donnees
  if (length(dataframes) != length(annees)) {
    stop("Le nombre de dataframes doit être égal au nombre d'années fournies.")
  }
  
  # Verification que tous les data.frames contiennent la colonne identifiant specifiee
  if (!all(sapply(dataframes, function(df) id %in% colnames(df)))) {
    stop("Tous les dataframes doivent contenir la colonne identifiant spécifiée.")
  }
  
  # Initialisation du data.frame final avec la colonne identifiant du premier data.frame
  result <- dataframes[[1]][, id, drop = FALSE]
  
  # Parcour des paires consecutives de data.frames
  for (i in seq_along(dataframes)[-length(dataframes)]) {
    # Data.frames et annees courantes
    df1 <- dataframes[[i]]
    df2 <- dataframes[[i + 1]]
    annee1 <- annees[i]
    annee2 <- annees[i + 1]
    
    # Nombre d'annees entre les deux periodes
    nb_years <- annee2 - annee1
    
    # Calcul des TVMA pour chaque variable (sauf colonne identifiant)
    tvam <- lapply(names(df1)[!names(df1) %in% id], function(var) {
      valeur1 <- df1[[var]]
      valeur2 <- df2[[var]]
      
      taux <- ifelse(valeur1 > 0,
                     round(((valeur2 / valeur1)^(1 / nb_years) - 1) * 100, 2),
                     NA) # NA si valeur initiale est 0
      return(taux)
    })
    
    # Convertion en data.frame
    df_tvam <- as.data.frame(tvam)
    colnames(df_tvam) <- paste0("tvam_", annee1, "_", annee2, "_", names(df1)[!names(df1) %in% id])
    
    # Ajout au dataframe final
    result <- cbind(result, df_tvam)
  }
  
  return(result)
}