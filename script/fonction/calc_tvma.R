#' @title Transformation de fichiers .sas en fichiers .parquet
#' @description
#' Cette fonction permet de creer des fichiers .parquet a partir de fichiers .sas7bdat
#' 
#' @param sas_files le vecteur avec le chemin vers le ou les fichiers .sas
#' @param parquet_dir l'endoit ou sera cree le dossier qui contiendra les fichiers .parquet
#' @param chunk_size le nombre de lignes des fichiers .parquet
#' 
#' @return 
#' La fonction cree un dossier du meme nom que le fichier .sas traite avec a l'interieur
#' les fichiers .parquet crees (si le fichier .sas traite est compose de 100 lignes 
#' et que la taille des chunks est de 10, alors il y aura 10 fichiers .parquet)
#' 
#' @examples
#' sas_parquet(sas_files = c("test/parquet/data1.sas7bdat", 
#'                           "test/parquet/data2.sas7bdat"), 
#'             parquet_dir = "test/parquet/export/",
#'             chunk_size = 100000)

# Fonction pour calculer le TVAM entre deux data.frames identiques 
calcul_tvam <- function(dataframes, 
                        annees, 
                        id) {
  
  # Vérification que le nombre de dataframes correspond au nombre d'années données
  if (length(dataframes) != length(annees)) {
    stop("Le nombre de dataframes doit être égal au nombre d'années fournies.")
  }
  
  # Vérification que tous les dataframes contiennent la colonne identifiant spécifiée
  if (!all(sapply(dataframes, function(df) id %in% colnames(df)))) {
    stop("Tous les dataframes doivent contenir la colonne identifiant spécifiée.")
  }
  
  # Initialisation du dataframe final avec la colonne identifiant du premier dataframe
  df_resultat <- dataframes[[1]][, id, drop = FALSE]
  
  # Parcourir les paires consecutives de dataframes
  for (i in seq_along(dataframes)[-length(dataframes)]) {
    # Dataframes et années courantes
    df1 <- dataframes[[i]]
    df2 <- dataframes[[i + 1]]
    annee1 <- annees[i]
    annee2 <- annees[i + 1]
    
    # Nombre d'annees entre les deux périodes
    nb_years <- annee2 - annee1
    
    # Calcul des TVAM pour chaque variable (sauf colonne identifiant)
    tvam <- lapply(names(df1)[!names(df1) %in% id], function(var) {
      valeur1 <- df1[[var]]
      valeur2 <- df2[[var]]
      taux <- ifelse(valeur1 > 0,
                     round(((valeur2 / valeur1)^(1 / nb_years) - 1) * 100, 2),
                     NA) # NA si valeur initiale est 0
      return(taux)
    })
    
    # Convertir en dataframe
    df_tvam <- as.data.frame(tvam)
    colnames(df_tvam) <- paste0("tvam_", annee1, "_", annee2, "_", names(df1)[!names(df1) %in% id])
    
    # Ajouter au dataframe final
    df_resultat <- cbind(df_resultat, df_tvam)
  }
  
  return(df_resultat)
}