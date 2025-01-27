#' @title Secretisation d'un tableau
#' @description
#' Cette fonction permet de respecter le secret statistique sur des tableaux de donnees
#' 
#' Sur la ligne, toutes les colonnes comprennent une valeur >= a la limite :
#' pas de secretisation des valeurs
#' 25   64   42   32   24   84   53
#' 25   64   42   32   24   84   53
#'
#' Sur la ligne, toutes les colonnes comprennent une valeur < a la limite :
#' secretisation des valeurs
#' 10   15   17   12   14   11   10
#' NA   NA   NA   NA   NA   NA   NA
#'
#' Sur la ligne, toutes les colonnes sauf une comprennent une valeur = 0  :
#' secretisation des valeurs
#'  0   64    0    0    0    0    0
#' NA   NA   NA   NA   NA   NA   NA
#'
#' Sur la ligne, toutes les colonnes sauf une comprennent une valeur >= a la limite :
#' secretisation de cette valeur et de la deuxieme plus petite valeur
#' 25   64   15   32   24   84   53
#' 25   64   NA   32   NA   84   53
#' 
#' @param data le tableau de donnees
#' @param vars le vecteur avec l'indice des variables a traiter dans 'data'
#' @param limit la valeur de l'effectif minimum de la secretisation
#' @param unique le boleen pour definir si l'on considere que l'ensemble des colonnes 
#' a secretiser forment ou non un tout ('TRUE'  si toutes les colonnes composent un tout
#  et 'FALSE' si les colonnes ne composent pas un tout)
#' 
#' @return 
#' La fonction renvoie un data.frame
#' 
#' @examples
#' secret_data(data = data, 
#'             vars = c(2:6),
#'             limit = 11,
#'             unique = TRUE)

secret_data <- function(data, 
                        vars,
                        limit = 11,
                        unique = TRUE) {
  
  # Fonction interne pour secretiser une ligne
  secret_row <- function(row) {
    
    # Conversion des valeurs en numerique
    values <- as.numeric(row)
    
    # Cas ou une seule colonne contient en realite tous les individus
    if (unique && length(values) > 1) {
      # Verification que toutes les colonnes sauf une contiennent un 0 ou un NA
      x <- which(values > 0 & !is.na(values))
      # Si une seule colonne contient une valeur non nulle, la secretiser
      if (length(x) == 1) {
        values[x] <- NA
      }
    }
    
    # Remplacement des valeurs inferieures a la limit par des valeurs NA
    values_secret <- ifelse(values < limit, NA, values)
    
    # Verification du nombre de valeurs secretisees
    secret_indices <- which(is.na(values_secret))
    
    if (length(secret_indices) == 1) {
      # Trouver l'indice de la deuxieme plus petite valeur
      values_no_secret <- values[!is.na(values_secret)]
      second_min <- min(values_no_secret)
      second_min_index <- which(values == second_min)[1]
      values_secret[second_min_index] <- NA
    }
    
    return(values_secret)
  }
  
  # Application de la secretisation ligne par ligne sur les colonnes specifiees
  dataframe[vars] <- t(apply(dataframe[vars], 1, secret_row))
  
  return(dataframe)
}