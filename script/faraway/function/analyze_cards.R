#' @titles Analyse des scores par carte
#'
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' ou les parties ont ete enregistrees grace a la fonction 'save_game()'
#' @param n une valeur numerique correspondant au nombre de meilleures parties a 
#' retenir (Inf = toutes)
#' @param j une chaine de caracteres correspondant au nom d'une joueuse pour ne 
#' prendre en compte que ses parties
#'
#' @return 
#' La fonction affiche :
#'   - mean_scores : scores moyens par carte sur les 'n' meilleures parties
#'   - best_scores : meilleurs scores par carte sur les 'n' meilleures parties
#'   
#'@examples
#'
#' 
#' @export
analyze_cards <- function(dir = "games_results", 
                          n = Inf, 
                          j = NULL) {
  
  # Chargement des parties enregistrees
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  dfs <- list()
  for (f in fichiers) {
    e <- new.env()
    load(f, envir = e)
    if (!exists("df", envir = e)) next
    dfs[[f]] <- e$df
  }
  all_df <- do.call(rbind, dfs)
  rownames(all_df) <- NULL
  
  # Filtrage si une joueuse est specifiee
  if (!is.null(j)) {
    all_df <- all_df[all_df$joueuse == j, , drop = FALSE]
    if (nrow(all_df) == 0) {
      message("aucune partie trouvee pour la joueuse : ", j)
      return(invisible(NULL))
    }
  }
  
  # Conservation des colonnes contenant les scores
  cartes <- setdiff(names(all_df), c("date", "joueuse", "total"))
  
  # Conservation des 'n' meilleures parties
  top_df <- all_df[order(-all_df$total), ]
  if (is.finite(n) && n < nrow(top_df)) {
    top_df <- top_df[seq_len(n), ]
  }
  
  # Definition du score moyen par carte (sur les parties retenues)
  mean_scores <- colMeans(top_df[, cartes, drop = FALSE])
  
  # Definition du meilleur score par carte (sur les parties retenues)
  best_scores <- apply(top_df[, cartes, drop = FALSE], 2, max)
  
  # Construction du tableau de resultat
  result <- data.frame(
    card = names(mean_scores),
    mean = round(mean_scores, 0),
    best = round(best_scores, 0),
    row.names = NULL
  )
  
  # Affichage
  print(result, row.names = FALSE)
  
  invisible(result)
}