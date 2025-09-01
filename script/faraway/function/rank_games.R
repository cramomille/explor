#' @title Lecture des resultats de toutes les parties et creation d'un 
#' classement des parties et des joueuses
#'
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' ou les parties ont ete enregistrees grace a la fonction 'save_game()'
#' @param n une valeur numerique correspondant au nombre de resultats a afficher 
#' (Inf = tout)
#' @param j une chaine de caracteres correspondant au nom d'une joueuse pour ne 
#' prendre en compte que ses parties
#'
#' @return 
#' La fonction renvoie un data.frame avec le classement demande ainsi qu'un 
#' visuel dans la console
#' 
#' @examples
#' rank_games()
#' 
#' @export
rank_games <- function(dir = "games_results", 
                       n = Inf, 
                       j = NULL) {
  
  # CHECK PARAMS --------------------------------------------------------------
  if (!dir.exists(dir)) stop("le dossier ", dir, " n'existe pas")
  
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  if (length(fichiers) == 0) stop("aucune partie trouvee dans : ", dir)
  
  # PROCESSING ----------------------------------------------------------------
  dfs <- list()
  for (f in fichiers) {
    e <- new.env()
    load(f, envir = e)
    if (!exists("df", envir = e)) next
    dfs[[f]] <- e$df
  }
  
  all_df <- do.call(rbind, dfs)
  rownames(all_df) <- NULL
  
  # Tri par total decroissant
  classement <- all_df[order(-all_df$total), c("joueuse", "total")]
  
  # Ajout des rangs avec gestion des egalites
  classement$rang <- rank(-classement$total, ties.method = "min")
  classement <- classement[, c("rang", "joueuse", "total")]
  
  # Filtrage si un nom de joueuse est fourni
  if (!is.null(j)) {
    classement <- classement[classement$joueuse == j, , drop = FALSE]
    if (nrow(classement) == 0) {
      message("Aucune partie trouvee pour la joueuse : ", j)
      return(invisible(classement))
    }
  }
  
  # Limitation si 'n' est fourni
  if (is.finite(n) && n < nrow(classement)) {
    classement <- classement[seq_len(n), ]
  }
  
  # Affichage dans la console
  for (i in seq_len(nrow(classement))) {
    cat(sprintf("%02d : %s - %d\n", 
                classement$rang[i], 
                classement$joueuse[i], 
                classement$total[i]))
  }
  
  # Affichage de la distrubition des scores
  scores <- all_df$total
  min_s <- min(scores)
  max_s <- max(scores)
  
  h <- hist(scores)
  
  # Conversion des densites en %
  h$counts <- h$density * 100 * diff(h$breaks)[1]
  plot(h, 
       col = "lightblue", 
       border = "white",
       main = sprintf("Distribution des scores (min = %d, max = %d)", min_s, max_s),
       xlab = "Scores", 
       ylab = "Pourcentages")
  
  # Si une joueuse est renseignee, superposition en %
  if (!is.null(j)) {
    scores_j <- all_df$total[all_df$joueuse == j]
    if (length(scores_j) > 0) {
      h_j <- hist(scores_j,
                  breaks = h$breaks, # memes classes pour comparer
                  plot = FALSE)
      h_j$counts <- h_j$counts / length(scores_j) * 100
      plot(h_j, 
           col = rgb(1, 0, 0, 0.25), 
           border = "white", 
           add = TRUE)
      legend("topright",
             legend = c("all", j),
             fill = c("lightblue", rgb(1, 0, 0, 0.25)),
             border = "white")
    }
  }
  
  invisible(classement)
}