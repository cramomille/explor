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
#'   - mean   : score moyen pour la carte
#'   - median : score median pour la carte
#'   - min    : score minimal pour la carte
#'   - max    : score maximal pour la carte
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
  
  # Calcul des statistiques par carte
  mean <- colMeans(top_df[, cartes, drop = FALSE])
  median <- apply(top_df[, cartes, drop = FALSE], 2, median)
  min <- apply(top_df[, cartes, drop = FALSE], 2, min)
  max <- apply(top_df[, cartes, drop = FALSE], 2, max)
  
  # Construction du tableau de resultat
  result <- data.frame(
    card   = names(mean),
    mean   = round(mean, 0),
    median = round(median, 0),
    min    = round(min, 0),
    max    = round(max, 0),
    row.names = NULL
  )
  
  # Affichage
  print(result, row.names = FALSE)
  
  # GRAPHIQUE -----------------------------------------------------------------
  # Construction du titre en fonction des filtres
  titre <- "Scores par carte"
  filtres <- c()
  if (!is.null(j)) filtres <- c(filtres, j)
  if (!is.null(n) && is.finite(n)) filtres <- c(filtres, paste0("top ", n))
  if (length(filtres) > 0) titre <- paste(titre, "(", paste(filtres, collapse = ", "), ")")
  
  x <- 1:9
  cols <- c("mean" = "#ea5153", "median" = "#8292ca", "min" = "#fdc543", "max" = "#5cb885")
  
  # Creation du graphique
  plot(x, result$mean, type = "n", ylim = c(0, 25),
       xaxt = "n", xlab = "Carte", ylab = "Score",
       main = titre)
  axis(1, at = x, labels = result$card, las = 2)
  
  # Creation des courbes
  lines(x, result$mean, type = "b", col = cols["mean"], lwd = 2, pch = 16)
  lines(x, result$median, type = "b", col = cols["median"], lwd = 2, pch = 17)
  lines(x, result$min, type = "b", col = cols["min"], lwd = 2, pch = 15)
  lines(x, result$max, type = "b", col = cols["max"], lwd = 2, pch = 18)
  
  # Ajout de la legende
  legend("topleft", legend = names(cols), col = cols, lty = 1, lwd = 2, pch = 16)
  
  invisible(result)
}