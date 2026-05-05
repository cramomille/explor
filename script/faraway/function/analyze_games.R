#' @title Analyse des parties
#'
#' @param dir chemin vers le dossier contenant les parties sauvegardées
#' @param rang vecteur numerique de longueur 2 indiquant l'intervalle
#' de classement a conserver apres tri par score total decroissant
#' @param j nom d'une joueuse pour ne conserver que ses parties
#'
#' @return
#' Affiche un graphique contenant une courbe par partie.
#' Retourne invisiblement le tableau des parties conservees.
#'
#' @export
analyze_games <- function(dir = "games_results",
                          rang = c(1, Inf),
                          j = NULL) {
  
  # Chargement des parties
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  
  dfs <- list()
  for (f in fichiers) {
    e <- new.env()
    load(f, envir = e)
    if (!exists("df", envir = e)) next
    dfs[[f]] <- e$df
  }
  
  if (length(dfs) == 0) {
    message("aucune partie trouvee")
    return(invisible(NULL))
  }
  
  all_df <- do.call(rbind, dfs)
  rownames(all_df) <- NULL
  
  # Filtre joueuse
  if (!is.null(j)) {
    all_df <- all_df[all_df$joueuse == j, , drop = FALSE]
    if (nrow(all_df) == 0) {
      message("aucune partie trouvee pour la joueuse : ", j)
      return(invisible(NULL))
    }
  }
  
  # Colonnes cartes
  cartes <- setdiff(names(all_df), c("date", "joueuse", "total"))
  
  # Tri par score total décroissant
  all_df <- all_df[order(-all_df$total), ]
  
  # Intervalle
  debut <- max(1, rang[1])
  fin <- min(nrow(all_df), rang[2])
  
  if (debut > fin) {
    message("intervalle invalide")
    return(invisible(NULL))
  }
  
  sel_df <- all_df[debut:fin, , drop = FALSE]
  
  # Titre
  titre <- "Scores par partie"
  filtres <- c()
  
  if (!is.null(j)) {
    filtres <- c(filtres, j)
  }
  
  filtres <- c(filtres, paste0("rang ", debut, "-", fin))
  
  titre <- paste0(
    titre,
    " (",
    paste(filtres, collapse = ", "),
    ")"
  )
  
  # Axe x
  x <- seq_along(cartes)
  
  # Limites y
  y_max <- max(sel_df[, cartes], na.rm = TRUE)
  
  # Graphique vide
  plot(x, rep(0, length(x)),
       type = "n",
       ylim = c(0, y_max),
       xaxt = "n",
       xlab = "Carte",
       ylab = "Score",
       main = titre)
  
  axis(1, at = x, labels = cartes, las = 2)
  
  # Couleurs
  cols <- rainbow(nrow(sel_df))
  
  # Une courbe par partie
  for (i in seq_len(nrow(sel_df))) {
    y <- as.numeric(sel_df[i, cartes])
    
    lines(x, y,
          type = "b",
          col = cols[i],
          lwd = 2,
          pch = 16)
  }
  
  # Légende
  labels <- paste0(
    seq(debut, fin),
    " - ",
    sel_df$joueuse,
    " (",
    sel_df$total,
    ")"
  )
  
  legend("topleft",
         legend = labels,
         col = cols,
         lty = 1,
         lwd = 2,
         cex = 0.8)
  
  invisible(sel_df)
}