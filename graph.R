graph <- function(df, xvar, yvar) {
  # Convertir en facteur la variable yvar pour assurer les niveaux
  if (!is.factor(df[[yvar]])) {
    df[[yvar]] <- as.factor(df[[yvar]])
  }
  y_levels <- levels(df[[yvar]])
  n_y <- length(y_levels)
  
  # Convertion de la variable xvar en facteur (utile pour empiler par modalite)
  if (!is.factor(df[[xvar]])) {
    df[[xvar]] <- as.factor(df[[xvar]])
  }
  x_levels <- levels(df[[xvar]])
  
  # Creation d'un tableau de contingence : frequence de xvar par yvar
  tab <- table(df[[xvar]], df[[yvar]])
  
  # Attribution d'une couleur a chaque modalite de yvar
  y_colors <- setNames(rainbow(n_y), y_levels)
  
  # Fonction interne pour generer les coordonnees empilees
  get_stacked_points <- function() {
    x_coords <- c()
    y_coords <- c()
    colors <- c()
    
    for (x_level in rownames(tab)) {
      for (y_level in colnames(tab)) {
        count <- tab[x_level, y_level]
        if (count > 0) {
          # Position x : position dans les niveaux de xvar
          x_pos <- which(x_levels == x_level)
          x_coords <- c(x_coords, rep(x_pos, count))
          
          # Position y : position du y_level + sequence pour empilement
          y_pos <- which(y_levels == y_level)
          y_coords <- c(y_coords, y_pos + seq(-0.3, 0.3, length.out = count))
          
          # Couleur liee a la modalite yvar
          colors <- c(colors, rep(y_colors[y_level], count))
        }
      }
    }
    
    list(x = x_coords, y = y_coords, col = colors)
  }
  
  pts <- get_stacked_points()
  
  plot(
    x = pts$x,
    y = pts$y,
    pch = 20,
    col = pts$col,
    xaxt = "n",
    yaxt = "n",
    xlab = xvar,
    ylab = yvar,
    main = paste("Distribution de", xvar, "selon", yvar)
  )
  
  axis(1, at = 1:length(x_levels), labels = x_levels)
  axis(2, at = 1:n_y, labels = y_levels)
}
