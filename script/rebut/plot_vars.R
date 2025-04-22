#' @title Creation d'un graphique sur la repartition de variables
#' @description
#' Cette fonction permet de creer un graphique sur la repartition de chaque
#' variable dans les categories d'une typologie 
#' 
#' @param data le tableau de donnees
#' @param vars le vecteur avec le nom des variables du tableau de donnees a traiter
#' @param typo le nom de la colonne contenant les categories de la typologie
#' @param order le vecteur avec le nom des categories de la typologie ou le numero
#' de leur position pour les ordonner comme on le souhaite
#' @param colors le vecteur avec les couleurs pour chaque variable
#' @param output_file le chemin du dossier ou l'on souhaite que le fichier .svg
#' soit ecrit si l'on souhaite l'exporter
#' 
#' @return
#' La fonction renvoie un graphique
#' 
#' @examples
#' \dontrun{
#' plot_vars(data = data,
#'           vars = c("va1", "va2"),
#'           typo = "taav",
#'           order = c(6, 3, 5, 4, 1, 2))
#' }
#' @export

plot_vars <- function(data,
                      vars,
                      typo,
                      order = NULL,
                      colors = NULL,
                      output_file = NULL) {
  
  # Verification des indices ou noms des colonnes 'vars' dans 'data'
  if (is.numeric(vars)) {
    vars <- names(data)[vars]
  } else if (is.character(vars)) {
    if (!all(vars %in% names(data))) {
      stop("Certaines colonnes sp\u00e9cifi\u00e9es dans 'vars' ne sont pas pr\u00e9sentes dans 'data'.")
    }
  } else {
    stop("'vars' doit \u00eatre un vecteur contenant des noms ou des indices de colonnes.")
  }
  
  # Definition des categories a partir des valeurs uniques de la colonne specifiee
  categories <- unique(data[[typo]])
  
  # Application d'un ordre pour les categories si specifie
  if (!is.null(order)) {
    if (is.numeric(order)) {
      # Si 'order' est un vecteur de numeros, on utilise les indices pour reordonner 'categories'
      if (all(order %in% seq_along(categories))) {
        categories <- categories[order]
      } else {
        stop("Les indices fournis dans 'order' doivent correspondre aux indices des cat\u00e9gories.")
      }
    } else if (is.character(order)) {
      # Si 'order' est un vecteur de noms de categories, on verifie leur presence et on les reordonne
      if (all(order %in% categories)) {
        categories <- order
      } else {
        stop("Certains noms fournis dans 'order' ne correspondent pas aux cat\u00e9gories.")
      }
    } else {
      stop("Le param\u00e8tre 'order' doit \u00eatre un vecteur de noms de cat\u00e9gories ou un vecteur num\u00e9rique.")
    }
  }
  
  # Si aucune couleur n'est specifiee, une palette par defaut est generee
  if (is.null(colors)) {
    colors <- stats::setNames(grDevices::colorRampPalette(c("#000000", "#f2f2f2"))(length(categories)), categories)
  }
  
  colors <- colors[categories]
  
  # Fonction pour calculer les pourcentages par categorie de la typo pour une variable donnee
  calc_pct <- function(var) {
    totvar <- sum(data[[var]], na.rm = TRUE)
    data.frame(
      variable = var,
      category = categories,
      pct = sapply(categories, function(cat) {
        sum(data[data[[typo]] == cat, ][[var]], na.rm = TRUE) / totvar * 100
      })
    )
  }
  
  # Calcul des pourcentages pour chaque variable et empilage des resultats
  y <- lapply(vars, calc_pct)
  z <- do.call(rbind, y)
  
  # Predefinition de l'ordre des categories et des variables pour le graphique
  z$category <- factor(z$category, levels = categories)
  z$variable <- factor(z$variable, levels = rev(vars))
  
  # Graphique avec ggplot2
  p <- ggplot2::ggplot(z, ggplot2::aes(x = variable, y = pct, fill = category)) +
       ggplot2::geom_bar(stat = "identity", position = "stack") +
       ggplot2::labs(title = paste("R\u00e9partition des variables par", typo),
                     x = "Variables",
                     y = "Pourcentage (%)") +
       ggplot2::scale_fill_manual(values = colors) +
       ggplot2::theme_minimal() +
       ggplot2::coord_flip()
  
  print(p)
  
  # Exportation si demandee
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot = p, width = 10, height = 8, device = "svg")
  }
}