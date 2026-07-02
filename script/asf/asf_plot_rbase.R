#' @title Création d'un graphique base R sur la composition d'une typologie
#' @description
#' Version R base de la fonction \code{asf_plot_typa}, produisant un barplot empilé
#' représentant la répartition d'un groupe de variables dans une typologie.
#'
#' @param d data.frame
#' @param vars vecteur de caractères ou d’indices des colonnes à représenter
#' @param typo nom de la colonne de typologie
#' @param order.v,order.t ordres optionnels des variables et catégories
#' @param pal palette de couleurs (vecteur de codes hex)
#' @param taille logique : si TRUE, les hauteurs sont en effectifs, sinon en %
#' 
#' @export
asf_plot_typa_base <- function(d, 
                               vars, 
                               typo, 
                               order.v = NULL,
                               order.t = NULL,
                               pal = NULL,
                               taille = FALSE) {
  
  # SUB-FUNCTIONS -------------------------------------------------------------
  .calc_pct_varnum <- function(cat) {
    sub_data <- d[d[[typo]] == cat, vars, drop = FALSE]
    total <- colSums(sub_data, na.rm = TRUE)
    data.frame(
      category = cat,
      variable = names(total),
      freq = total,
      pct = (total / sum(total)) * 100,
      n = sum(total)
    )
  }
  
  .calc_pct_varcat <- function() {
    z <- as.data.frame(table(d[[typo]], d[[vars]]))
    names(z) <- c("category", "variable", "freq")
    z$n <- as.numeric(stats::ave(z$freq, z$category, FUN = sum))
    z$pct <- 100 * z$freq / z$n
    return(z)
  }
  
  # CHECK PARAMS --------------------------------------------------------------
  vars_names <- if (is.numeric(vars)) names(d)[vars] else vars
  unique_var <- length(vars_names) == 1 && is.character(d[[vars_names]])
  
  if (any(is.na(d[[typo]]))) {
    warning(paste0("des 'NA' sont présents dans la colonne '", typo, "' — ils seront ignorés"))
    d <- d[!is.na(d[[typo]]), , drop = FALSE]
  }
  
  if (!is.null(order.t)) {
    categories <- if (is.numeric(order.t)) unique(d[[typo]])[order.t] else order.t
  } else {
    categories <- unique(d[[typo]])
  }
  
  # PROCESSING ----------------------------------------------------------------
  if (unique_var) {
    z <- .calc_pct_varcat()
    vars_names <- unique(d[[vars_names]])
  } else {
    z <- do.call(rbind, lapply(categories, .calc_pct_varnum))
  }
  
  if (!is.null(order.v)) {
    vars_names <- if (is.numeric(order.v)) vars_names[order.v] else order.v
  }
  
  z$category <- factor(z$category, levels = categories)
  z$variable <- factor(z$variable, levels = vars_names)
  
  if (is.null(pal)) {
    noms_var <- sort(unique(z$variable))
    pal <- stats::setNames(grDevices::rainbow(length(noms_var)), noms_var)
  }
  
  yvar <- if (taille) "freq" else "pct"
  ylab <- if (taille) "Effectifs" else "Pourcentage (%)"
  
  # MATRICES POUR BARPLOT -----------------------------------------------------
  m_val <- tapply(z[[yvar]], list(z$variable, z$category), sum)
  m_val[is.na(m_val)] <- 0
  m_val <- as.matrix(m_val)
  
  # Pour les labels : on garde toujours les pourcentages internes
  m_pct <- tapply(z$pct, list(z$variable, z$category), sum)
  m_pct[is.na(m_pct)] <- 0
  m_pct <- as.matrix(m_pct)
  
  # AFFICHAGE -----------------------------------------------------------------
  op <- par(mar = c(5, 8, 4, 2))  # marges généreuses
  barpos <- barplot(m_val,
                    horiz = TRUE,
                    col = pal[rownames(m_val)],
                    las = 1,
                    border = "white",
                    xlab = ylab,
                    main = paste("Répartition", 
                                 if (unique_var) "de la variable" else "des variables", 
                                 "par", typo),
                    axes = FALSE)
  
  # Axe formaté sans notation scientifique
  axis(1, at = axTicks(1),
       labels = format(axTicks(1), big.mark = " ", scientific = FALSE))
  
  # Ajout des pourcentages dans les barres ------------------------------------
  cum <- apply(m_val, 2, cumsum)
  mid <- rbind(0, cum[-nrow(cum), ]) + m_val / 2
  
  for (i in seq_len(ncol(m_val))) {
    for (j in seq_len(nrow(m_val))) {
      if (m_val[j, i] > 0) {
        text(x = mid[j, i],
             y = barpos[i],
             labels = sprintf("%.1f", m_pct[j, i]),
             cex = 0.7)
      }
    }
  }
  
  legend("topright", legend = rownames(m_val), fill = pal[rownames(m_val)], bty = "n")
  par(op)
}
