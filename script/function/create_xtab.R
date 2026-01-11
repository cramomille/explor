#' @title Creation d'un tableau croise
#'
#' @description
#' Construit un tableau croise entre deux variables categorielles d'un 
#' data.frame, eventuellement pondere
#'
#' @param d le data.frame contenant les variables a croiser
#' @param var_row le nom de la variable utilisee pour les lignes du tableau
#' @param var_col le nom de la variable utilisee pour les colonnes du tableau
#' @param weight le nom de la variable de ponderation (si `NULL`, une 
#' ponderation uniforme de 1 est utilisee pour chaque ligne)
#'
#' @return 
#' La fonction renvoie un data.frame
#'
#' @examples
#' create_xtab(d, "csp", "nat")
#' create_xtab(d, "csp", "nat", weight = "w")

create_xtab <- function(d, 
                        var_row, 
                        var_col, 
                        weight = NULL) {

  rowv <- d[[var_row]]
  colv <- d[[var_col]]
  
  # Gestion des NA dans les variables
  rowv <- addNA(rowv)
  colv <- addNA(colv)
  levels(rowv)[is.na(levels(rowv))] <- "NA"
  levels(colv)[is.na(levels(colv))] <- "NA"
  
  # Ponderation
  if (is.null(weight)) {
    w <- rep(1, nrow(d))
  } else {
    w <- d[[weight]]
    w[is.na(w)] <- 0
  }
  
  # Creation du tableau croise
  tab <- tapply(w, list(rowv, colv), sum, default = 0)
  
  # Suppression eventuelle de la ligne "NA"
  if ("NA" %in% rownames(tab)) {
    if (all(tab["NA", ] == 0)) {
      tab <- tab[rownames(tab) != "NA", , drop = FALSE]
    }
  }
  
  # Suppression eventuelle de la colonne "NA"
  if ("NA" %in% colnames(tab)) {
    if (all(tab[, "NA"] == 0)) {
      tab <- tab[, colnames(tab) != "NA", drop = FALSE]
    }
  }
  
  # Conversion en data.frame
  out <- as.data.frame.matrix(tab)
  
  # Nommage des colonnes
  new_names <- paste0(var_col, "_", colnames(out))
  colnames(out) <- new_names
  
  # Ajout de la premiere colonne (var_row) qui etait dans les noms des lignes
  out[[var_row]] <- rownames(out)
  rownames(out) <- NULL
  
  # Reorganisation du tableau
  result <- out[, c(var_row, new_names)]
  
  return(result)
}