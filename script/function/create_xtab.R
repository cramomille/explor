#' @title 
#' @description
#' 
#' @param
#' 
#' @return 
#' 
#' 
#' @examples
#' 

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
  
  # Conversion en data.frame
  out <- as.data.frame.matrix(tab)
  
  # Nommage des colonnes
  new_names <- paste0(var_col, "_", colnames(out))
  colnames(out) <- new_names
  
  # Ajout de la premiere colonne (var_row) qui etait dans les noms des lignes
  out[[var_row]] <- rownames(out)
  rownames(out) <- NULL
  
  # Reorganisation du tableau
  out <- out[, c(var_row, new_names)]
  
  return(out)
}