#' @title Creation d'un cartogramme
#' @description
#' Cette fonction utilise les packages cartogram et cartogramR et permet de 
#' creer un cartogramme a partir d’un fond geographique et selon deux methodes
#' de deformation differentes (Dorling ou Gastner, Seguy et More)
#' 
#' @param f un objet sf
#' @param var une chaine de caracteres correspondant au nom de la colonne de 
#' \code{f} utilisee pour la deformation
#' @param method une chaine de caracteres correspondant a la methode de 
#' deformation choisie ("do" pour la methode de Dorling ou "gsm" pour la methode 
#' de Gastner, Seguy et More)
#' @param min une valeur numerique correspondant au seuil minimal de la colonne
#' \code{var} en dessous duquel les entites geographiques ne seront pas prises 
#' en compte lors de la deformation
#' 
#' @return
#' La fonction renvoie un objet sf
#' 
#' @examples
#' \dontrun{
#' asf_cartogram(f = fond,
#'               var = "pop",
#'               method = "do",
#'               min = 500)
#' }
#' @export
asf_cartogram <- function(f,
                          var, 
                          method = "gsm",
                          min = NULL) {
  
  # PACKAGE -------------------------------------------------------------------
  if (method == "do") {
    if (!requireNamespace("cartogram", quietly = TRUE)) {
      stop(paste0(
        "le package 'cartogram' est necessaire pour cette fonction",
        "installez le avec install.package(\"cartogram\") et vous pourrez utiliser la fonction"
      ), call. = FALSE)
    }
  } else {
    if (!requireNamespace("cartogramR", quietly = TRUE)) {
      stop(paste0(
        "le package 'cartogramR' est necessaire pour cette fonction",
        "installez le avec install.package(\"cartogramR\") et vous pourrez utiliser la fonction"
      ), call. = FALSE)
    }
  }
  
  # CHECK PARAMS --------------------------------------------------------------
  check_f <- deparse(substitute(f))
  check_var <- deparse(substitute(var))
  
  if (!var %in% names(f)) {
    stop("la colonne '", check_var, "' n'est pas presente dans '", check_f, "'", 
         call. = FALSE)
  }
  
  if (!method %in% c("do", "gsm")) {
    stop("la methode '", method, "' n'est pas reconnue, vous devez choisir entre 'do' (Dorling) et 'gsm' (Gastner, Seguy et More)", 
         call. = FALSE)
  }
  
  # PROCESSING ----------------------------------------------------------------
  # Forcage du type de la colonne en numerique
  f[[var]] <- as.numeric(f[[var]])
  
  # Suppression des valeurs NA
  f <- f[!is.na(f[[var]]), ]
  
  # Suppression des plus petites entites si precise
  if (!is.null(min)) {
    f <- f[f[[var]] >= min, ]
  }
  
  # Creation de la deformation
  if (method == "do") {
    result <- cartogram::cartogram_dorling(f, var, k = 1.75)
    
  } else {
    result <- cartogramR::cartogramR(f, count = var, method = "GastnerSeguyMore")
    result <- cartogramR::as.sf(result)
  }
  
  return(result)
}