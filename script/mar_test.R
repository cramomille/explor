#' @title Telechargement et lecture du maillage d'Aliette Roux
#' @description 
#' Cette fonction permet de telecharger et de lire les differents fichiers issus 
#' des travaux d’Aliette Roux portant sur la definition de tables de passages 
#' pour une geographie constante des IRIS (de 1943 a 2023) et sur la 
#' construction d’un maillage compose d’IRIS simples ou regroupes
#' 
#' @param md une chaine de caracteres correspondant a la maille d'origine 
#' (maille de depart)
#' @param ma une chaine de caracteres correspondant a la maille d'arrivee
#' @param geom un booleen indiquant si l’on souhaite lire le fond geographique 
#' avec les geometries des iris de reference
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' contenant les tables de passage des differents maillages si elles ont deja
#' ete telechargees
#' 
#' @return 
#' La fonction renvoie un data.frame, un objet sf ou une liste composee d'un 
#' data.frame et d'un objet sf
#'
#' @examples
#' result <- asf_mar(md = "iris_xxxx", ma = "com_r2")
#'
#' @export
asf_mar <- function(md = NULL,
                    ma = NULL,
                    geom = FALSE,
                    dir = NULL) {
  
  # CHECK PARAMS --------------------------------------------------------------
  valid_conversions <- list(
    iris_xxxx = c("iris_f", "iris_r2", "iris_r5", "com_f", "com_r2", "com_r5"),
    iris_2023 = c("iris_r2", "iris_r5", "com_f", "com_r2", "com_r5"),
    com_xxxx  = c("com_f", "com_r2", "com_r5"),
    com_2023  = c("com_r2", "com_r5"),
    iris_r2   = c("iris_r5", "com_r5"),
    com_r2    = c("com_r5")
  )
  
  if (is.null(md) || !md %in% names(valid_conversions)) {
    stop("'md' doit etre une maille valide : ", paste(names(valid_conversions), collapse = ", "))
  }
  if (is.null(ma) || !ma %in% valid_conversions[[md]]) {
    stop("Depuis '", md, "' seules ces mailles sont possibles : ",
         paste(valid_conversions[[md]], collapse = ", "))
  }
  if (!is.logical(geom) || length(geom) != 1) {
    stop("'geom' doit etre un booleen (TRUE ou FALSE)")
  }
  if (!is.null(dir) && !dir.exists(dir)) {
    stop("'dir' doit etre NULL ou un chemin de dossier existant")
  }
  
  # SUB-FUNCTIONS (récupérées de ta version) ----------------------------------
  .cache <- new.env()
  .read_csv <- function(path, name) {
    if (!exists(name, envir = .cache)) {
      message(name)
      .cache[[name]] <- utils::read.csv(path)
    }
    return(.cache[[name]])
  }
  .read_sf <- function(path, name) {
    if (!exists(name, envir = .cache)) {
      message(name)
      .cache[[name]] <- sf::st_read(path, quiet = TRUE)
    }
    return(.cache[[name]])
  }
  .add_mayotte <- function(df, f_code, f_lib, r_code, r_lib) {
    myt <- df[df$OM_CODE == "MYT", ]
    myt[[r_code]] <- myt[[f_code]]
    names(myt)[which(names(myt) == f_lib)] <- r_lib
    return(myt)
  }
  
  # TODO: compléter toutes tes sous-fonctions existantes (déjà dans ton code)
  # Exemple de nouveau bloc pour IRIS regroupés 5000
  .ixxxx_to_i2023r5 <- function() {
    d.irisr5.pass <- .read_csv(path_d.irisr5.pass, "d.irisr5.pass")
    d.irisr5.app  <- .read_csv(path_d.irisr5.app, "d.irisr5.app")
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r5 <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                       d.irisr5.pass,
                       by = "IRISF_CODE")
    i_i2023r5 <- merge(i_i2023r5, d.irisr5.app, by = "IRISR5_CODE")
    
    return(i_i2023r5)
  }
  
  .i2023_to_i2023r5 <- function() {
    d.irisr5.pass <- .read_csv(path_d.irisr5.pass, "d.irisr5.pass")
    d.irisr5.app  <- .read_csv(path_d.irisr5.app, "d.irisr5.app")
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r5 <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                       d.irisr5.pass,
                       by = "IRISF_CODE")
    i_i2023r5 <- merge(i_i2023r5, d.irisr5.app, by = "IRISR5_CODE")
    
    return(i_i2023r5)
  }
  
  # PROCESSING (simplifié) ----------------------------------------------------
  # --> Ici tu gardes ton gros bloc PROCESSING existant
  # --> et à la fin tu remplaces tous les tests (md / ma / a2023) 
  # par une seule sélection claire :
  
  if (md == "iris_xxxx" && ma == "iris_f") result <- .ixxxx_to_i2023()
  if (md == "iris_xxxx" && ma == "iris_r2") result <- .ixxxx_to_i2023r()
  if (md == "iris_xxxx" && ma == "iris_r5") result <- .ixxxx_to_i2023r5()
  if (md == "iris_xxxx" && ma == "com_f")  result <- .ixxxx_to_c2023()
  if (md == "iris_xxxx" && ma == "com_r2") result <- .ixxxx_to_c2023r()
  if (md == "iris_xxxx" && ma == "com_r5") result <- .ixxxx_to_c2023r5() # à coder
  
  if (md == "iris_2023" && ma == "iris_r2") result <- .i2023_to_i2023r()
  if (md == "iris_2023" && ma == "iris_r5") result <- .i2023_to_i2023r5()
  if (md == "iris_2023" && ma == "com_f")  result <- .i2023_to_c2023()
  if (md == "iris_2023" && ma == "com_r2") result <- .i2023_to_c2023r()
  if (md == "iris_2023" && ma == "com_r5") result <- .i2023_to_c2023r5() # à coder
  
  if (md == "com_xxxx" && ma == "com_f")  result <- .cxxxx_to_c2023()
  if (md == "com_xxxx" && ma == "com_r2") result <- .cxxxx_to_c2023r()
  if (md == "com_xxxx" && ma == "com_r5") result <- .cxxxx_to_c2023r5() # à coder
  
  if (md == "com_2023" && ma == "com_r2") result <- .c2023_to_c2023r()
  if (md == "com_2023" && ma == "com_r5") result <- .c2023_to_c2023r5() # à coder
  
  if (md == "iris_r2" && ma == "iris_r5") result <- .ir2_to_ir5()   # à coder
  if (md == "iris_r2" && ma == "com_r5")  result <- .ir2_to_comr5() # à coder
  
  if (md == "com_r2" && ma == "com_r5")   result <- .cr2_to_cr5()   # à coder
  
  # Ajout du fond
  if (geom) {
    sf_irisf <- .read_sf(path_sf.irisf, "sf.irisf")
    result <- list(
      tabl = result,
      geom = sf_irisf
    )
  }
  
  return(result)
}
