#' @title Telechargement et lecture du maillage d'Aliette Roux
#' @description 
#' Cette fonction permet de telecharger et de lire les differents fichiers issus 
#' des travaux d’Aliette Roux portant sur la definition de tables de passages 
#' pour une geographie constante des IRIS (de 1943 a 2023) et sur la 
#' construction d’un maillage compose d’IRIS simples ou regroupes
#' 
#' @param md une chaine de caracteres correspondant a la maille d'origine sur 
#' laquelle est renseignee l'information que l'on souhaite traiter (iris ou 
#' commune)
#' @param ma une chaine de caracteres correspondant a la maille vers laquelle on
#' souhaite faire basculer cette information : soit une maille de reference 
#' ('irisf' ou 'comf') soit une maille d'entites agregees ('irisr' ou 'comr')
#' @param a2023 un booleen indiquant si l'information contenue dans les donnees
#' est dans la maille de reference
#' de reference ou non
#' @param geom un booleen indiquant si l’on souhaite lire le fond geographique 
#' avec les geometries des iris de reference
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' contenant les tables de passage des differents maillages si elles ont deja
#' ete telechargees, evitant ainsi l'utilisation d'une connexion internet
#' 
#' @return 
#' La fonction renvoie un data.frame, un objet sf ou une liste composee d'un 
#' data.frame et d'un objet sf
#'
#' @examples
#' result <- asf_mar(md = "iris",
#'                   ma = "comr")
#'
#' @export
asf_maa <- function(md = NULL,
                    ma = NULL,
                    a2023 = FALSE,
                    geom = FALSE,
                    dir = NULL) {
  
  # SUB-FUNCTIONS -------------------------------------------------------------
  .read_csv_verbose <- function(path, name) {
    message("lecture du fichier .csv ", name, " depuis le web : \n", path)
    utils::read.csv(path)
  }
  
  .read_sf_verbose <- function(path, name) {
    message("lecture du fichier .gpkg ", name, " depuis le web : \n", path)
    sf::st_read(path, quiet = TRUE)
  }
  
  .add_mayotte <- function(df, f_code, f_lib, r_code, r_lib) {
    myt <- df[df$OM_CODE == "MYT", ]
    myt[[r_code]] <- myt[[f_code]]
    names(myt)[which(names(myt) == f_lib)] <- r_lib
    return(myt)
  }
  
  .ixxxx_to_i2023  <- function() {
    d.irisf.pass <- .read_csv_verbose(path_d.irisf.pass, "d.irisf.pass")
    d.comf.app   <- .read_csv_verbose(path_d.comf.app, "d.comf.app")
    
    i_i2023 <- merge(d.irisf.pass, 
                     d.comf.app, 
                     by = "COMF_CODE")
    
    i_i2023 <- i_i2023[, c("IRIS_CODE", "IRISF_CODE", "IRISF_LIB", cols)]
    
    i_i2023[] <- lapply(i_i2023, as.character)
    i_i2023 <- i_i2023[order(i_i2023[[1]]), ]
    
    return(i_i2023)
  }
  .ixxxx_to_i2023r <- function() {
    d.irisr.pass <- .read_csv_verbose(path_d.irisr.pass, "d.irisr.pass")
    d.irisr.app  <- .read_csv_verbose(path_d.irisr.app, "d.irisr.app")
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                      d.irisr.pass[, c("IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB")],
                      by = "IRISF_CODE")
    i_i2023r <- merge(i_i2023r,
                      d.irisr.app[, -2],
                      by = "IRISrD_CODE")
    
    i_i2023r <- i_i2023r[, c("IRIS_CODE", "IRISrD_CODE", "IRISrD_LIB", cols)]
    
    # Ajout Mayotte
    myt <- .add_mayotte(i_i2023, "IRISF_CODE", "IRISF_LIB", "IRISrD_CODE", "IRISrD_LIB")
    myt <- myt[, colnames(i_i2023r)]
    i_i2023r <- rbind(i_i2023r, myt)
    
    i_i2023r[] <- lapply(i_i2023r, as.character)
    i_i2023r <- i_i2023r[order(i_i2023r[[1]]), ]
    
    return(i_i2023r)
  }
  .ixxxx_to_c2023  <- function() {
    d.irisf.pass <- .read_csv_verbose(path_d.irisf.pass, "d.irisf.pass")
    d.comf.app   <- .read_csv_verbose(path_d.comf.app, "d.comf.app")
    
    i_c2023 <- merge(d.irisf.pass, 
                     d.comf.app, 
                     by = "COMF_CODE")
    
    i_c2023 <- i_c2023[, c("IRIS_CODE", "COMF_CODE", "COMF_LIB", cols)]
    
    i_c2023[] <- lapply(i_c2023, as.character)
    i_c2023 <- i_c2023[order(i_c2023[[1]]), ]
    
    return(i_c2023)
  }
  .ixxxx_to_c2023r <- function() {
    i_c2023 <- .ixxxx_to_c2023()
    c_c2023r <- .cxxxx_to_c2023r()
    
    i_c2023$COM_CODE <- substr(i_c2023$IRIS_CODE, 1, 5)
    i_c2023 <- i_c2023[, c("IRIS_CODE", "COM_CODE", "COMF_CODE")]
    
    i_c2023r <- merge(i_c2023,
                      c_c2023r[, -2],
                      by = "COM_CODE")
    
    i_c2023r <- i_c2023r[, -1]
    
    i_c2023r[] <- lapply(i_c2023r, as.character)
    i_c2023r <- i_c2023r[order(i_c2023r[[1]]), ]
    
    return(i_c2023r)
  }
  
  .i2023_to_i2023r <- function() {
    d.irisr.pass <- .read_csv_verbose(path_d.irisr.pass, "d.irisr.pass")
    d.irisr.app  <- .read_csv_verbose(path_d.irisr.app, "d.irisr.app")
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                      d.irisr.pass[, c("IRISF_CODE", "IRISrS_CODE", "IRISrS_LIB", "IRISrD_CODE")],
                      by = "IRISF_CODE")
    i_i2023r <- merge(i_i2023r,
                      d.irisr.app[, -2],
                      by = "IRISrD_CODE")
    
    i_i2023r <- i_i2023r[, c("IRIS_CODE", "IRISrS_CODE", "IRISrS_LIB", cols)]
    
    # Ajout Mayotte
    myt <- .add_mayotte(i_i2023, "IRISF_CODE", "IRISF_LIB", "IRISrS_CODE", "IRISrS_LIB")
    myt <- myt[, colnames(i_i2023r)]
    i_i2023r <- rbind(i_i2023r, myt)
    
    i_i2023r[] <- lapply(i_i2023r, as.character)
    i_i2023r <- i_i2023r[order(i_i2023r[[1]]), ]
    
    return(i_i2023r)
  }
  .i2023_to_c2023  <- function() {
    i_i2023 <- .ixxxx_to_i2023()
    i_c2023 <- .ixxxx_to_c2023()
    
    i_c2023 <- i_c2023[i_c2023$IRIS_CODE == i_i2023$IRISF_CODE, ]
    
    return(i_c2023)
  }
  .i2023_to_c2023r <- function() {
    i_i2023 <- .ixxxx_to_i2023()
    i_c2023r <- .ixxxx_to_c2023r()
    
    i_c2023r <- i_c2023r[i_c2023r$IRIS_CODE == i_i2023$IRISF_CODE, ]
    
    return(i_c2023r)
  }
  
  .cxxxx_to_c2023  <- function() { 
    d.comf.app <- .read_csv_verbose(path_d.comf.app, "d.comf.app")
    d.comf.pass <- .read_csv_verbose(path_d.comf.pass, "d.comf.pass")
    
    c_c2023 <- merge(d.comf.pass[, -c(2, 5)], 
                     d.comf.app, 
                     by = "COMF_CODE", 
                     all.x = TRUE)
    
    names(c_c2023)[5] <- "COM_TYPE"
    c_c2023 <- c_c2023[, c("COM_CODE", "COM_TYPE", 
                           "COMF_CODE", "COMF_LIB", 
                           cols)]
    
    c_c2023[] <- lapply(c_c2023, as.character)
    c_c2023 <- c_c2023[order(c_c2023[[1]]), ]
    
    return(c_c2023)
  }
  .cxxxx_to_c2023r <- function() { 
    d.irisr.app <- .read_csv_verbose(path_d.irisr.app, "d.irisr.app")
    d.comf.pass <- .read_csv_verbose(path_d.comf.pass, "d.comf.pass")
    c_c2023 <- .cxxxx_to_c2023()
    
    id_list <- strsplit(d.irisr.app$COMF_CODE_MULTI, " \\| ")
    id_tabl <- data.frame(
      COMF_CODE = unlist(id_list),
      COMR_CODE = rep(d.irisr.app$COMF_CODE_MULTI, sapply(id_list, length))
    )
    id_tabl <- id_tabl[!duplicated(id_tabl$COMF_CODE), ]
    
    c_c2023r <- merge(d.comf.pass, 
                      id_tabl, 
                      by = "COMF_CODE",
                      all.x = TRUE)
    
    d.irisr.app <- d.irisr.app[, c(5:6, 10:25)]
    d.irisr.app <- d.irisr.app[!duplicated(d.irisr.app$COMF_CODE_MULTI), ]
    
    c_c2023r <- merge(c_c2023r, 
                      d.irisr.app, 
                      by.x = "COMR_CODE", 
                      by.y = "COMF_CODE_MULTI", 
                      all.x = TRUE)
    
    names(c_c2023r)[4] <- "COM_TYPE"
    names(c_c2023r)[7] <- "COMR_LIB"
    
    c_c2023r <- c_c2023r[, c("COM_CODE", "COM_TYPE", 
                             "COMR_CODE", "COMR_LIB", 
                             cols)]
    
    myt <- .add_mayotte(c_c2023, "COMF_CODE", "COMF_LIB", "COMR_CODE", "COMR_LIB")
    myt <- myt[, colnames(c_c2023r)]
    
    c_c2023r <- rbind(c_c2023r[!grepl("^976", c_c2023r$COM_CODE), ], myt)
    
    c_c2023r[] <- lapply(c_c2023r, as.character)
    c_c2023r <- c_c2023r[order(c_c2023r[[1]]), ]
    
    return(c_c2023r)
  }
  
  .c2023_to_c2023r <- function() {
    c_c2023 <- .cxxxx_to_c2023()
    c_c2023r <- .cxxxx_to_c2023r()
    
    c_c2023r <- c_c2023r[c_c2023r$COM_CODE == c_c2023$COMF_CODE, ]
    
    return(c_c2023r)
  }
  
  # PROCESSING ----------------------------------------------------------------
  # Definition des chemins selon le mode
  if (is.null(dir)) {
    path_sf.irisf <- "https://sharedocs.huma-num.fr/wl/?id=AMw46huJSZLVk1oqVx0MVmpuwxch0MZh&mode=grid&download=1"
    path_d.irisr.pass <- "https://sharedocs.huma-num.fr/wl/?id=vj5IeTHl913v84yYgZlLBUecgrZnnHZR&mode=grid&download=1"
    path_d.irisr.app  <- "https://sharedocs.huma-num.fr/wl/?id=sywlXWRph0cGfoRohzYSR8IGJWSAlYvx&mode=grid&download=1"
    path_d.irisf.pass <- "https://sharedocs.huma-num.fr/wl/?id=9rAif9O43umIs2cnEeMK4n1uMwS2F0Bl&mode=grid&download=1"
    path_d.comf.pass  <- "https://sharedocs.huma-num.fr/wl/?id=71exUwWdYhEOof3DJbg5ea1p89HrJwAl&mode=grid&download=1"
    path_d.comf.app   <- "https://sharedocs.huma-num.fr/wl/?id=6Wmy4MtiCxbMs8OUQ7Eht1X1dOlCLXcb&mode=grid&download=1"
  } else {
    path_sf.irisf <- file.path(dir, "sf.irisf.gpkg")
    path_d.irisr.pass <- file.path(dir, "d.irisr.pass.csv")
    path_d.irisr.app  <- file.path(dir, "d.irisr.app.csv")
    path_d.irisf.pass <- file.path(dir, "d.irisf.pass.csv")
    path_d.comf.pass  <- file.path(dir, "d.comf.pass.csv")
    path_d.comf.app   <- file.path(dir, "d.comf.app.csv")
  }
  
  # Definition des colonnes communes a toutes les tables
  cols <- c("OM_CODE", "EPCI", "NATURE_EPCI", "ARR", "CV", 
            "UU2020", "TUU2017", "TDUU2017", "BV2022", "ZE2020", 
            "AAV2020", "TAAV2017", "TDAAV2017", "CATEAAV2020", 
            "DEP", "REG")
  
  # Cas ou seul le fond geographique est telecharge
  if (is.null(md) && is.null(ma) && geom) {
    result <- .read_sf_verbose(path_sf.irisf, "sf.irisf")
    return(result)
  }
  
  # Traitement selon md / ma / a2023
  if (md == "iris" && !a2023) {
    if (ma == "irisf") result <- .ixxxx_to_i2023()
    else if (ma == "irisr") result <- .ixxxx_to_i2023r()
    else if (ma == "comf") result <- .ixxxx_to_c2023()
    else if (ma == "comr") result <- .ixxxx_to_c2023r()
    else stop("'ma' doit etre 'irisf', 'irisr', 'comf' ou 'comr'")
    
  } else if (md == "iris" && a2023) {
    if (ma == "irisr") result <- .i2023_to_i2023r()
    else if (ma == "comf") result <- .i2023_to_c2023()
    else if (ma == "comr") result <- .i2023_to_c2023r()
    else stop("'ma' doit etre 'irisr', 'comf' ou 'comr'")
    
  } else if (md == "com" && !a2023) {
    if (ma == "comf") result <- .cxxxx_to_c2023()
    else if (ma == "comr") result <- .cxxxx_to_c2023r()
    else stop("ma doit etre 'comf' ou 'comr'")
    
  } else if (md == "com" && a2023) {
    if (ma == "comr") result <- .c2023_to_c2023r()
    else stop("'ma' doit etre 'comr'")
    
  } else {
    stop("combinaison md/ma/a2023 non supportee")
  }
  
  if (geom) {
    sf_irisf <- .read_sf_verbose(path_sf.irisf, "sf.irisf")
    result <- list(
      tabl = result,
      geom = sf_irisf
    )
  }
  
  return(result)
}
