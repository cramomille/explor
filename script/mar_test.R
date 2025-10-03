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
#' result <- asf_mar(geom = TRUE)
#'
#' result <- asf_mar(md = "iris_xxxx", ma = "com_r2", geom = TRUE)
#'
#' @export
asf_maa <- function(md = NULL,
                    ma = NULL,
                    geom = FALSE,
                    dir = NULL) {
  
  # CHECK PARAMS --------------------------------------------------------------
  valid_conversions <- list(
    iris_xxxx = c("iris_f", "iris_r2", "com_f", "com_r2"),
    iris_2023 = c("iris_r2", "com_f", "com_r2"),
    com_xxxx  = c("com_f", "com_r2"),
    com_2023  = c("com_r2")
  )
  
  if (is.null(md) || !md %in% names(valid_conversions)) {
    stop("'md' doit etre une maille valide : ", paste(names(valid_conversions), collapse = ", "))
  }
  if (is.null(ma) || !ma %in% valid_conversions[[md]]) {
    stop("depuis la maille '", md, "' seules ces mailles sont possibles : ",
         paste(shQuote(valid_conversions[[md]], type = "sh"), collapse = ", "))
  }
  if (!is.logical(geom) || length(geom) != 1) {
    stop("'geom' doit etre un booleen (TRUE ou FALSE)")
  }
  if (!is.null(dir) && !dir.exists(dir)) {
    stop("'dir' doit etre NULL ou un chemin de dossier existant")
  }
  
  # SUB-FUNCTIONS -------------------------------------------------------------
  .cache <- new.env()
  
  .read_csv <- function(path) {
    name <- sub("^path_", "", deparse(substitute(path)))
    if (!exists(name, envir = .cache)) {
      message(name)
      .cache[[name]] <- utils::read.csv(path)
    }
    return(.cache[[name]])
  }
  
  .read_sf <- function(path) {
    name <- sub("^path_", "", deparse(substitute(path)))
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
  
  # IRIS d'une annee autre que 2023 vers les IRIS de 2023
  .ixxxx_to_i2023 <- function() {
    irisf.pass <- .read_csv(path_irisf.pass)
    comf.app <- .read_csv(path_comf.app)
    
    i_i2023 <- merge(irisf.pass, 
                     comf.app, 
                     by = "COMF_CODE")
    
    i_i2023 <- i_i2023[, c("IRIS_CODE", "IRISF_CODE", "IRISF_LIB", cols)]
    
    i_i2023[] <- lapply(i_i2023, as.character)
    i_i2023 <- i_i2023[order(i_i2023[[1]]), ]
    row.names(i_i2023) <- NULL
    
    return(i_i2023)
  }
  
  # IRIS d'une annee autre que 2023 vers les IRIS de 2023 regroupes
  .ixxxx_to_i2023r2 <- function() {
    irisr2.pass <- .read_csv(path_irisr2.pass)
    irisr2.app <- .read_csv(path_irisr2.app)
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r2 <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                       irisr2.pass[, c("IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB")],
                       by = "IRISF_CODE")
    
    i_i2023r2 <- merge(i_i2023r2,
                       irisr2.app[, -2],
                       by = "IRISrD_CODE")
    
    i_i2023r2 <- i_i2023r2[, c("IRIS_CODE", "IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB", cols)]
    
    # Ajout Mayotte
    myt <- .add_mayotte(i_i2023, "IRISF_CODE", "IRISF_LIB", "IRISrD_CODE", "IRISrD_LIB")
    myt <- myt[, colnames(i_i2023r2)]
    i_i2023r2 <- rbind(i_i2023r2, myt)
    
    i_i2023r2[] <- lapply(i_i2023r2, as.character)
    i_i2023r2 <- i_i2023r2[order(i_i2023r2[[1]]), ]
    row.names(i_i2023r2) <- NULL
    
    return(i_i2023r2)
  }
  
  # IRIS d'une annee autre que 2023 vers les communes de 2023
  .ixxxx_to_c2023 <- function() {
    irisf.pass <- .read_csv(path_irisf.pass)
    comf.app <- .read_csv(path_comf.app)
    
    i_c2023 <- merge(irisf.pass, 
                     comf.app, 
                     by = "COMF_CODE")
    
    i_c2023 <- i_c2023[, c("IRIS_CODE", "COMF_CODE", "COMF_LIB", cols)]
    
    i_c2023[] <- lapply(i_c2023, as.character)
    i_c2023 <- i_c2023[order(i_c2023[[1]]), ]
    row.names(i_c2023) <- NULL
    
    return(i_c2023)
  }
  
  # IRIS d'une annee autre que 2023 vers les communes de 2023 regroupees
  .ixxxx_to_c2023r2 <- function() {
    i_c2023 <- .ixxxx_to_c2023()
    c_c2023r2 <- .cxxxx_to_c2023r2()
    
    i_c2023$COM_CODE <- substr(i_c2023$IRIS_CODE, 1, 5)
    i_c2023 <- i_c2023[, c("IRIS_CODE", "COM_CODE", "COMF_CODE")]
    
    i_c2023r2 <- merge(i_c2023,
                       c_c2023r2[, -2],
                       by = "COM_CODE")
    
    i_c2023r2 <- i_c2023r2[, -1]
    
    i_c2023r2[] <- lapply(i_c2023r2, as.character)
    i_c2023r2 <- i_c2023r2[order(i_c2023r2[[1]]), ]
    row.names(i_c2023r2) <- NULL
    
    return(i_c2023r2)
  }
  
  # IRIS de 2023 vers les IRIS de 2023 regroupes
  .i2023_to_i2023r2 <- function() {
    irisr2.pass <- .read_csv(path_irisr2.pass)
    irisr2.app <- .read_csv(path_irisr2.app)
    i_i2023 <- .ixxxx_to_i2023()
    
    i_i2023r2 <- merge(i_i2023[, c("IRIS_CODE", "IRISF_CODE")],
                       irisr2.pass[, c("IRISF_CODE", "IRISrS_CODE", "IRISrS_LIB", "IRISrD_CODE")],
                       by = "IRISF_CODE")
    i_i2023r2 <- merge(i_i2023r2,
                       irisr2.app[, -2],
                       by = "IRISrD_CODE")
    
    i_i2023r2 <- i_i2023r2[, c("IRIS_CODE", "IRISF_CODE", "IRISrS_CODE", "IRISrS_LIB", cols)]
    
    # Ajout Mayotte
    myt <- .add_mayotte(i_i2023, "IRISF_CODE", "IRISF_LIB", "IRISrS_CODE", "IRISrS_LIB")
    myt <- myt[, colnames(i_i2023r2)]
    i_i2023r2 <- rbind(i_i2023r2, myt)
    
    i_i2023r2[] <- lapply(i_i2023r2, as.character)
    i_i2023r2 <- i_i2023r2[order(i_i2023r2[[1]]), ]
    row.names(i_i2023r2) <- NULL
    
    return(i_i2023r2)
  }
  
  # IRIS de 2023 vers les communes de 2023
  .i2023_to_c2023 <- function() {
    i_i2023 <- .ixxxx_to_i2023()
    i_c2023 <- .ixxxx_to_c2023()
    
    i_c2023 <- i_c2023[i_c2023$IRIS_CODE %in% i_i2023$IRISF_CODE, ]
    colnames(i_c2023)[1] <- "IRISF_CODE"
    row.names(i_c2023) <- NULL
    
    return(i_c2023)
  }
  
  # IRIS de 2023 vers les communes de 2023 regroupees
  .i2023_to_c2023r2 <- function() {
    i_i2023 <- .ixxxx_to_i2023()
    i_c2023r2 <- .ixxxx_to_c2023r2()
    
    i_c2023r2 <- i_c2023r2[i_c2023r2$IRIS_CODE %in% i_i2023$IRISF_CODE, ]
    colnames(i_c2023r2)[1] <- "IRISF_CODE"
    row.names(i_c2023r2) <- NULL
    
    return(i_c2023r2)
  }
  
  # Communes d'une annee autre que 2023 vers les communes de 2023
  .cxxxx_to_c2023 <- function() { 
    comf.app <- .read_csv(path_comf.app)
    comf.pass <- .read_csv(path_comf.pass)
    
    c_c2023 <- merge(comf.pass[, -c(2, 5)], 
                     comf.app, 
                     by = "COMF_CODE", 
                     all.x = TRUE)
    
    names(c_c2023)[5] <- "COM_TYPE"
    c_c2023 <- c_c2023[, c("COM_CODE", "COM_TYPE", 
                           "COMF_CODE", "COMF_LIB", 
                           cols)]
    
    c_c2023[] <- lapply(c_c2023, as.character)
    c_c2023 <- c_c2023[order(c_c2023[[1]]), ]
    row.names(c_c2023) <- NULL
    
    return(c_c2023)
  }
  
  # Communes d'une annee autre que 2023 vers les communes de 2023 regroupees
  .cxxxx_to_c2023r2 <- function() { 
    irisr2.app <- .read_csv(path_irisr2.app)
    comf.pass <- .read_csv(path_comf.pass)
    c_c2023 <- .cxxxx_to_c2023()
    
    id_list <- strsplit(irisr2.app$COMF_CODE_MULTI, " \\| ")
    id_tabl <- data.frame(
      COMF_CODE = unlist(id_list),
      COMR_CODE = rep(irisr2.app$COMF_CODE_MULTI, sapply(id_list, length))
    )
    id_tabl <- id_tabl[!duplicated(id_tabl$COMF_CODE), ]
    
    c_c2023r2 <- merge(comf.pass, 
                       id_tabl, 
                       by = "COMF_CODE",
                       all.x = TRUE)
    
    irisr2.app <- irisr2.app[, c(5:6, 10:25)]
    irisr2.app <- irisr2.app[!duplicated(irisr2.app$COMF_CODE_MULTI), ]
    
    c_c2023r2 <- merge(c_c2023r2, 
                       irisr2.app, 
                       by.x = "COMR_CODE", 
                       by.y = "COMF_CODE_MULTI", 
                       all.x = TRUE)
    
    names(c_c2023r2)[4] <- "COM_TYPE"
    names(c_c2023r2)[7] <- "COMR_LIB"
    
    c_c2023r2 <- c_c2023r2[, c("COM_CODE", "COM_TYPE", 
                               "COMF_CODE",
                               "COMR_CODE", "COMR_LIB", 
                               cols)]
    
    myt <- .add_mayotte(c_c2023, "COMF_CODE", "COMF_LIB", "COMR_CODE", "COMR_LIB")
    myt <- myt[, colnames(c_c2023r2)]
    
    c_c2023r2 <- rbind(c_c2023r2[!grepl("^976", c_c2023r2$COM_CODE), ], myt)
    
    c_c2023r2[] <- lapply(c_c2023r2, as.character)
    c_c2023r2 <- c_c2023r2[order(c_c2023r2[[1]]), ]
    row.names(c_c2023r2) <- NULL
    
    return(c_c2023r2)
  }
  
  # Communes de 2023 vers les communes de 2023 regroupees
  .c2023_to_c2023r2 <- function() {
    c_c2023 <- .cxxxx_to_c2023()
    c_c2023r2 <- .cxxxx_to_c2023r2()
    
    c_c2023r2 <- c_c2023r2[c_c2023r2$COM_CODE == c_c2023$COMF_CODE, ]
    colnames(c_c2023r2)[1] <- "COMF_CODE"
    row.names(c_c2023r2) <- NULL
    
    return(c_c2023r2)
  }
  
  # PROCESSING ----------------------------------------------------------------
  # Definition des chemins selon le mode
  if (is.null(dir)) {
    message("Lecture des fichiers depuis le web (huma-num) :")
    path_irisf             <- "https://sharedocs.huma-num.fr/wl/?id=AMw46huJSZLVk1oqVx0MVmpuwxch0MZh&mode=grid&download=1"
    path_irisr5.pass       <- "https://sharedocs.huma-num.fr/wl/?id=jwSNdmvykqbKaZAr21UlGcZMZd0pAfVX&mode=grid&download=1"
    path_irisr5.app.iris   <- "https://sharedocs.huma-num.fr/wl/?id=5mvRbwFkiV56UuNVXWPSeXLTmX2QmNjL&mode=grid&download=1"
    path_irisr5.app.irisr2 <- "https://sharedocs.huma-num.fr/wl/?id=4Zaj6aV0clEfzvKhGPah4pBxCLa6mJkY&mode=grid&download=1"
    path_irisr2.pass       <- "https://sharedocs.huma-num.fr/wl/?id=vj5IeTHl913v84yYgZlLBUecgrZnnHZR&mode=grid&download=1"
    path_irisr2.app        <- "https://sharedocs.huma-num.fr/wl/?id=sywlXWRph0cGfoRohzYSR8IGJWSAlYvx&mode=grid&download=1"
    path_irisf.pass        <- "https://sharedocs.huma-num.fr/wl/?id=9rAif9O43umIs2cnEeMK4n1uMwS2F0Bl&mode=grid&download=1"
    path_comf.pass         <- "https://sharedocs.huma-num.fr/wl/?id=71exUwWdYhEOof3DJbg5ea1p89HrJwAl&mode=grid&download=1"
    path_comf.app          <- "https://sharedocs.huma-num.fr/wl/?id=6Wmy4MtiCxbMs8OUQ7Eht1X1dOlCLXcb&mode=grid&download=1"
    
  } else {
    message("Lecture des fichiers depuis le dossier local : ", dir)
    path_irisf             <- file.path(dir, "sf.irisf.gpkg")
    path_irisr5.pass       <- file.path(dir, "d.irisr5.pass")
    path_irisr5.app.iris   <- file.path(dir, "d.irisr5.app.iris.csv")
    path_irisr5.app.irisr2 <- file.path(dir, "d.irisr5.app.irisr.csv")
    path_irisr2.pass       <- file.path(dir, "d.irisr.pass.csv")
    path_irisr2.app        <- file.path(dir, "d.irisr.app.csv")
    path_irisf.pass        <- file.path(dir, "d.irisf.pass.csv")
    path_comf.pass         <- file.path(dir, "d.comf.pass.csv")
    path_comf.app          <- file.path(dir, "d.comf.app.csv")
  }
  
  # Definition des colonnes communes a toutes les tables
  cols <- c("OM_CODE", "EPCI", "NATURE_EPCI", "ARR", "CV", 
            "UU2020", "TUU2017", "TDUU2017", "BV2022", "ZE2020", 
            "AAV2020", "TAAV2017", "TDAAV2017", "CATEAAV2020", 
            "DEP", "REG")
  
  # Cas ou seul le fond geographique est demande
  if (is.null(md) && is.null(ma) && geom) {
    result <- .read_sf(path_irisf)
    return(result)
  }
  
  # Traitement des tables de passages et d'appartennace
  if (md == "iris_xxxx" && ma == "iris_f")  result <- .ixxxx_to_i2023()
  if (md == "iris_xxxx" && ma == "iris_r2") result <- .ixxxx_to_i2023r2()
  if (md == "iris_xxxx" && ma == "com_f")   result <- .ixxxx_to_c2023()
  if (md == "iris_xxxx" && ma == "com_r2")  result <- .ixxxx_to_c2023r2()
  
  if (md == "iris_2023" && ma == "iris_r2") result <- .i2023_to_i2023r2()
  if (md == "iris_2023" && ma == "com_f")   result <- .i2023_to_c2023()
  if (md == "iris_2023" && ma == "com_r2")  result <- .i2023_to_c2023r2()
  
  if (md == "com_xxxx" && ma == "com_f")    result <- .cxxxx_to_c2023()
  if (md == "com_xxxx" && ma == "com_r2")   result <- .cxxxx_to_c2023r2()
  
  if (md == "com_2023" && ma == "com_r2")   result <- .c2023_to_c2023r2()
  
  # Ajout du fond en plus d'une table si les deux sont demandes
  if (geom) {
    sf_irisf <- .read_sf(path_irisf)
    result <- list(
      tabl = result,
      geom = sf_irisf
    )
  }
  
  return(result)
}
