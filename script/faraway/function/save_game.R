#' @title Enregistrement des scores des joueuses lors d'une partie de Faraway
#'
#' @param scores une liste de vecteurs (entre 2 et 6) :
#'   - premier element : une chaine de caracteres correspondant au nom de la 
#'   joueuse
#'   - neuf autres elements : des valeurs numeriques correspondants aux scores 
#'   des 8 cartes (de a 8e a la 1ere) et a celui des cartes sanctuaires
#' @param dir chemin du dossier ou la partie est enregistree
#' @param id identifiant a utiliser pour remplacer une partie existante
#'
#' @return 
#' La fonction renvoie le chemin vers le fichier Rdata cree ou remplace lors de 
#' l'enregistrement et contenant les scores de la partie ainsi qu'un visuel sur 
#' les scores et les resultats totaux des joueuses
#' 
#' @examples
#' save_game(scores = list(
#'   c("antoine", 1, 3, 6, 9, 0, 0, 6, 10, 15), 
#'   c("rose", 5, 10, 5, 6, 10, 3, 4, 9, 5)
#' ))
#'        
#' @export
save_game <- function(scores, 
                      dir = "games_results", 
                      id = NULL) {
  
  # CHECK PARAMS --------------------------------------------------------------
  if (!is.list(scores)) stop("le parametre 'scores' doit etre une liste de vecteurs")
  if (length(scores) < 2 || length(scores) > 6) stop("il ne peut y avoir qu'entre 2 et 6 joueuses")
  
  for (i in seq_along(scores)) {
    v <- scores[[i]]
    
    # Verification de la longueur des vecteurs
    if (length(v) != 10) {
      stop("le vecteur de la joueuse ", i, " doit contenir exactement 1 nom et 9 scores")
    }
    
    # Verification du nom de la joueuse
    if (!is.character(v[1]) || length(v[1]) != 1) {
      stop("le premier element du vecteur ", i, " doit etre le nom de la joueuse")
    }
    
    # Verification des scores des joueuses
    vals <- suppressWarnings(as.numeric(v[-1]))
    if (any(is.na(vals))) {
      stop("les 9 scores de la joueuse ", v[1], " doivent etre numeriques")
    }
    if (any(vals < 0)) {
      stop("Les 9 scores de la joueuse ", v[1], " doivent etre positifs ou nuls")
    }
    if (any(vals %% 1 != 0)) {
      stop("Les 9 scores de la joueuse ", v[1], " doivent etre des entiers")
    }
  }
  
  # PROCESSING ----------------------------------------------------------------
  # Creation du dossier contenant les fichiers s'il n'existe pas
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # Transformation du vecteur en data.frame
  df <- do.call(rbind, lapply(scores, function(x) {
    data.frame(
      joueuse = as.character(x[1]),
      t(as.numeric(x[-1])),
      stringsAsFactors = FALSE
    )
  }))
  rownames(df) <- NULL
  
  # Renommage des colonnes des scores
  names(df) <- c("joueuse", "C8", "C7", "C6", "C5", "C4", "C3", "C2", "C1", "CS")
  
  # Ajout d'une colonne avec la date
  df <- cbind(date = as.Date(Sys.Date()), df)
  
  # Ajout de la colonne resultat
  df$total <- rowSums(df[, c(3:11)])
  
  # Verification des noms de joueuses precedemment enregistrees
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  joueuses <- character()
  if (length(fichiers) > 0) {
    for (f in fichiers) {
      e <- new.env()
      load(f, envir = e)
      joueuses <- union(joueuses, e$df$joueuse)
    }
    nouvelles <- setdiff(df$joueuse, joueuses)
    if (length(nouvelles) > 0) {
      message("nouvelles joueuses detectees : ", paste(nouvelles, collapse = ", "))
      answer <- readline("confirmer l'enregistrement ? (o/n) : ")
      if (tolower(answer) != "o") {
        stop("ENREGISTREMENT ANNULE")
      }
    }
  }
  
  # Gestion de l'identifiant
  # S'il n'y en a pas un de fourni, creation d'un identifiant
  if (is.null(id)) {
    if (length(fichiers) == 0) {
      id_num <- 1
    } else {
      ids <- sub("^(\\d{4})\\.RData$", "\\1", basename(fichiers))
      ids <- suppressWarnings(as.integer(ids))
      ids <- ids[!is.na(ids)]
      id_num <- ifelse(length(ids) == 0, 1, max(ids) + 1)
    }
    id <- sprintf("%04d", id_num)
  } else {
    # S'il y en a un de fourni, verification de son format
    if (!grepl("^\\d{4}$", id)) {
      stop("le parametre 'id' doit etre une chaine a 4 chiffres")
    }
    if (file.exists(file.path(dir, paste0(id, ".RData")))) {
      answer <- readline(
        paste0("ATTENTION : la partie ", id, " existe deja, confirmer l'ecrasement ? (o/n) : ")
      )
      if (tolower(answer) != "o") {
        stop("ENREGISTREMENT ANNULE")
      }
    } else {
      message("ATTENTION : la partie ", id, " va etre creee (pas d'ecrasement)")
    }
  }
  
  # Nommage final du fichier
  file <- file.path(dir, paste0(id, ".RData"))
  
  # Enregistrement et affichage dans la console
  save(df, file = file)
  print(df)
  
  return(file)
}