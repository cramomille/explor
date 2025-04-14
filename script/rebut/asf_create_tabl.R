#' @title Creation d'une table de passage standardisee
#' @description
#' Cette fonction permet de combiner les iris d'une annee donnee avec : la table 
#' des correspondances entre les communes et les autres mailles administratives
#' et un fichier sur la taille de la population par commune la meme annee
#' 
#' @param tabl_file le chemin du fichier "Table d'appartenance geographique des 
#' communes" de l'INSEE
#' @param popu_file le chemin du fichier de la population par commune
#' @param iris_file le chemin du fichier issue de la fonction create_fond
#' @param output_path le chemin du dossier ou l'on souhaite que le fichier .csv
#' soit ecrit
#' @param year l'annee de tous ces fichiers pour avoir cette information dans 
#' le nom du fichier .csv
#' @param arr le booleen pour definir si l'on considere les arrondissements des 
#' villes de Paris, Lyon et Marseille comme des communes ou non
#' 
#' @return 
#' La fonction renvoie un data.frame et ecrit un fichier .csv
#' 
#' @examples
#' \dontrun{
#' tabl <- create_tabl(tabl_file = "input/table-appartenance-geo-communes-20_zonages20.xlsx",
#'                     popu_file = "input/TD_POP1A_2020.csv",
#'                     iris_file = "output/fond_2020.gpkg",
#'                     output_path = "output",
#'                     year = "2020",
#'                     arr = FALSE)
#' }
#' @export

create_tabl <- function(tabl_file,
                        popu_file,
                        iris_file,
                        output_path,
                        year,
                        arr = FALSE) {
  
  # Traitement de la table de correspondance de l'INSEE ------------------------
  # Detection de l'extension du fichier
  file_extension <- tools::file_ext(tabl_file)
  
  # Lecture du fichier en fonction de son extension
  if (file_extension == "xlsx") {
    x1 <- data.frame(readxl::read_xlsx(tabl_file, skip = 5, sheet = "COM"))
    x2 <- data.frame(readxl::read_xlsx(tabl_file, skip = 5, sheet = "ARM"))
  } else if (file_extension == "xls") {
    x1 <- data.frame(readxl::read_xls(tabl_file, skip = 5, sheet = "COM"))
    x2 <- data.frame(readxl::read_xls(tabl_file, skip = 5, sheet = "ARM"))
  } else {
    stop("Le fichier doit \u00eatre au format xls ou xlsx.")
  }
  
  # Recuperation des codes qui nous interessent
  cols <- c("CODGEO", "EPCI", "NATURE_EPCI", "CV", "DEP", "REG")
  
  # Creation de la table avec arrondissements comme communes ou non
  if (arr) {
    x <- rbind(x1[, cols], x2[, cols])
  } else {
    x <- x1[, cols]
  }
  names(x) <- c("COM", "EPCI", "TYP_EPCI", "CAN", "DEP", "REG")
  
  # Traitement du fichier de population de l'INSEE -----------------------------
  pop <- utils::read.csv(popu_file, sep = ";", fileEncoding = "UTF-8")
  
  # Agregation pour avoir la population totale par commune
  pop <- stats::aggregate(NB ~ CODGEO, data = pop, FUN = sum)
  
  # Jointure avec la table de correspondance de l'INSEE pour avoir les codes EPCI
  pop <- merge(pop, x, by.x = "CODGEO", by.y = "COM")
  
  # Calcul de la population communale maximale dans chaque EPCI
  pop_max <- tapply(pop$NB, pop$EPCI, max)
  
  # Nouvelle colonne qui indique si la commune est la plus peuplee de son EPCI
  pop$com_popmax_epci <- pop$NB == pop_max[pop$EPCI]
  pop <- pop[, c(1, 8)]
  
  # Traitement sur le fond iris de l'INSEE -------------------------------------
  iris <- sf::st_read(iris_file)
  iris$geom <- NULL
  
  # Jointure des iris avec toutes les autres
  tabl <- merge(iris, x, by.x = "INSEE_COM", by.y = "COM", all.x = TRUE)
  tabl <- merge(tabl, pop, by.x = "INSEE_COM", by.y = "CODGEO", all.x = TRUE)
  
  # Reorganisation de l'ordre des colonnes
  tabl <- tabl[, c(4:6, 1:2, 7:12)]
  tabl$com_popmax_epci[is.na(tabl$com_popmax_epci)] <- FALSE
  
  # Definition des colonnes d'agregation des iris
  tabl$maille_iris <- ifelse(grepl("Arrondissement", tabl$NOM_COM) | tabl$TYP_EPCI %in% c("ME", "CU", "CA"), TRUE, FALSE)
  tabl$maille_comm <- ifelse(tabl$com_popmax_epci == TRUE & tabl$maille_iris == FALSE, TRUE, FALSE)
  tabl$maille_epci <- ifelse(tabl$maille_iris == FALSE & tabl$maille_comm == FALSE, TRUE, FALSE)
  
  # Creation des codes d'agregation des iris
  tabl$ce <- ifelse(tabl$maille_iris == TRUE | tabl$maille_comm == TRUE, tabl$INSEE_COM, tabl$EPCI)
  tabl$ice <- ifelse(tabl$maille_iris == TRUE, tabl$CODE_IRIS, 
                     ifelse(tabl$maille_comm == TRUE, tabl$INSEE_COM, 
                            ifelse(tabl$maille_epci == TRUE, tabl$EPCI, 
                                   NA)))
  
  # Nettoyage de la table
  tabl <- tabl[, c(1:10, 15, 16)]
  
  # Creation du chemin de sortie pour le fichier .rds
  output_file <- file.path(output_path, paste0("tabl_", year, ".csv"))
  
  # Export
  utils::write.csv(tabl, output_file, row.names = FALSE)
  
  # Affichage du chemin du fichier de sortie
  print(paste("Table de passage enregistr\u00e9e \u00e0 :", output_file))
  
  return(tabl)
}