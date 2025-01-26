#' @title Preparation des donnees
#' @description
#' Cette fonction permet de combiner les donnees de l'utilisateur.ice au fond 
#' geographique des iris
#' 
#' @param data un tableau de donnees
#' @param vars une liste des variables du tableau de donnees
#' @param maillage le niveau du maillage des donnees
#' @param id colonne d'identifiant unique dans le tableau de donnees
#' @param arr presence ou non des arrondissememts dans le tableau de donnees
#' @param geo_file chemin vers le .gpkg d'un fond de carte des iris avec les DROM en place, par défaut celui de 2020
#' @param tab_file chemin vers le fichier du tableau de correspondance iris/com/epci
#' @return 
#' La fonction renvoie un geo dataframe a l'echelle des iris, 
#' des communes ou des epci avec les donnees de l'utilisateur.ice
#' 
#' @examples
#' \dontrun{
#' data <- get_pop()
#' vars <- list(tot = "C20_POP15P",
#'              var = "C20_POP15P_CS6")
#' prep_data(data = data, 
#'           vars = vars, 
#'           maillage = "iris",
#'           id = "IRIS", 
#'           arr = FALSE)
#' }
#' @export
prep_data <- function(data,
                      vars,
                      maillage = NULL, 
                      id = NULL, 
                      arr = FALSE, 
                      geo_file = NULL, tab_file = NULL) {
  
  # check fond de carte par defaut
  if(missing(geo_file)) {
    # alors on utilise le fond de 2020
    geo_file <- "fond/fond_prep_2020.gpkg"
    tab_file <- "fond/table_2020.xlsx"
    
  } else {
    geo_file <- geo_file
    tab_file <- tab_file
  }
  
  # Lecture et transformation des donnees geographiques des iris
  iris <- sf::st_read(geo_file, quiet = TRUE)
  iris <- sf::st_transform(iris, crs = "EPSG:2154")
  colnames(iris)[colnames(iris) == "INSEE_COM"] <- "COM"
  
  # Lecture de la table de correspondance
  tab1 <- data.frame(readxl::read_xlsx(tab_file, skip = 5, sheet = "COM"))
  tab2 <- data.frame(readxl::read_xlsx(tab_file, skip = 5, sheet = "ARM"))
  
  # Traitement de la table de correspondance
  col <- c("CODGEO", "EPCI", "NATURE_EPCI", "CV", "DEP", "REG")
  tab_arr <- rbind(tab1[, col], tab2[, col])
  tab_com <- tab1[, col]
  names(tab_arr) <- c("COM", "EPCI", "TYP_EPCI", "CAN", "DEP", "REG")
  names(tab_com) <- c("COM", "EPCI", "TYP_EPCI", "CAN", "DEP", "REG")
  
  # Selection des donnees des utilisateur.ices
  var <- data[, c(id, unlist(vars))]
  
  if (maillage == "iris") {
    # Jointure des iris avec la table de correspondance
    geo <- merge(iris, tab_arr, 
                 by.x = "COM", by.y = "COM", 
                 all.x = TRUE)
    
    # Jointure des iris avec les donnees
    data <- merge(geo, var, 
                  by.x = "CODE_IRIS", by.y = id, 
                  all.x = TRUE)
    
  } else if (maillage == "com") {
    if (arr){
      # Jointure des iris avec la table de correspondance
      geo <- merge(iris, tab_arr, 
                   by.x = "COM", by.y = "COM", 
                   all.x = TRUE)
      
    } else {
      # Homogeneisation des arrondissements de Paris, Marseille et Lyon
      iris$COM <- ifelse(grepl("^751", iris$COM), "75056", iris$COM)
      iris$COM <- ifelse(grepl("^132", iris$COM), "13055", iris$COM)
      iris$COM <- ifelse(grepl("^6938", iris$COM), "69123", iris$COM)
      
      # Jointure des iris avec la table de correspondance
      geo <- merge(iris, tab_com, 
                   by.x = "COM", by.y = "COM", 
                   all.x = TRUE)
    }
    
    # Agregation des codes de correspondance a l'echelle des communes
    tabGroup <- stats::aggregate(geo[, c("EPCI", "TYP_EPCI", "CAN", "DEP", "REG")], 
                                 by = list(geo$COM), 
                                 FUN = function(x) x[1])
    
    colnames(tabGroup)[1] <- "COM"
    
    # Agregation des iris en commune
    geo <- stats::aggregate(sf::st_drop_geometry(geo[, c("geometry")]), 
                            by = list(geo$COM), 
                            FUN = sum, 
                            na.rm = TRUE)
    
    colnames(geo)[1] <- "COM"
    
    # Jointure des communes avec leurs codes de correspondance
    geo <- merge(geo, tabGroup, 
                 by = "COM", 
                 all.x = TRUE)
    
    # Jointure des communes avec les donnees
    data <- merge(geo, var, 
                  by.x = "COM", by.y = id, 
                  all.x = TRUE)
    
    # Reconversion en un objet sf
    data <- sf::st_as_sf(data, sf_column_name = "geometry")
    
  } else if (maillage == "epci") {
    # Jointure des iris avec la table de correspondance
    geo <- merge(iris, tab_arr, 
                 by.x = "COM", by.y = "COM", 
                 all.x = TRUE)
    
    # Agregation des codes de correspondance a l'echelle des epci
    tabGroup <- stats::aggregate(geo[, c("TYP_EPCI", "CAN", "DEP", "REG")], 
                                 by = list(geo$EPCI), 
                                 FUN = function(x) x[1])
    
    colnames(tabGroup)[1] <- "EPCI"
    
    # Agregation des iris en epci
    geo <- stats::aggregate(sf::st_drop_geometry(geo[, c("geometry")]), 
                            by = list(geo$EPCI), 
                            FUN = sum, 
                            na.rm = TRUE)
    
    colnames(geo)[1] <- "EPCI"
    
    # Jointure des epci avec leurs codes de correspondance
    geo <- merge(as.data.frame(geo), as.data.frame(tabGroup), 
                 by = "EPCI",
                 all.x = TRUE)
    
    # Jointure des epci avec les donnees
    data <- merge(geo, var, 
                  by.x = "EPCI", by.y = id, 
                  all.x = TRUE)
    
    # Reconversion en objet sf
    data <- sf::st_as_sf(data, sf_column_name = "geometry")
  }
  
  # Renommage des colonnes data et conversion en valeurs numeriques
  for (var_name in names(vars)) {
    var_index <- which(colnames(data) == vars[[var_name]])
    colnames(data)[var_index] <- var_name
    data[[var_name]] <- as.numeric(data[[var_name]])
  }
  
  return(data)
}