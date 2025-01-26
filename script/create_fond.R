#' @title Creation d'un fond de carte
#' @description
#' Cette fonction permet de creer un fond de carte avec un choix pour
#' l'utilisateur.ice du niveau de maillage et des zooms ajoutes ou non 
#' 
#' @param data un geo dataframe
#' @param vars la liste des variables
#' @param zoom creation d'un ou de zooms
#' @param cutzoom conservation du ou des zooms crees uniquement
#' @param villes la liste de noms de villes parmi celles pre-renseignees. 
#' Les villes disponibles sont "Paris","Bordeaux", "Grenoble", "Lille", "Lyon", 
#' "Marseille", "Montpellier", "Nantes", "Rennes", "Strasbourg" et "Toulouse".
#' @param long un vecteur de longitudes pour des zooms sur des lieux particuliers
#' @param lat un vecteur de latitudes pour des zooms sur des lieux particuliers
#' @param noms le vecteur des noms pour ces zooms particuliers
#' @param buffer la taille du rayon du zoom
#' @param k l'indice de grossissement du zoom
#' @param maillage le type de maillage choisi parmi une selection
#' @param cellsize la taille des carreaux de la grille si le choix du maillage est un carroyage
#' @param simply coefficient de simplification (entre 0 et 1)
#' @return 
#' La fonction renvoie une liste de deux objets sf. Le premier element sont les geom des zooms, 
#' le deuxième la position de lable pour les titres.
#' @examples
#' \dontrun{
#' data <- get_pop()
#' vars <- list(tot = "C20_POP15P",
#'              var = "C20_POP15P_CS6")
#' data <- prep_data()
#' create_fond(data = data,
#'             vars = names(vars),
#'             zoom = TRUE, cutzoom = FALSE, 
#'             villes = c("Paris","Bordeaux"),
#'             long = c(-1.151), lat = c(46.159), noms = c("La Rochelle"),
#'             buffer = 10000, k = 15,
#'             maillage = com, cellsize = NULL,
#'             vars = NULL)
#' }
#' @export
create_fond <- function(data,
                        vars = NULL,
                        zoom = FALSE, cutzoom = FALSE, 
                        villes = NULL, 
                        long = NULL, lat = NULL, noms = NULL, 
                        buffer = NULL, k = NULL,
                        maillage = NULL, cellsize = NULL,
                        simply = NULL) {
  
  
  # Tableau des villes predefinies ---------------------------------------------
  ville_geom <- data.frame(matrix(ncol = 1, nrow = 11))
  names(ville_geom) <- c("name") 
  
  # Nom de ces communes avec une aire d'attraction de plus de 700 000 habitants
  ville_geom$name <- c("Paris",
                       "Bordeaux", "Grenoble", "Lille", "Lyon", "Marseille", 
                       "Montpellier", "Nantes", "Rennes", "Strasbourg", "Toulouse"
  )
  
  # Coordonnees des centroides de ces communes
  ville_geom$long <- c(2.342,
                       -0.574, 5.721, 3.048, 4.836, 5.422, 
                       3.869, -1.548, -1.682, 7.768, 1.432
  )
  ville_geom$lat <- c(48.857, 
                      44.857, 45.182, 50.632, 45.755, 43.292, 
                      43.613, 47.232, 48.112, 48.571, 43.596
  )
  
  # Verification de la liste de variables
  if (is.null(vars) || length(vars) == 0) {
    stop("La liste des variables 'vars' doit \u00eatre spécifiée et non vide.")
  }
  
  # Creation des zooms ---------------------------------------------------------
  if (zoom) {
    if (is.null(data) || is.null(buffer) || is.null(k)) {
      stop("Les paramètres 'data', 'buffer' et 'k' doivent être spécifiés pour les zooms.")
    }
    
    # Creation des listes de longitudes et latitudes
    long_list <- c()
    lat_list <- c()
    name_list <- c()
    
    # Ajout des coordonnees des villes specifiees
    if (!is.null(villes)) {
      for (ville in villes) {
        if (ville %in% ville_geom$name) {
          long_list <- c(long_list, ville_geom$long[ville_geom$name == ville])
          lat_list <- c(lat_list, ville_geom$lat[ville_geom$name == ville])
          name_list <- c(name_list, ville)
        } else {
          warning(paste("La ville", ville, "n'est pas dans la liste des villes disponibles."))
        }
      }
    }
    
    # Ajout des coordonnees specifiees par l'utilisateur.ice
    if (!is.null(long) && !is.null(lat)) {
      long_list <- c(long_list, long)
      lat_list <- c(lat_list, lat)
      
      # Ajout des noms specifies par l'utilisateur.ice
      # S'il n'y en a pas utilisation d'un terme generique
      if (!is.null(noms)) {
        if (length(noms) < length(long)) {
          noms <- c(noms, rep("Coords spécifiées", length(long) - length(noms)))
        }
        name_list <- c(name_list, noms)
      } else {
        name_list <- c(name_list, rep("Coords spécifiées", length(long)))
      }
    }
    
    if (length(long_list) == 0 || length(lat_list) == 0)  {
      stop("Aucune coordonnée valide spécifiée pour les zooms.")
    }
    
    # Creation des zooms
    zoom_data <- create_zoom(data = data, 
                             long = long_list, lat = lat_list, 
                             names = name_list, 
                             buffer = buffer, k = k, 
                             cutzoom = cutzoom)
    
    zoom_iris <- zoom_data[[1]]
    zoom_boxes <- zoom_data[[2]]
    zoom_labels <- zoom_data[[3]]
    
    data <- zoom_iris
    
  } else {
    # Utilisation de la data de base pour la suite si zoom est FALSE
    data <- data
  }
  
  # Selection du maillage ------------------------------------------------------
  if (maillage == "iris") {
    
    # Reorganisation des colonnes
    data <- data[, c("CODE_IRIS", unlist(vars))]
    
  } else if (maillage == "com") {
    
    # Agregation des iris en commune
    data <- stats::aggregate(data[,c(unlist(vars))], 
                             by = list(data$COM), 
                             FUN = sum, na.rm = TRUE)
    
    colnames(data)[1] <- "CODE_COM"
    
  } else if (maillage == "epci") {
    # Agregation des iris en epci
    data <- stats::aggregate(data[,c(unlist(vars))], 
                             by = list(data$EPCI), 
                             FUN = sum, na.rm = TRUE)
    
    colnames(data)[1] <- "CODE_EPCI"
    
  } else if (maillage == "epci-iris") {    
    # Filtrage des iris presentes dans des epci ME, CU ou CA
    iris <- data[data$TYP_EPCI %in% c("ME", "CU", "CA"), ]
    epci <- data[!(data$TYP_EPCI %in% c("ME", "CU", "CA")), ]
    
    # Agregation des iris restantes en epci
    data <- stats::aggregate(epci[,c(unlist(vars))], 
                             by = list(epci$EPCI), 
                             FUN = sum, na.rm = TRUE)
    
    colnames(data)[1] <- "CODE_EPCI"
    
    # Recuperation des noms des colonnes communes
    cols <- c(names(data), "CODE_IRIS")
    iris <- iris[, names(iris) %in% cols]
    
    # Recuperation des colonnes uniques
    all_columns <- union(names(iris), names(data))
    
    # Ajout des colonnes manquantes avec des valeurs NA
    iris[setdiff(all_columns, names(iris))] <- NA
    data[setdiff(all_columns, names(data))] <- NA
    
    # Reorganisation des colonnes pour avoir le meme ordre
    iris <- iris[all_columns]
    data <- data[all_columns]
    
    # Regroupement des deux dataframes
    data <- rbind(iris, data)
    colnames(data)[1] <- "CODE_IRIS"
    data <- data[, c("CODE_IRIS", "CODE_EPCI", unlist(vars))]
    
  } else if (maillage == "carro") {
    if (is.null(cellsize)) {
      stop("Le paramètre 'cellsize' (en mètres) doit être spécifié lorsque le maillage est 'carroyage'.")
    }
    
    # Calcul de l'enveloppe des entites
    box <- sf::st_as_sfc(sf::st_bbox(data))
    
    # Creation du carroyage
    carroyage <- sf::st_as_sf(sf::st_make_grid(box, cellsize = cellsize))
    
    # Intersection pour ne conserver que les carreaux qui sont dans la zone d'etude
    carroyage$id <- 1:nrow(carroyage)
    carroyage <- sf::st_filter(carroyage, data, .predicate = sf::st_intersects)
    
    # Calcul de la surface des entites
    data <- sf::st_as_sf(data)
    data$area <- as.numeric(sf::st_area(data))
    
    # Decoupe des iris en fonction des carreaux du carroyage
    data_decoup <- sf::st_intersection(data, carroyage)
    
    # Calcul de la surface de chaque morceau d'iris
    data_decoup$areabou <- as.numeric(sf::st_area(data_decoup))
    
    # Attribution des donnees en fonction du taux de recouvrement de ces morceaux
    for (var in vars) {
      data_decoup[[var]] <- data_decoup[[var]] * (data_decoup$areabou / data_decoup$area)
    }
    
    data_decoup <- data_decoup[, c("id", unlist(vars))]
    
    # Suppression des geometries pour eviter un temps de calcul inutile
    data_decoup <- sf::st_set_geometry(data_decoup, NULL)
    
    # Agregation et calcul de la somme des variables
    carroAgreg <- stats::aggregate(x = data_decoup[, vars], 
                                   by = list(id = data_decoup$id), 
                                   FUN = sum, na.rm = TRUE)
    
    # Jointure avec les carreaux pour retrouver leur geometrie
    data <- merge(carroyage, carroAgreg, 
                  by.x = "id", by.y = "id", 
                  all.x = TRUE)
    
    colnames(data)[1] <- "CODE_CARRO"
    
  } else {
    stop("Maillage non valide. Les options sont : 
         'iris' (iris // 48 500),
         'com' (commune // 34 800),
         'epci' (epci // 1 200),
         'epci-iris' (epci et iris méthode A // 23 100),
         'carro' (carroyage)"
         )
  }
  
  # Simplification des geometries ----------------------------------------------
  if (!is.null(simply)) {
    if (!is.numeric(simply) || simply <= 0 || simply >= 1) {
      stop("Le paramètre 'simply' doit être un nombre entre 0 et 1.")
    }
    data <- rmapshaper::ms_simplify(data, keep = simply, keep_shapes = TRUE)
  }
  
  return(list(data, if (zoom) zoom_labels else NULL))
}