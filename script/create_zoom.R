#' @title Creation de zooms
#' @description
#' Cette fonction permet de creer des zooms sur des lieux predefinis ou definis
#' par l'utilisateur.ice
#' 
#' @param data un geo dataframe
#' @param long la liste des longitudes
#' @param lat la liste des latitudes
#' @param names la liste des noms des lieux ou l'on zoome
#' @param buffer la taille du rayon du zoom
#' @param k l'indice de grossissement du zoom
#' @param cutzoom conservation du ou des zooms crees uniquement
#' 
#' @return 
#' La fonction permet de creer des zooms qui s'ajoutent au geo dataframe de base
#' 
#' @examples
#' \dontrun{
#' data <- get_pop()
#' create_zoom(data = data, 
#'             long = long_list, lat = lat_list, 
#'             names = c("Nantes","Rennes","Strasbourg"), 
#'             buffer = 10000, k = 15, 
#'             cutzoom = TRUE)
#' }
#' @export
create_zoom <- function(data, 
                        long, lat, names, 
                        buffer, k, 
                        cutzoom = FALSE) {
  
  nbCoords <- length(long)
  zooms <- list()
  circles <- list()
  labels <- list()
  
  # Creation d'un zoom pour chaque couple de coordonnees
  for (i in 1:nbCoords) {
    point <- data.frame(long = long[i], lat = lat[i])
    point <- sf::st_as_sf(point, coords = c("long", "lat"), crs = "EPSG:4326")
    point <- sf::st_transform(point, crs = "EPSG:2154")
    
    # Creation du buffer circulaire autour du point
    zoom <- sf::st_buffer(point, dist = buffer)
    
    # Decalage horizontal et vertical du zoom par rapport aux entites de base
    bbox_main <- sf::st_bbox(data)
    xmin <- bbox_main[1]
    ymin <- bbox_main[2]
    xmax <- bbox_main[3]
    ymax <- bbox_main[4]
    
    # Calcul des positions pour les zooms en colonnes de trois
    col_index <- (i - 1) %/% 3
    row_index <- (i - 1) %% 3
    
    # Ajustement automatique de l'espacement entre les zooms 
    # quels que soient leur taille (buffer) et leur indice de grossissement (k)
    offset_x <- xmax + ((xmax - xmin) * 0.1 * (1 + col_index)) + (col_index * (buffer * k * 2))
    offset_y <- ymax - ((xmax - xmin) * 0.1 * row_index) - (buffer * k * row_index * 2) -300000
    
    # Creation du zoom a cote de la carte principale et recuperation des donnees
    zoomed_data <- mapinsetr::move_and_resize(x = data, mask = zoom, xy = c(offset_x, offset_y), k = k, prj = "EPSG:2154")
    zooms[[i]] <- zoomed_data
    circles[[i]] <- zoom
    
    # Creation des labels avec les noms des zooms
    label_position <- sf::st_point(c(offset_x + buffer * k, offset_y + buffer * k *2.3))
    labels[[i]] <- sf::st_sf(label = names[i], geometry = sf::st_sfc(label_position, crs = sf::st_crs(zoomed_data)))
  }
  
  if (!cutzoom) {
    dataFinal <- mapinsetr::inset_rbinder(c(list(data), zooms))
  } else {
    dataFinal <- mapinsetr::inset_rbinder(zooms)
  }
  
  labels_sf <- do.call(rbind, labels)
  
  return(list(dataFinal, circles, labels_sf))
}