#' @title Creation d'une carte discretisee
#' @description
#' Cette fonction permet de creer une carte avec une discretisation en Q6
#' 
#' @param data l'objet sf / data.frame 
#' @param tot le total
#' @param var la variable (partie du total)
#' @param breaks le nombre de quantiles pour la discretisation
#' @param zoomlabel l'objet sf comprenant le noms des zooms et leur emplacement
#' @param palette la palette de couleurs
#' 
#' @return
#' La fonction renvoie une carte
#' 
#' @examples
#' \dontrun{
#' map_q(data = data, 
#'       tot = "tot",
#'       var = "var")
#' }
#' @export

map_q <- function(data, 
                  tot,
                  var, 
                  breaks = "q6", 
                  zoomlabel = NULL,
                  palette = "rou") {
  
  # Verification des variables specifiees
  if (!all(c(tot, var) %in% names(data))) {
    stop("Les colonnes sp\u00e9cifi\u00e9es pour 'tot' et 'var' ne sont pas pr\u00e9sentes dans le dataframe.")
  }
  
  # Calcul du pourcentage de la variable
  data$ptc_var <- as.numeric(data[[var]] / data[[tot]] * 100)
  
  # Determination des breaks en fonction de l'option choisie
  if (breaks == "q6") {
    breaks <- mapsf::mf_get_breaks(data$ptc_var, breaks = "q6")
    leg_title <- "Discr\u00e9tisation en Q6"
    
    # Recuperation de la palette de couleurs
    pal <- asf_palette("simple", palette)
    
  } else if (is.numeric(as.numeric(breaks))) {
    breaks <- mapsf::mf_get_breaks(data$ptc_var, nbreaks = as.numeric(breaks), breaks = "quantile")
    leg_title <- paste("Discr\u00e9tisation en", breaks, "quantiles")
    
    # Recuperation de la palette de couleurs
    pal <- asf_palette("simple", palette)
    
  } else {
    stop("L'argument 'breaks' doit \u00eatre 'q6' ou un nombre entier.")
  }
  
  # Creation de la carte
  mapsf::mf_map(data, var = "ptc_var", breaks = breaks, type = "choro", 
                pal = pal,
                col_na = "#ffffff",
                border = NA,
                leg_pos = 'bottomleft',
                leg_title = leg_title,
                leg_horiz = TRUE,
                leg_val_rnd = 1,
                leg_box_border = NA,
                leg_box_cex = c(.8, 2))
  
  mapsf::mf_title("Part de la variable")
  
  # reg <- sf::st_as_sf(stats::aggregate(data$geometry, 
  #                                      by = list(data$DEP), 
  #                                      FUN = st_union))
  # mapsf::mf_map(reg,
  #               col = NA,
  #               border = "#ffffff", lwd = 0.1,
  #               add = TRUE)
  
  # Ajout des labels des zooms lorsqu'il y en a
  if (!is.null(zoomlabel)) {
    mapsf::mf_label(zoomlabel, var = "label", col = "#000000", font = 1)
  }
}