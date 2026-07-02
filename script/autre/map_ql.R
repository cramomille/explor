#' @title Creation d'une carte discretisee
#' @description
#' Cette fonction permet de creer une carte des quotients de localisation
#' 
#' @param data l'objet sf / data.frame
#' @param tot le total
#' @param var la variable (partie du total)
#' @param zoomlabel l'objet sf comprenant le noms des zooms et leur emplacement
#' @param palette la palette de couleurs
#' 
#' @return
#' La fonction renvoie une carte
#' 
#' @examples
#' \dontrun{
#' map_ql(data = data,
#'        tot = "tot",
#'        var = "var")
#' }
#' @export

map_ql <- function(data, 
                   tot,
                   var, 
                   zoomlabel = NULL,
                   palette = "rou") {
  
  # Verification des variables specifiees
  if (!all(c(tot, var) %in% names(data))) {
    stop("Les colonnes sp\u00e9cifi\u00e9es pour 'tot' et 'var' ne sont pas pr\u00e9sentes dans le dataframe.")
  }
  
  # Calcul du pourcentage de la variable
  data$pct_var <- data[[var]] / data[[tot]] * 100
  
  # Calcul de la moyenne de la variable sur l'aire d'étude
  mean_var <- sum(data[[var]], na.rm = TRUE) / 
    sum(data[[tot]], na.rm = TRUE) * 100
  
  # Calcul du quotient de localisation pour chaque entité
  data$ql_var <- data$pct_var / mean_var
  
  # Recuperation de la palette de couleurs
  pal <- asf_palette("double", palette)
  
  # Creation de la carte
  mapsf::mf_map(data, var = "ql_var", breaks = c(0, 0.7, 1.3, 2, Inf), type = "choro", 
                pal = pal,
                col_na = "#ffffff",
                border = NA,
                leg_pos = 'bottomleft',
                leg_title = "Quotient de localisation",
                leg_horiz = TRUE,
                leg_val_rnd = 1,
                leg_box_border = NA,
                leg_box_cex = c(.8, 2))
  
  mapsf::mf_title("Surrepr\u00e9sentation des variables")
  
  # Ajout des labels des zooms lorsqu'il y en a
  if (!is.null(zoomlabel)) {
    mapsf::mf_label(zoomlabel, var = "label", col = "#000000", font = 1)
  }
}