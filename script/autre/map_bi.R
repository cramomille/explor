#' @title Creation d'une carte bivariee
#' @description
#' Cette fonction permet de creer une carte bivariee a partir de deux variables
#' 
#' @param data l'objet sf / data.frame
#' @param tots le vecteur avec deux totaux
#' @param vars le vecteur avec deux variable (partie des totaux)
#' @param zoomlabel l'objet sf comprenant le noms des zooms et leur emplacement
#' @param palette la palette de couleurs
#' 
#' @return
#' La fonction renvoie une carte
#' 
#' @examples
#' \dontrun{
#' map_bi(data = data,
#'        tots = c("tot", "tot"),
#'        vars = c("va1", "va2"))
#' }
#' @export

map_bi <- function(data,
                   tots,
                   vars, 
                   zoomlabel = NULL,
                   palette = "rouver") {
  
  # Verification des variables specifiees
  if (!all(c(tots, vars) %in% names(data))) {
    stop("Les colonnes sp\u00e9cifi\u00e9es pour 'tots' et 'vars' ne sont pas pr\u00e9sentes dans le dataframe.")
  }
  
  # Calcul du pourcentage des variables
  data$pct_va1 <- data[[vars[1]]] / data[[tots[1]]] * 100
  data$pct_va2 <- data[[vars[2]]] / data[[tots[2]]] * 100
  
  # Calcul de la moyenne des variables sur l'aire d'etude
  mean_va1 <- sum(data[[vars[1]]], na.rm = TRUE) / sum(data[[tots[1]]], na.rm = TRUE) *100
  mean_va2 <- sum(data[[vars[2]]], na.rm = TRUE) / sum(data[[tots[2]]], na.rm = TRUE) *100
  
  # Calcul des quotients de localisation pour chaque entite
  data$ql_va1 <- as.numeric(data$pct_va1 / mean_va1)
  data$ql_va2 <- as.numeric(data$pct_va2 / mean_va2)
  
  # Definition des classes en fonction des coordonnees des entites sur le graph
  data$class <- with(data, ifelse(ql_va1 < 0.7 & ql_va2 < 0.7, "1",
                           ifelse(ql_va1 > 0.7 & ql_va1 < 1.3 & ql_va2 < 0.7, "2",
                           ifelse(ql_va1 > 1.3 & ql_va2 < 0.7, "3",
                                                
                           ifelse(ql_va1 < 0.7 & ql_va2 > 0.7 & ql_va2 < 1.3, "4",
                           ifelse(ql_va1 > 0.7 & ql_va1 < 1.3 & ql_va2 > 0.7 & ql_va2 < 1.3, "5",
                           ifelse(ql_va1 > 1.3 & ql_va2 > 0.7 & ql_va2 < 1.3, "6",
                                                               
                           ifelse(ql_va1 < 0.7 & ql_va2 > 1.3, "7",
                           ifelse(ql_va1 > 0.7 & ql_va1 < 1.3 & ql_va2 > 1.3, "8",
                           ifelse(ql_va1 > 1.3 & ql_va2 > 1.3, "9",
                                                                                          
                           NA))))))))))
  
  # Recuperation de la palette de couleurs
  pal <- asf_palette("triple", palette)
  
  # Creation de la carte
  mapsf::mf_map(data, var = "class", type = "typo",
                val_order = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                pal = pal,
                col_na = "#ffffff",
                border = NA)
  
  mapsf::mf_title("Surrepr\u00e9sentation des variables (QL)")
  
  # Ajout des labels des zooms lorsqu'il y en a
  if (!is.null(zoomlabel)) {
    mapsf::mf_label(zoomlabel, var = "label", col = "#000000", font = 1)
  }
}