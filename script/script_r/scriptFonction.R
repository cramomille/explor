# Fonction pour créer une carte en fonction du maillage et du paramètre de zoom
create_map <- function(maillage, zoom = FALSE, codereg = NULL) {
  # Sélection du maillage en fonction du paramètre
  if (maillage == "commune") {
    data <- commune
  } else if (maillage == "iris") {
    data <- iris
  } else if (maillage == "canton") {
    data <- canton
  } else if (maillage == "epci") {
    data <- epci
  } else {
    stop("Maillage non valide. Les options sont 'commune', 'iris', 'canton' ou 'epci'.")
  }
  
  # Filtrage par région si le paramètre de zoom est activé
  if (zoom) {
    if (is.null(codereg)) {
      stop("Le paramètre 'codereg' doit être spécifié lorsque le zoom est activé.")
    }
    data <- data[data$INSEE_REG == codereg, ]
  }
  
  # Création de la carte
  ggplot(data) +
    geom_sf() +
    theme_minimal()
}

# Exemple d'utilisation de la fonction
# Sans zoom
create_map("commune")

# Avec zoom
create_map("commune", zoom = TRUE, codereg = "11")