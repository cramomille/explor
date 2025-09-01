
#                                   BIDOUILLAGES ET MODELISATION DES MECANISMES 
#                                               DU CAPITALISME SUR LES SOCIETES
#                                                                              
#                                                            cramomille petibon
#                                                                  juillet 2025


###############################################################################
init_world <- function(n = 1000, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  digits <- nchar(as.character(n)) + 1
  
  data.frame(
    id = sprintf(paste("%0", digits, "d"), 1:n),
    ag = sample(0:90, n, replace = TRUE, prob = dnorm(0:90, mean = 45, sd = 15)),
    ge = sample(0:100, n, replace = TRUE),
    ra = sample(0:100, n, replace = TRUE)
  )
}


###############################################################################
nationalism <- function(world, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  nb_countries <- sample(5:10, 1)
  
  centers_list <- lapply(1:nb_countries, function(i) {
    sample(0:100, sample(1:3, 1), replace = FALSE)
  })
  
  distance_matrix <- sapply(centers_list, function(centers) {
    rowMeans(sapply(centers, function(c) dnorm(world$r, mean = c, sd = 10)))
  })
  
  probas <- distance_matrix / rowSums(distance_matrix)
  
  country_id <- apply(probas, 1, function(p) sample(1:nb_countries, 1, prob = p))
  
  world$country <- country_id
  
  return(world)
}


###############################################################################
colonialism <- function(world) {
  
  
}





world <- init_world(n = 1000) 
world <- nationalism(world)


graph(world, "ra", "country")



capitalism <- function(world, years = 1) {
  
  # Categorisation pour la premiere fois apres on reprend le resultat
  world$gender_cat <- ifelse(world$gender < 45, "man",
                            ifelse(world$gender > 55, "woman", "autres"))
  world$race_cat <- ifelse(world$race < 65, "racisee", "blanche")
  
  
  return(world)
}
