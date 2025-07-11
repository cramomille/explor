


init_world <- function(n = 1000, seed = 1312) {
  
  set.seed(seed)
  
  digits <- nchar(as.character(n)) + 1
  
  data.frame(
    i = sprintf(paste("%0", digits, "d"), 1:n),
    a = sample(0:90, n, replace = TRUE, prob = dnorm(0:90, mean = 45, sd = 15)),
    g = sample(0:100, n, replace = TRUE),
    r = sample(0:100, n, replace = TRUE)
  )
}

world <- init_world() 


capitalism <- function(world, years = 1) {
  
  # Categorisation pour la premiere fois après on reprend le resultat
  world$gender_cat <- ifelse(world$gender < 45, "man",
                            ifelse(world$gender > 55, "woman", "autres"))
  world$race_cat <- ifelse(world$race < 65, "racisee", "blanche")
  
  
  return(world)
}
