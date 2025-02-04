
#                                  EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                antoine beroud & renaud le goix
#                                                                  decembre 2024

library(sf)
library(mapsf)
library(readxl)
library(data.table)
library(rmapshaper)
library(asf)


################################################################################
###################################################################### FONCTIONS

# Fonction pour charger des fichiers
load_files <- function(dir, ext, fun = read.csv, list = FALSE, var = NULL) {
  
  # Fonction interne pour lire et selectionner les colonnes
  read <- function(file) {
    data <- fun(file)
    if (!is.null(var)) {
      data <- data[, var, drop = FALSE]
    }
    return(data)
  }
  
  # Creation de la liste des fichiers avec l'extension specifiee
  files <- list.files(path = dir, pattern = paste0("\\.", ext, "$"), full.names = TRUE)
  
  if (list) {
    file_list <- lapply(files, read)
    names(file_list) <- gsub(paste0("\\.", ext, "$"), "", basename(files))
    return(file_list)
    
  } else {
    for (file in files) {
      file_name <- gsub(paste0("\\.", ext), "", basename(file))
      assign(file_name, read(file), envir = .GlobalEnv)
    }
  }
}


# Fonction pour calculer les valeurs a partir des tables dvf
process_dvf <- function(dvf) {
  
  # Fonction pour traiter un type de bien (Maison/Appartement)
  filter_dvf <- function(dvf, type_bien) {
    # Filtrer les données pour le type de bien
    dvf_filtered <- dvf[dvf$type == type_bien, ]
    
    # Calcul des agrégations par commune
    com_prix <- tapply(dvf_filtered$prix, dvf_filtered$codecommune, median, na.rm = TRUE)
    com_nomb <- tapply(dvf_filtered$prix, dvf_filtered$codecommune, length)
    
    # Convertir en data.frame
    result <- data.frame(commune = names(com_prix),
                         prix = as.vector(com_prix),
                         nb = as.vector(com_nomb))
    
    # Renommer les colonnes selon le type de bien
    colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
    colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
    
    return(result)
  }
  
  result <- list()
  
  for (name in names(dvf)) {
    
    x <- dvf[[name]]
    
    maison <- filter_dvf(x, "Maison")
    appart <- filter_dvf(x, "Appartement")
    
    tmp <- merge(maison, appart, by = "commune", all = TRUE)
    
    result[[name]] <- tmp
  }
  
  return(result)  
}


################################################################################
############################################################### DONNEES FISOSOFI

# Recuperation du fond iris du package asf
iris <- st_read("data/fond_2019.gpkg")

# Agregation en communes
com <- aggregate(iris, by = list(iris$INSEE_COM), FUN = function(x) x[1])
colnames(com)[1] <- "CODE_COM"
com <- com[, c(1,3)]

# Traitement du fichier filosofi (carroyage 1km)
filosofi <- st_read("data/filo/filo_2019.gpkg")

filo <- filosofi[, c(3:10, 14:30)]
filo$log <- filo$log_av45 + filo$log_45_70 + filo$log_70_90 + filo$log_ap90 + filo$log_inc 
filo$log_av90 <- filo$log - filo$log_ap90
filo$ind_25m <- filo$ind_0_3 + filo$ind_4_5 + filo$ind_6_10 + filo$ind_11_17 + filo$ind_18_24
filo <- filo[, -c(9:11, 13, 15:23, 25)]

# On conserve le nombre d'individus et la somme des revenus pour calculer les salaires
# et en faisant 'logements' - 'menages proprietaires' - 'logements sociaux'
# on decide d'estimer le parc locatif prive
filo <- filo[, c("ind", "ind_snv", "men_prop", "log", "log_soc")]

# Calcul de la surface des communes
com$area_com <- as.numeric(st_area(com))

# Calcul de la surface des carreaux
filo$area_caro <- as.numeric(st_area(filo))

# Decoupage des carreaux en fonction des communes
carro_decoup <- st_intersection(filo, com)

# Calcul de la surface de chaque morceau de carreau decoupe
carro_decoup$area_boucarro <- as.numeric(st_area(carro_decoup))

# Liste des colonnes a ajuster
cols <- head(names(filo), -1)

# Fonction pour ajuster les valeurs selon la part de recouvrement
by_area <- function(col) {
  carro_decoup[[col]] <- carro_decoup[[col]] * (carro_decoup$area_boucarro / carro_decoup$area_caro)
}

# Application de la fonction a toutes les colonnes de la liste
lapply(cols, by_area)

# Convertion du data.frame en un data.table pour reduire le temps de calcul
setDT(carro_decoup)

# Agregation (sum) sur toutes les colonnes numeriques sauf celles specifiees (id, geom)
carro_agreg <- carro_decoup[, lapply(.SD, sum), 
                            by = CODE_COM, 
                            .SDcols = !c("CODE_COM", "NOM_COM", "geom")]
com <- com[, c(1,2)]
carro_agreg <- carro_agreg[, c(1:6)]

# Jointure entre le fond geographique et les donnees agregees au niveau communal
fond <- merge(com, carro_agreg, by.x = "CODE_COM", by.y = "CODE_COM", all.x = TRUE)

# Nettoyage et export
rm(iris, com, filosofi, filo, carro_decoup, carro_agreg, cols, by_area)
st_write(fond, "export/fond_filo.gpkg")


################################################################################
################################################################################
# # Création d'un fichier à partir de plusieurs fichiers DVF traités par le code 'prep_dvf'
# 
# # Liste des fichiers et des années correspondantes
# fichiers_dvf <- list("data/dvf_prep/dvf_2019.csv")
# annees <- c(2019)
# 
# # Initialisation du data.frame final
# dvf_final <- NULL
# 
# # Boucle sur les fichiers
# for (i in 1:length(fichiers_dvf)) {
#   
#   # Lecture du fichier correspondant à l'année
#   dvf <- read.csv(fichiers_dvf[[i]])
#   annee <- annees[i]
#   
#   # Filtre des appartements
#   appart <- dvf[dvf$type == "Appartement", ]
#   a_agreg <- aggregate(cbind(prix, surface) ~ codecommune, data = appart, FUN = sum)
#   names(a_agreg) <- c("code_com", paste0("prix_appart_", annee), paste0("surf_appart_", annee))
#   
#   # Filtre des maisons
#   maison <- dvf[dvf$type == "Maison", ]
#   m_agreg <- aggregate(cbind(prix, surface) ~ codecommune, data = maison, FUN = sum)
#   names(m_agreg) <- c("code_com", paste0("prix_maison_", annee), paste0("surf_maison_", annee))
#   
#   # Fusion des appartements et maisons pour cette année
#   dvf_annee <- merge(a_agreg, m_agreg, by = "code_com", all = TRUE)
#   
#   # Si c'est la première itération, initialiser df_final
#   if (is.null(dvf_final)) {
#     dvf_final <- dvf_annee
#   } else {
#     # Fusion avec les résultats des années précédentes
#     dvf_final <- merge(dvf_final, dvf_annee, by = "code_com", all = TRUE)
#   }
# }
# 
# # Calcul des prix au m² pour chaque année
# for (annee in annees) {
#   dvf_final[[paste0("prix_m2_appart_", annee)]] <- round(dvf_final[[paste0("prix_appart_", annee)]] / 
#                                                            dvf_final[[paste0("surf_appart_", annee)]], 0)
#   dvf_final[[paste0("prix_m2_maison_", annee)]] <- round(dvf_final[[paste0("prix_maison_", annee)]] / 
#                                                            dvf_final[[paste0("surf_maison_", annee)]], 0)
#   
#   # Calcul du prix moyen au m² (appart + maison)
#   dvf_final[[paste0("prix_m2_", annee)]] <- round(
#     (ifelse(is.na(dvf_final[[paste0("prix_appart_", annee)]]), 0, dvf_final[[paste0("prix_appart_", annee)]]) + 
#        ifelse(is.na(dvf_final[[paste0("prix_maison_", annee)]]), 0, dvf_final[[paste0("prix_maison_", annee)]])) /
#       (ifelse(is.na(dvf_final[[paste0("surf_appart_", annee)]]), 0, dvf_final[[paste0("surf_appart_", annee)]]) + 
#          ifelse(is.na(dvf_final[[paste0("surf_maison_", annee)]]), 0, dvf_final[[paste0("surf_maison_", annee)]])), 0)
# }
# 
# # Nettoyage
# rm(fichiers_dvf, dvf, dvf_annee, appart, maison, a_agreg, m_agreg)
# 
# # Jointure avec le fond géographique
# fond_final <- merge(fond, dvf_final, by.x = "CODE_COM", by.y = "code_com", all.x = TRUE)


################################################################################
################################################################################

file_list <- load_files("data/dvf_prep/", "csv", list = TRUE)

dvf <- file_list[c(6, 7)]

dvf_prep <- process_dvf(dvf)
dvf_prep <- dvf_prep[[1]]


loyer <- read.csv("data/loyer/indicateurs-loyers-appartements.csv", sep = ';')

loyer <- loyer[, c(2,8)]
loyer$loy_m2  <- as.numeric(gsub(",", ".", loyer$loypredm2))
loyer$INSEE <- ifelse(nchar(loyer$INSEE) == 4, paste0("0", loyer$INSEE), loyer$INSEE)
loyer <- loyer[, c(1,3)]

data <- merge(dvf_prep, loyer, by.x = "commune", by.y = "INSEE", all.x = TRUE)

data$loc_priv <- round(data$log - data$log_soc - data$men_prop, 1)

data <- data[, c(1:4, 8:13)]


################################################################################
############################################################################ ASF

fond <- st_read("data/fond_2019.gpkg")
tabl <- read.csv("data/tabl_2019.csv")

data <- aggreg_data(tabl = tabl,
                    data = data, 
                    vars = c(3:10),
                    funs = c("sum", "sum", "prod1", "coef1", "prod2", "coef2", "prod3", "coef3"),
                    id = c("INSEE_COM", "CODE_COM"),
                    maille = "ce")

################################################################################
fond <- aggreg_fond(fond = fond,
                    tabl = tabl,
                    id = c("CODE_IRIS", "CODE_IRIS"),
                    maille = "ce")

################################################################################
zoom <- create_zoom(fond = fond,
                    villes = c("Marseille", "Lyon", "Lille", "Nantes",
                               "Bordeaux", "Toulouse", "Clermont-Ferrand", "Angers",
                               "Perpignan", "Le Havre", "Rouen", "Rennes",
                               "Tours", "Dijon", "Reims", "Grenoble",
                               "Nice", "Montpellier", "La Rochelle", "Besancon"),
                    buffer = 10000)

zooms <- zoom$zooms
labels <- zoom$labels

################################################################################
fond <- simplify_geom(fond, keep = 0.5)

fondata <- merge_fondata(data = data,
                         fond = fond,
                         zoom = zooms,
                         id = c("ce", "ce"))

# Nombre d'annee de revenu pour acheter un bien
fondata$maison_abord <- (fondata$median_prix_maison * 0.9) / (fondata$ind_snv / fondata$ind)
fondata$appart_abord <- (fondata$median_prix_appart * 0.9) / (fondata$ind_snv / fondata$ind)

# Abordabilite du loyer (pourcentage du salaire)
fondata$location_abord <- (fondata$loy_m2 * 49) / (fondata$ind_snv / fondata$ind / 12) * 100





library(RColorBrewer)

palette <- rev(brewer.pal(10, "RdGy"))

mf_map(fondata,
       var = "maison_abord",
       type = "choro",
       breaks = "quantile",
       nbreaks = 10,
       pal = palette,
       border = NA,
       na.color = "gray"
)















file_list <- load_files("data/dvf_prep/", "csv", list = TRUE, var = c(5:9))

test <- process_dvf(file_list)



# Fonction pour calculer les déciles
calc_decile <- function(df, var) {
  quantile(df[[var]], probs = seq(0, 1, by = 0.1), na.rm = TRUE)
}

# Initialiser une liste vide pour stocker les résultats
result <- list()

# Boucle sur la liste de dataframes pour calculer les déciles pour chaque année
for (annee in names(liste_df)) {
  df <- liste_df[[annee]]
  
  # Calcul des déciles pour le prix des maisons
  deciles <- calc_decile(df, "median_prix_maison")
  
  # Ajouter une ligne avec les déciles calculés et l'année comme nom de ligne
  result[[annee]] <- deciles
}

# Convertir la liste en un dataframe avec les noms d'années comme lignes
resultat_final <- do.call(rbind, result)

# Mettre les noms des lignes (années)
rownames(resultat_final) <- names(liste_df)























aav <- read.csv("data/aav_2020.csv")

# Fusionner les dataframes sur la colonne "commune"
df_combined <- Reduce(function(x, y) merge(x, y, by = "commune", all = TRUE),
                      list(df_2010, df_2011, df_2012))

# Calculer la population moyenne par année
df_combined$population_moyenne <- rowMeans(df_combined[, -1], na.rm = TRUE)

# Afficher le dataframe
print(df_combined)

# Tracer l'évolution de la population moyenne
library(ggplot2)
df_melted <- reshape2::melt(df_combined, id.vars = "commune", variable.name = "year", value.name = "population")
df_melted$year <- gsub("population_", "", df_melted$year)  # Nettoyer les noms de colonnes

# Calculer la population moyenne pour chaque année
avg_population_per_year <- aggregate(population ~ year, data = df_melted, FUN = mean)

# Tracer la courbe
ggplot(avg_population_per_year, aes(x = as.integer(year), y = population)) +
  geom_line() +
  geom_point() +
  labs(title = "Évolution de la population moyenne", x = "Année", y = "Population moyenne")


