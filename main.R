
#                                           EXPLORATIONS ET TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                  janvier 2025

invisible(sapply(list.files("script/function", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))

library(sf)
library(asf)
library(mapsf)

options(error = NULL)


###############################################################################
############################################ CREATION DE FICHIERS .PARQUET TEST

# Creation de fichiers de donnees test
set.seed(123)
n <- 1e6

df <- data.frame(
  id = sprintf("%07d", 1:n),
  value1 = sample(10:100000, n, replace = TRUE),
  value2 = sample(10:100000, n, replace = TRUE)
)

DF <- data.frame(
  ID = sprintf("%07d", 1:n),
  VALUE1 = sample(10:100000, n, replace = TRUE),
  VALUE2 = sample(10:100000, n, replace = TRUE)
)

# Conversion en fichier.sas7bdat
write_sas(df, "output/sas/data1.sas7bdat")
write_sas(DF, "output/sas/data2.sas7bdat")

# Transformation de fichiers .sas7bdat en .parquet
parquet_convert(sas = c("output/sas/data1.sas7bdat",
                        "output/sas/data2.sas7bdat"),
                parquet = "output/parquet/",
                chunk = 100000)


###############################################################################
########################################### OUVERTURE DE FICHIERS .PARQUET TEST

# recuperation des noms d'un fichier parquet
names <- parquet_colname(dir = "output/parquet/data1")
names <- parquet_colname(dir = "output/parquet/data2")
names <- parquet_colname(dir = "output/parquet/data3.parquet")

# Ouverture de fichiers .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
                       cols = list(c("id", "ID"),
                                   c("value1", "VALUE1"),
                                   c("value2", "VALUE2")))
 
data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]


###############################################################################
############################################################ SECRET STATISTIQUE

# Creation d'un data.frame d'exemple
x <- data.frame(
  commune = c("com1", "com2", "com3", "com4", "com5"),
  tot = c(100, 100, 50, 60, 20),
  ca1 = c(20, 40, 10, 0, 0),
  ca2 = c(10, 10, 10, 20, 0),
  ca3 = c(30, 10, 10, 20, 0),
  ca4 = c(40, 40, 10, 20, 20)
)

y <- dput(x) # test de cette fonction

test <- secret_data(x, cols = c(3:6), limit = 11, unique = FALSE)

# # install.packages("devtools")
# devtools::install_github("alietteroux/subwork")
# 
# library(subwork)
# 
# # importer les données "FT810" dans un dataframe nommé "FT810.data"
# FT810.data <- import(code = "FT810", type = "data")
# 
# # FT711.data pour echelle com
# # base to salariers
# 
# 
# 
# data <- aggreg_data(tabl = d.irisR.pass,
#                     data = FT810.data, 
#                     id = c("IRIS"))


###############################################################################
######################################## TEST MAILLAGE COMPOSITE D'ALIETTE ROUX

mar <- asf_mar()


# Fond de carte ---------------------------------------------------------------
fond <- mar$geom$irisf
tabl <- mar$pass$irisr

# Repositionnement des DROM
fond <- asf_drom(fond, 
                 id = "IRISF_CODE")

# Creation des limites departementales
dep <- asf_dep(fond, 
               id = "IRISF_CODE")

# Fond de carte specifique
fond_aggreg <- asf_fond(tabl, 
                        fond, 
                        id = c("IRISF_CODE", "IRISF_CODE"), 
                        maille = "IRISrS_CODE") 

# Creation de zooms
z <- asf_zoom(fond_aggreg, 
              villes = c(1:8))

zoom <- z$zoom
label <- z$label

# Simplification des geometries du fond de carte
fond_aggreg <- asf_simplify(fond_aggreg)


# Data ------------------------------------------------------------------------
data <- mar$data$csp

data_aggreg <- asf_data(tabl, data, 
                        vars = c(4:13), 
                        funs = c("sum"), 
                        id = c("IRIS_CODE", "IRIS"), 
                        maille = "IRISrS_CODE")


# Jointure --------------------------------------------------------------------
fondata <- asf_fondata(data_aggreg, fond_aggreg, zoom, 
                       id = c("IRISrS_CODE", "IRISrS_CODE"))


# Creation de cartes et graphiques --------------------------------------------
map_q(fondata,
      tot = "C20_POP15P", 
      var = "C20_POP15P_CS6", 
      breaks = 4,
      zoomlabel = label)

map_bi(fondata, 
       tots = c("C20_POP15P", "C20_POP15P"), 
       vars = c("C20_POP15P_CS3", "C20_POP15P_CS6"))








data <- mar$data$csp
tabl <- mar$pass$irisf
tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE) 
tmp <- tmp[, c(1, 15, 4:13)]

tabl <- mar$pass$irisr
tmp <- merge(tmp, tabl, by = "IRISF_CODE", all.x = TRUE)
tmp <- tmp[, c(2, 1, 16, 18, 3:12)]

tabl <- mar$app$irisr
tmp <- merge(tmp, tabl, by = "IRISrD_CODE", all.x = TRUE)
tmp <- tmp[, c(1, 5:14, 36:39)]

# plot_glis(data = tmp, 
#           vars = c("C20_POP15P_CS3", "C20_POP15P_CS6"), 
#           tot = "C20_POP15P", 
#           plot = TRUE)

asf_plotypo(data = tmp,
            vars = c(4),
            typo = "CATEAAV2020")

asf_plotvar(data = tmp,
            vars = c(4),
            typo = "TAAV2017",
            order = c(4, 1, 2, 6, 3, 5))




# Exemple de dataframe
set.seed(123)

df <- data.frame(
  commune = paste0("Commune_", 1:100),
  departement = sample(c("01 - Ain", "02 - Aisne", "03 - Allier"), 100, replace = TRUE),
  csp_typologie = sample(c("Populaire", "Cadre", "Agricole", "Intermediaire"), 100, replace = TRUE)
)
asf_plotcat_by_typo(data = df,
                    catvar = "csp_typologie",
                    typo = "departement")

data <- df
catvar <- "csp_typologie"
typo <- "departement"
pal <- NULL


asf_plotypo <- function(data, 
                        catvar, 
                        typo, 
                        pal = NULL) {
  
  tmp <- table(data[[typo]], data[[catvar]])
  data <- as.data.frame(tmp)
  names(data) <- c("typo", "var", "count")
  
  # Calcul des pourcentages par categorie de la typologie
  data$pct <- NA
  for (g in unique(data$typo)) {
    idx <- data$typo == g
    total <- sum(data$count[idx])
    data$pct[idx] <- data$count[idx] / total * 100
  }
  
  # Palette
  if (is.null(pal)) {
    nb_cat <- length(unique(data$var))
    pal <- stats::setNames(grDevices::rainbow(nb_cat), sort(unique(data$var)))
  }
  
  # Graphique
  p <- ggplot2::ggplot(data, ggplot2::aes(x = typo, y = pct, fill = var)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste("Repartition de", catvar, "par", typo),
                  x = typo,
                  y = "Pourcentage (%)") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip()
  
  print(p)
}
















# data <- tmp
# vars <- c(4:11)
# typo <- "CATEAAV2020"
# order = c(1, 3, 5, 2, 4)
# pal = NULL
# 
# # SUB-FUNCTIONS -------------------------------------------------------------
# .calc_pct_cat <- function(cat) {
#   cat_data <- data[data[[typo]] == cat, vars, drop = FALSE]
#   total <- colSums(cat_data, na.rm = TRUE)
#   data.frame(
#     category = cat,
#     variable = vars,
#     pct = (total / sum(total)) * 100
#   )
# }
# 
# # PROCESSING ----------------------------------------------------------------
# if (is.numeric(vars)) {
#   vars <- names(data)[vars]
# }
# 
# categories <- unique(data[[typo]])
# 
# # Application d'un ordre pour les categories si specifie
# if (!is.null(order)) {
#   if (is.numeric(order)) {
#     categories <- categories[order]
#     
#   } else if (is.character(order)) {
#     categories <- order
#   }
# }
# 
# # Si aucune couleur n'est specifiee, une palette par defaut est generee
# if (is.null(pal)) {
#   pal <- stats::setNames(grDevices::colorRampPalette(c("#000000", "#f2f2f2"))(length(vars)), vars)
# }
# 
# # Calcul des pourcentages pour chaque categorie et empilage des resultats
# z <- do.call(rbind, lapply(categories, .calc_pct_cat))
# 
# # Predefinition de l'ordre des categories pour le graphique
# z$category <- factor(z$category, levels = rev(categories))
# 
# # Graphique avec ggplot2
# p <- ggplot2::ggplot(z, ggplot2::aes(x = category, y = pct, fill = variable)) +
#      ggplot2::geom_bar(stat = "identity") +
#      ggplot2::labs(title = paste("Repartition des variables par categorie :", typo),
#                    x = "Categories",
#                    y = "Pourcentage (%)") +
#      ggplot2::scale_fill_manual(values = pal) +
#      ggplot2::theme_minimal() +
#      ggplot2::coord_flip()
# 
# print(p)
# 
# 
# 
# rm(data, p, tabl, tmp, z, categories, order, pal, typo, vars)

















# Selection des iris
iris <- mar$geom$irisrs
iris <- iris[, c(1,2,7)]
colnames(iris) <- c("irisrs_code", "irisrs_lib", "p21_pop", "geometry")
st_geometry(iris) <- "geometry"

# # Selection des iris de Mayotte
# mayo <- mar$geom$irisf
# mayo <- mayo[grepl("^976", mayo$IRISF_CODE), ]
# mayo$P21_POP <- NA
# mayo$P21_POP <- as.numeric(mayo$P21_POP)
# mayo <- mayo[, c(1,2,7)]
# colnames(mayo) <- c("irisrs_code", "irisrs_lib", "p21_pop", "geometry")
# st_geometry(mayo) <- "geometry"
# 
# # Collage des deux objets sf/data.frames
# fond <- rbind(iris, mayo)

# Repositionnement des geometries des DROM
fond <- asf_drom(iris, id = "irisrs_code")