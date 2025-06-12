
#                                                        EXPLORATIONS ET TEST R
#
#                                                                antoine beroud
#                                                                  janvier 2025

invisible(sapply(list.files("script/function", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))

library(sf)
library(mapsf)
library(asf)


###############################################################################
################################################# CREATION DE FICHIERS .PARQUET

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
################################################ OUVERTURE DE FICHIERS .PARQUET

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

# Ouverture de fichiers .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
                       cols = c(1:3))
 
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


###############################################################################
###################################################### AUTOCORRELATION SPATIALE

# Lecture du fond regroupe
mar <- asf_mar()

iris <- mar$sf.irisf
tabl <- mar$df.irisr

iris_drom <- asf_drom(iris, 
                      id = "IRISF_CODE")

tmp <- merge(iris_drom[, 1], 
             tabl[, c(2, 7)], 
             by = "IRISF_CODE")

st_write(iris, "test1.gpkg")

tmp <- aggregate(tmp,
                 by = list(tmp$COMR_CODE),
                 FUN = function(x) x[1])

mf_map(tmp)



fond <- asf_fond(iris_drom, 
                 tabl, 
                 by = "IRISF_CODE", 
                 maille = "COMR_CODE")


# Lecture des donnees
mar_revenu <- read.csv("input/decile_revucm_comar.csv")

# Joindre les données au fond de carte
communes_sf <- merge(communes_sf, revenus_df, by.x = "INSEE_COM", by.y = "INSEE")







library(spdep)

# Créer les voisins avec contiguïté de Queen (voisinage 8)
communes_nb <- poly2nb(communes_sf)

# Convertir en matrice de pondération spatiale
communes_listw <- nb2listw(communes_nb, style = "W", zero.policy = TRUE)






moran.test(communes_sf$revenu_median, communes_listw, zero.policy = TRUE)


local_moran <- localmoran(communes_sf$revenu_median, communes_listw, zero.policy = TRUE)

# Ajouter les résultats à l'objet sf
communes_sf$Ii <- local_moran[,1]     # indice de Moran local
communes_sf$Z.Ii <- local_moran[,4]  # score z (standardisé)
communes_sf$pval <- local_moran[,5]  # p-value




library(ggplot2)

# Par exemple : visualiser les valeurs significatives
communes_sf$signif <- communes_sf$pval < 0.05

ggplot(communes_sf) +
  geom_sf(aes(fill = signif)) +
  scale_fill_manual(values = c("grey80", "red"), labels = c("Non significatif", "Significatif")) +
  labs(title = "Clusters de revenus médians (Moran local)", fill = "Significativité") +
  theme_minimal()











revenu_c <- scale(communes_sf$revenu_median)[,1]
revenu_lag <- lag.listw(communes_listw, revenu_c)

communes_sf$quad <- NA
communes_sf$quad[revenu_c >= 0 & revenu_lag >= 0] <- "High-High"
communes_sf$quad[revenu_c <= 0 & revenu_lag <= 0] <- "Low-Low"
communes_sf$quad[revenu_c >= 0 & revenu_lag <= 0] <- "High-Low"
communes_sf$quad[revenu_c <= 0 & revenu_lag >= 0] <- "Low-High"
communes_sf$quad[communes_sf$pval > 0.05] <- "Non significatif"

# Carte typologique
ggplot(communes_sf) +
  geom_sf(aes(fill = quad), color = NA) +
  scale_fill_manual(values = c(
    "High-High" = "darkred",
    "Low-Low" = "darkblue",
    "High-Low" = "orange",
    "Low-High" = "lightblue",
    "Non significatif" = "grey80"
  )) +
  labs(title = "Typologie des clusters LISA", fill = "Cluster") +
  theme_minimal()



