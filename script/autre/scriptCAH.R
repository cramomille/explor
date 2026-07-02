library(cluster)
library(ggplot2)
library(ggdendro)
library(reshape2)


################################################################################
library(duckdb)
library(dplyr)

# Connexion à DuckDB
con <- dbConnect(duckdb())

# Répertoire des fichiers Parquet
parquet_dir <- "test/parquet/export/data1/"

# Charger tous les fichiers Parquet dans une table DuckDB
files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

# Combiner tous les fichiers dans une seule table temporaire
query <- paste0(
  "CREATE OR REPLACE TEMPORARY TABLE all_data AS ",
  paste0("SELECT * FROM read_parquet('", files, "')", collapse = " UNION ALL ")
)
dbExecute(con, query)

# Exemple de manipulation avec dplyr
tbl_duckdb <- tbl(con, "all_data")

# Filtrage et transformation
df <- tbl_duckdb %>%
  filter(dep == "55") %>%          # Filtre sur un departement
  select(c(1,2,45:53)) %>%         # Selection des colonnes souhaitees
  collect()                        # Convertion en dataframe

# Deconnexion
dbDisconnect(con, shutdown = TRUE)


df <- df %>%
  group_by(COM) %>%
  summarise(
    across(.cols = where(is.numeric), .fns = sum, .names = "{.col}"),
    .groups = "drop"
  )

################################################################################

names(df)
# Étape 1 : Calculer la classification
varquanti <- c("C20_POP15P", 
               "C20_POP15P_CS1",
               "C20_POP15P_CS2",
               "C20_POP15P_CS3", 
               "C20_POP15P_CS4",
               "C20_POP15P_CS5",
               "C20_POP15P_CS6",
               "C20_POP15P_CS7",
               "C20_POP15P_CS8")
classifObj <- ComputeClassif(df = df, varquanti = varquanti, stand = TRUE, method = "ward")

# Étape 2 : Tracer le dendrogramme
dendroPlot <- PlotDendro(classifobj = classifObj)
print(dendroPlot)

# Étape 3 : Visualiser les niveaux d'inertie
heightPlot <- PlotHeight(classifobj = classifObj)
print(heightPlot)

# Étape 4 : Tracer les profils des classes
nbclus <- 3
profileResults <- PlotProfile(classifobj = classifObj, nbclus = nbclus)
profilePlot <- profileResults$PROFILE
print(profilePlot)



# ---
# Découper l'arbre pour assigner les entités à des clusters
clusId <- cutree(classifObj, k = 3)

# Ajouter la colonne d'appartenance des clusters au dataframe original
dfFinal <- df  # Copie du dataframe original
dfFinal$Cluster <- factor(clusId, 
                          levels = 1:nbclus, 
                          labels = paste("Cluster", 1:nbclus))
# ---




# # Calculer les moyennes des variables par cluster
# clusterMeans <- aggregate(df[, varquanti], 
#                           by = list(Cluster = factor(clusId, 
#                                                      levels = 1:nbclus, 
#                                                      labels = paste("Cluster", 1:nbclus))), 
#                           mean)
# 
# # Ajouter les moyennes à chaque observation
# dfFinal <- df
# dfFinal$Cluster <- factor(clusId, 
#                           levels = 1:nbclus, 
#                           labels = paste("Cluster", 1:nbclus))
# 
# # Ajout des écarts à la moyenne
# for (var in varquanti) {
#   dfFinal[[paste0(var, "_ecart")]] <- round(dfFinal[[var]] - clusterMeans[match(dfFinal$Cluster, clusterMeans$Cluster), var])
# }


################################################################################
# HIERARCHICAL CLUSTERING ----

# compute classification ----
ComputeClassif <- function(df, varquanti, stand, method){
  classifObj <- agnes(x = df[, varquanti], diss = FALSE, metric = "euclidean", stand = stand, method = method)
  return(classifObj)
}

# plot dendrogram ----
PlotDendro <- function(classifobj){
  dendroPlot <- as.dendrogram(classifobj)
  dendroData <- dendro_data(dendroPlot, type = "rectangle")
  dendroGgplot <- ggplot(segment(dendroData)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_x_continuous("") + scale_y_continuous("") +
    theme_bw()
  
  return(dendroGgplot)
}

# plot inertia ----
PlotHeight <- function(classifobj){
  sortedHeight <- sort(classifobj$height, decreasing = TRUE)
  relHeigth <- sortedHeight / sum(sortedHeight) * 100
  tabHeight <- data.frame(NODES = factor(1:20),
                          INERTIE = relHeigth[1:20])
  
  heightPlot <- ggplot(tabHeight) +
    geom_bar(aes(x = NODES, y = INERTIE), fill = "grey30", stat = "identity") +
    scale_x_discrete("Nombre de classes") + scale_y_continuous("Niveau") +
    theme_bw()
  
  return(heightPlot)
}

# plot profile ----
PlotProfile <- function(classifobj, nbclus){
  dfOri <- as.data.frame(classifobj$data, stringsAsFactors = FALSE)
  clusId <- cutree(classifobj, k = nbclus)
  dfOri$CLUS <- factor(clusId,
                       levels = 1:nbclus,
                       labels = paste("CLASSE", 1:nbclus))
  clusProfile <- aggregate(dfOri[, 1:ncol(dfOri)-1],
                           by = list(dfOri$CLUS),
                           mean)
  colnames(clusProfile)[1] <- "CLASSE"
  clusLong <- melt(clusProfile, id.vars = "CLASSE")
  
  profilePlot <- ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
    scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
    facet_wrap(~ CLASSE) + coord_flip() + theme_bw()
  
  return(list(PROFILE = profilePlot, CLUSID = dfOri$CLUS))
}

