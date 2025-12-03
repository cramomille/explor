# ==============================================================================
# EXEMPLE COMPLEXE : Analyse multivariee complete sur les CSP
# ==============================================================================

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(cluster)
library(corrplot)

# ---------------------------------------------------------------------------
# 1. LECTURE DES DONNEES
# ---------------------------------------------------------------------------

data <- read.csv("input/csp_2020.csv")
data_quant <- data[, c(6:13)]

# Explorations preliminaires
str(data)
summary(data)

# Matrice de correlation
corrplot(cor(data_quant), method = "color", type = "upper",
         tl.cex = 0.7, title = "Matrice de correlations")


# ---------------------------------------------------------------------------
# 2. ACP
# ---------------------------------------------------------------------------

acp <- PCA(data_quant, scale.unit = TRUE, ncp = 10, graph = FALSE)

# --- 2.1 Analyse des valeurs propres ---
cat("\n=== VALEURS PROPRES ===\n")
eigenvalues <- acp$eig
print(eigenvalues)

# Critère de Kaiser (valeur propre > 1)
nb_kaiser <- sum(eigenvalues[, 1] > 1)
cat("\nNombre de composantes selon Kaiser:", nb_kaiser, "\n")

# Variance cumulée
var_cum_70 <- which(eigenvalues[, 3] >= 70)[1]
cat("Nombre de composantes pour 70% variance:", var_cum_70, "\n")
var_cum_80 <- which(eigenvalues[, 3] >= 80)[1]
cat("Nombre de composantes pour 80% variance:", var_cum_80, "\n")

# Visualisation
fviz_eig(acp, addlabels = TRUE, ncp = 10,
         main = "Scree plot", barfill = "#00AFBB")


# --- 2.2 Analyse des variables ---

# Cercle des corrélations (Dim 1-2)
fviz_pca_var(acp, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE, title = "Variables - Dim 1-2")

# Cercle des corrélations (Dim 3-4)
fviz_pca_var(acp, axes = c(3, 4), col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE, title = "Variables - Dim 3-4")

# Contributions des variables
cat("\n=== TOP 5 CONTRIBUTIONS Dim 1 ===\n")
contrib_dim1 <- sort(acp$var$contrib[, 1], decreasing = TRUE)[1:5]
print(contrib_dim1)

cat("\n=== TOP 5 CONTRIBUTIONS Dim 2 ===\n")
contrib_dim2 <- sort(acp$var$contrib[, 2], decreasing = TRUE)[1:5]
print(contrib_dim2)

# Qualité de représentation (Cos²)
fviz_cos2(acp, choice = "var", axes = 1:2, top = 10,
          title = "Cos² variables (Dim 1-2)")

fviz_cos2(acp, choice = "var", axes = 3:4, top = 10,
          title = "Cos² variables (Dim 1-2)")

# cos² proche de 1 → la variable est très bien représentée sur le plan (elle “appartient” clairement à ces axes)
# cos² proche de 0 → le plan 1–2 ne représente presque pas cette variable ; elle est surtout portée par d’autres axes
# éviter d’interpréter une variable qui a un cos² très faible sur ce plan

# Interprétation des axes
cat("\n=== INTERPRÉTATION DES AXES ===\n")
dimdesc_result <- dimdesc(acp, axes = 1:3)
print(dimdesc_result)

# AXE 1 ----
# Les corrélations sont fortes et organisées en deux groupes :
# Variables fortement positives (Var11–Var15, Var6–Var10)
# → corrélées entre elles, tirent l’axe dans le sens positif
# Variables fortement négatives (Var1–Var5)
# → tirent l’axe dans le sens négatif
# 
# Interprétation :
# Dim 1 oppose un groupe de variables du type Var1 à Var5 contre un groupe Var6 à Var15
# C’est l’opposition structurante des groupes simulés dans cet exemple :
# – Groupe 1 : Var1–5 fortes ; Var6–10 faibles
# – Groupe 3 : Var11–15 fortes
#
# Axe 1 = « Opposition Var1–5 vs Var6–15 »
#
#
# AXE 2 ----
# Les corrélations sont beaucoup plus faibles
# Les variables les plus liées : Var11, Var15, Var13, Var14, Var12 (positives)
# En face : Var6–Var10 (négatives)
# 
# Interprétation :
# Dim 2 oppose certains sous-groupes au sein des blocs hauts de Var11–15 vs Var6–10
# C’est une dimension secondaire : variations fines entre les groupes simulés
# 
# Axe 2 = « nuances à l’intérieur des blocs Var11–15 vs Var6–10 ».
#
#
# AXE 3 ----
# Variables positives : Var9, Var6, Var8
# Variables négatives : Var10, Var7
# 
# Interprétation :
# Dim 3 sépare surtout un sous-groupe parmi les variables Var6–Var10
# C’est une dimension locale, interne au “bloc central”
# 
# Axe 3 = « variation interne du bloc Var6–Var10 »


# --- 2.3 Analyse des individus ---

# Projection avec coloration par contribution
fviz_pca_ind(acp, col.ind = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = FALSE, alpha.ind = 0.5,
             title = "Individus colorés par contribution")

# Identification des individus extrêmes
ind_extreme_dim1 <- order(acp$ind$coord[, 1], decreasing = TRUE)[1:5]
ind_extreme_dim2 <- order(abs(acp$ind$coord[, 2]), decreasing = TRUE)[1:5]

cat("\n=== INDIVIDUS EXTRÊMES Dim 1 ===\n")
print(data[ind_extreme_dim1, ])

# Biplot (variables + individus)
fviz_pca_biplot(acp, 
                geom.ind = "point",
                col.ind = "#00AFBB", alpha.ind = 0.3,
                col.var = "red",
                repel = TRUE,
                title = "Biplot ACP")

# ---------------------------------------------------------------------------
# 3. CAH SUR LES RÉSULTATS DE L'ACP
# ---------------------------------------------------------------------------

# CAH avec méthode de Ward sur les 5 premières composantes
coords_acp <- acp$ind$coord[, 1:5]

# Calcul de la distance euclidienne
dist_matrix <- dist(coords_acp, method = "euclidean")

# CAH avec Ward
cah_ward <- hclust(dist_matrix, method = "ward.D2")

# Dendrogramme
plot(cah_ward, labels = FALSE, hang = -1,
     main = "Dendrogramme (Ward)", xlab = "", sub = "")
rect.hclust(cah_ward, k = 7, border = 2:5)

# --- 3.1 Détermination du nombre optimal de classes ---

# Méthode du coude (inertie)
inertie <- rep(0, 10)
for(k in 1:10) {
  classes <- cutree(cah_ward, k = k)
  inertie[k] <- sum(sapply(1:k, function(i) {
    cluster_data <- coords_acp[classes == i, , drop = FALSE]
    if(nrow(cluster_data) > 1) {
      sum(dist(cluster_data)^2) / (2 * nrow(cluster_data))
    } else {
      0
    }
  }))
}

plot(1:10, inertie, type = "b", xlab = "Nombre de classes",
     ylab = "Inertie intra-classe", main = "Méthode du coude")

# Indice de silhouette
silhouette_scores <- numeric(8)
for(k in 2:9) {
  classes <- cutree(cah_ward, k = k)
  sil <- silhouette(classes, dist_matrix)
  silhouette_scores[k-1] <- mean(sil[, 3])
}

plot(2:9, silhouette_scores, type = "b",
     xlab = "Nombre de classes", ylab = "Silhouette moyenne",
     main = "Indice de silhouette")

# Choix
k_optimal <- 7
classes_finales <- cutree(cah_ward, k = k_optimal)

cat("\n=== RÉPARTITION DES CLASSES ===\n")
print(table(classes_finales))

# --- 3.2 Visualisation du clustering ---

# Projection sur ACP avec classes
fviz_cluster(list(data = coords_acp, cluster = classes_finales),
             palette = c("#E7B800", "#00AFBB", "#FC4E07",
                         "#7AD151", "#B33DC6", "#FF6F91",
                         "#2E86C1"),
             ellipse.type = "convex",
             # repel = TRUE,
             show.clust.cent = TRUE,
             ggtheme = theme_minimal(),
             main = "Clustering (3 classes) sur ACP")

# --- 3.3 Caractérisation des classes ---

data$Classe <- as.factor(classes_finales)





library(asf)
library(mapsf)

mar <- asf_mar(md = "iris_xxxx", ma = "iris_f", geom = TRUE)

geom <- asf_drom(mar[[2]])

z <- asf_zoom(geom, places = c("5", "4"))

data <- asf_data(data, mar[[1]], by.x = "IRIS", by.y = "IRIS_CODE", maille = "IRISF_CODE", vars = 4, funs = "sum", keep = c("COM", "Classe"))

fondata <- asf_fondata(geom, z[[1]], data, by = "IRISF_CODE")


mf_map(fondata, 
       var = "Classe", 
       type = "typo", 
       pal = c("#E7B800", "#00AFBB", "#FC4E07", "#7AD151", "#B33DC6", "#FF6F91", "#2E86C1"),
       border = NA)







# Statistiques descriptives par classe
cat("\n=== MOYENNES PAR CLASSE ===\n")
moyennes_classes <- aggregate(data_quant, 
                              by = list(Classe = classes_finales),
                              FUN = mean)
print(moyennes_classes)

# Test de V de Cramer pour variables qualitatives
cat("\n=== ASSOCIATION CLASSES - VARIABLES QUALITATIVES ===\n")
for(var_qual in names(data_qual)) {
  table_cont <- table(classes_finales, data_qual[[var_qual]])
  chi2_test <- chisq.test(table_cont)
  cramer_v <- sqrt(chi2_test$statistic / (n * (min(dim(table_cont)) - 1)))
  cat(var_qual, "- V de Cramer:", round(cramer_v, 3), 
      "- p-value:", format.pval(chi2_test$p.value), "\n")
}

# Variable	    V de Cramer	    Interprétation
# Region	      0.068	          Très faible association avec les clusters (proche de 0), p ≈ 0.59 → aucune relation significative
# Categorie	    0.039	          Très faible association, p ≈ 0.82 → pas d’effet
# Niveau	      0.048	          Très faible association, p ≈ 0.68 → pas d’effet






library(ggplot2)
library(reshape2)

plot(acp$ind$coord[,1], acp$ind$coord[,2], col = classes_finales,
     pch = 19, xlab = "Dim1", ylab = "Dim2", main = "Clusters sur plan PCA")
legend("topright", legend = 1:3, col = 1:3, pch = 19)





















# ---------------------------------------------------------------------------
# 4. AFC (Analyse Factorielle des Correspondances)
# ---------------------------------------------------------------------------

# AFC entre Region et Categorie
table_contingence <- table(donnees$Region, donnees$Categorie)

cat("\n=== TABLE DE CONTINGENCE ===\n")
print(table_contingence)

# Test d'indépendance du Chi²
chi2_test <- chisq.test(table_contingence)
cat("\nTest du Chi² - p-value:", chi2_test$p.value, "\n")

# AFC
afc <- CA(table_contingence, graph = FALSE)

# Inertie
cat("\n=== INERTIE AFC ===\n")
print(afc$eig)

# Visualisation
fviz_ca_biplot(afc, repel = TRUE,
               title = "AFC - Region vs Categorie",
               col.row = "blue", col.col = "red")

# Contributions
cat("\n=== CONTRIBUTIONS LIGNES (Region) ===\n")
print(afc$row$contrib)

cat("\n=== CONTRIBUTIONS COLONNES (Categorie) ===\n")
print(afc$col$contrib)

