# ==============================================================================
# EXEMPLE COMPLEXE : Analyse multivariée complète sur données simulées
# 500 individus, 15 variables quantitatives + 3 variables qualitatives
# ==============================================================================

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(cluster)
library(corrplot)

set.seed(123)

# ---------------------------------------------------------------------------
# 1. CRÉATION DE DONNÉES COMPLEXES SIMULÉES
# ---------------------------------------------------------------------------

n <- 1500  # Nombre d'individus

# Simulation de 3 groupes sous-jacents
groupe <- sample(1:3, n, replace = TRUE, prob = c(0.4, 0.35, 0.25))

# Variables quantitatives (15 variables)
# Groupe 1 : valeurs élevées sur var1-5, faibles sur var6-10
# Groupe 2 : valeurs moyennes partout
# Groupe 3 : valeurs élevées sur var11-15

data_quant <- matrix(NA, nrow = n, ncol = 15)
colnames(data_quant) <- paste0("Var", 1:15)

for(i in 1:n) {
  if(groupe[i] == 1) {
    data_quant[i, 1:5] <- rnorm(5, mean = 10, sd = 2)
    data_quant[i, 6:10] <- rnorm(5, mean = 3, sd = 1.5)
    data_quant[i, 11:15] <- rnorm(5, mean = 5, sd = 2)
  } else if(groupe[i] == 2) {
    data_quant[i, ] <- rnorm(15, mean = 6, sd = 2)
  } else {
    data_quant[i, 1:5] <- rnorm(5, mean = 4, sd = 1.5)
    data_quant[i, 6:10] <- rnorm(5, mean = 7, sd = 2)
    data_quant[i, 11:15] <- rnorm(5, mean = 12, sd = 2)
  }
}

# Variables qualitatives (3 variables)
data_qual <- data.frame(
  Region = sample(c("Nord", "Sud", "Est", "Ouest"), n, replace = TRUE,
                  prob = c(0.3, 0.3, 0.2, 0.2)),
  Categorie = sample(c("A", "B", "C"), n, replace = TRUE,
                     prob = c(0.5, 0.3, 0.2)),
  Niveau = sample(c("Bas", "Moyen", "Haut"), n, replace = TRUE,
                  prob = c(0.3, 0.5, 0.2))
)

# Fusion
donnees <- data.frame(data_quant, data_qual)

# Explorations préliminaires
str(donnees)
summary(donnees)

# Matrice de corrélation
corrplot(cor(data_quant), method = "color", type = "upper",
         tl.cex = 0.7, title = "Matrice de corrélations")


# ---------------------------------------------------------------------------
# 2. ACP APPROFONDIE
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
print(donnees[ind_extreme_dim1, ])

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
rect.hclust(cah_ward, k = 4, border = 2:5)

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
k_optimal <- 3
classes_finales <- cutree(cah_ward, k = k_optimal)

cat("\n=== RÉPARTITION DES CLASSES ===\n")
print(table(classes_finales))

# --- 3.2 Visualisation du clustering ---

# Projection sur ACP avec classes
fviz_cluster(list(data = coords_acp, cluster = classes_finales),
             palette = c("#E7B800", "#00AFBB", "#FC4E07"),
             ellipse.type = "convex",
             # repel = TRUE,
             show.clust.cent = TRUE,
             ggtheme = theme_minimal(),
             main = "Clustering (3 classes) sur ACP")

# --- 3.3 Caractérisation des classes ---

donnees$Classe <- as.factor(classes_finales)

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

