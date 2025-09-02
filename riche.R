
#                                           AUTOCORRELATION SPATIALE DES RICHES
#
#                                                                antoine beroud
#                                                                  juillet 2025


# Preparation des donnees
mar <- asf_mar(maille = "comr")

iris <- mar$geom
tabl <- mar$tabl

iris <- iris[!grepl("^96|^97|^98|^N|^P", iris$IRISF_CODE), ]

fond <- asf_fond(iris, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR_CODE")

fond <- asf_simplify(fond)

data <- read.csv("input/decile_revucm_comar.csv")
data <- data[, c(2, 123:132)]

fondata <- asf_fondata(f = fond, 
                       d = data, 
                       by.x = "COMR_CODE", by.y = "comar")

fond$dep <- substr(fond$COMR_CODE, 1, 2)
dep <- asf_borders(fond, by = "dep", keep = 0.1)

mf_map(fondata, 
       var = "d9_2022",
       type = "choro", 
       nbreaks = 6,
       border = NA)




library(MTA)

t <- fondata
t$dep <- substr(t$COMR_CODE, 1, 2)

# Palette de couleurs (deviation, 6 classes, origine Color Brewer)
devpal <-  c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")


# Deviation generale
t$gdevrel <- gdev(x = t,  var1 = "d9_2022", var2 = "d1_2022",  type = "rel")

# Deviation territoriale
t$tdevrel <- tdev(x = t, var1 = "d9_2022", var2 = "d1_2022",  type = "rel", key = "dep")

# Deviation spatiale
t$sdevrel <- sdev(x = t, var1 = "d9_2022", var2 = "d1_2022", type = "rel", order = 1)


# Cartographie
mf_map(x = t, var = "gdevrel", type = "choro", pal = devpal, 
       breaks = c(min(t$gdevrel), 75, 90, 100, 110, 125, max(t$gdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Déviation au contexte général\n(100 = moyenne de la France)")
mf_map(dep,
       col = "#000",
       add = TRUE)

# Cartographie
mf_map(x = t, var = "tdevrel", type = "choro", pal = devpal, 
       breaks = c(min(t$tdevrel), 75, 90, 100, 110, 125, max(t$tdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Déviation au contexte territorial\n(100 = moyenne du département)")
mf_map(dep,
       col = "#000",
       add = TRUE)

# Cartographie
mf_map(x = t, var = "sdevrel", type = "choro", pal = devpal, 
       breaks = c(min(t$sdevrel), 75, 90, 100, 110, 125, max(t$sdevrel)),
       border = NA, leg_pos = "left",  leg_val_rnd = 0, 
       leg_title = "Déviation au contexte spatial\n(100 = moyenne des communes contigües)")
mf_map(dep,
       col = "#000",
       add = TRUE)




library(spdep)

t$Y <- t$d5_2022
t$Y <- t$d9_2022 / t$d5_2022

Moran <- t[,c(1,17)]
Moran$COMR_CODE <- substr(Moran$COMR_CODE, 1, 5)
names(Moran) <- c("code", "Y", "geometry")
row.names(Moran) <- Moran$code

# Normalisation log
Moran$Y <- log(Moran$Y)
Moran$Y_std <- scale(Moran$Y)

# Table de contiguïté
contig_nb <- poly2nb(Moran, row.names = Moran$code)
contig_nb_w <- nb2listw(contig_nb, zero.policy = TRUE)

# # Fond général
# mf_map(Moran, col = "lightgrey", border = NA)
# 
# # Entités sans voisins
# no_neighbors <- which(card(contig_nb) == 0)
# mf_map(Moran[no_neighbors, ], col = "red", border = NA, add = TRUE)

# Moyenne locale
Moran$Y_lag <- lag.listw(contig_nb_w, Moran$Y)
Moran$Y_std_lag <- lag.listw(contig_nb_w, Moran$Y_std)

# Calcul de l'indice de Moran
cor.test(Moran$Y, Moran$Y_lag)





# Local Moran : significativité des valeurs (Z.Ii) 
L.Moran <- localmoran(Moran$Y, contig_nb_w, alternative = "two.sided")
L.Moran <- as.data.frame(L.Moran)
L.Moran$code <- row.names(L.Moran)

# Jointure avec les indices de Moran calculés en amont
Moran <- merge(Moran, L.Moran, by = "code", all.x = TRUE)

# Interprétation des valeurs en quadrants
Moran$q1 <- as.factor(Moran$Y_std > 0)
levels(Moran$q1) <- c("Bas","Haut")
Moran$q2 <- as.factor(Moran$Y_std_lag > 0)
levels(Moran$q2) <- c("Bas","Haut")

# Synthèse des quadrants et définition d'un seuil de valeurs non significatives
signThr <- 0.75
Moran$q <- paste(as.character(Moran$q1), as.character(Moran$q2), sep = "-")
Moran$q <- ifelse(abs(Moran$Z.Ii) < signThr, "Non Sign.", Moran$q)

# Réorganiser les valeurs des quadrants par ordre alphabétique (gestion des couleurs)
Moran <- Moran[order(as.factor(Moran$q)),]
Moran$q <- as.factor(Moran$q)
cols <- c("blue", "skyblue2", "lightpink", "red", "#f5f5f5")[as.factor(Moran$q)]

# Graphique
par(mfrow = c(1,2), mar = c(2,4,2,2), pty = "s")
plot(x = Moran$Y_std, y = Moran$Y_std_lag,  bg = cols, asp = 1,pch = 21,
     cex = 0.8, cex.main = 1, cex.lab = 0.6, cex.axis = 0.6,
     main = "Diagramme de Moran", xlab = "Valeur observée",
     ylab = "Moyenne des communes voisines")

abline(h = 0, v = 0)
lm.Moran <- lm(Moran$Y_std_lag ~ Moran$Y_std)
abline(lm.Moran, lty = 2, lwd = 1, col = "red")

legend(-1, -2, "Bas-Bas", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(-1, 2, "Bas-Haut", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(2, -2, "Haut-Bas", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(2, 2, "Haut-Haut", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")
legend(0, 0, "Non Sign.", xjust = 0.5,  yjust = 0.5, cex = 0.9,
       x.intersp = -0.5, y.intersp = 0.1, bg = "#ffffff90")

# Cartographie
mf_map(type = "typo", x = Moran, var = "q", val_order = levels(Moran$q),
       pal = unique(cols), border = NA, leg_val_cex = 0.7,
       leg_title = NA, leg_pos = "bottomleft")
mf_map(dep,
       col = "#000",
       add = TRUE)













