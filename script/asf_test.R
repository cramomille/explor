
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

# remove.packages("rmapshaper")
# remove.packages("mapinsetr")
# remove.packages("asf")
# 
# remotes::install_gitlab("atlas-social-de-la-france/asf",
#                         host = "gitlab.huma-num.fr",
#                         build_vignettes = TRUE)

library(sf)
library(mapsf)
library(asf)
library(ggplot2)


data <- data.frame(
  com = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
  tav = c("1", "1", "1", "2", "2", "2", "3", "3", "4", "4"),
  cav = c("1", "2", "3", "1", "2", "3", "1", "2", "1", "2"),
  pop = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
  m50 = c(20, 30, 50, 80, 130, 210, 220, 230, 450, 460),
  p50 = c(30, 70, 100, 120, 120, 90, 130, 170, 0, 40),
  cah = c("c1", "c2", "c2", "c3", "c3", "c2", "c3", "c4", "c1", "c1")
)

palette <- asf_palette(type = "qua", nb = 4)

asf_plot_typo(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_typo(data, vars = c("m50", "p50"), typo = "tav", pal = palette)
asf_plot_vars(data, vars = c("m50", "p50"), typo = "tav", pal = palette)



asf_plot_typa(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, taille = TRUE)






# ex <- asf_example()
# 
# geom <- ex$epci_2023
# 
# # poly <- asf_drom(geom, id = "DEP")
# poly <- geom[geom$DEP == "06", ]
# 
# poin <- st_centroid(poly)
# lign <- st_cast(poly[poly$EPCI == "240600593", ], "MULTILINESTRING")
# 
# poly <- st_transform(poly, crs = "EPSG:2154")
# poin <- st_transform(poin, crs = "EPSG:2154")
# lign <- st_transform(lign, crs = "EPSG:2154")
# 
# mf_map(poly)
# mf_map(poin, add = TRUE)
# mf_map(lign, col = "white", add = TRUE)
# 
# z_poly <- asf_zoom(poly, coords = c(7.174, 43.784), r = 20000)
# z_poin <- asf_zoom(poin, coords = c(7.174, 43.784), r = 20000, f_ref = poly)
# z_lign <- asf_zoom(lign, coords = c(7.174, 43.784), r = 20000, f_ref = poly)
# 
# zoom_poly <- z_poly$zooms
# zoom_poin <- z_poin$zooms
# zoom_lign <- z_lign$zooms
# 
# mf_map(zoom_poly)
# mf_map(zoom_poin, add = TRUE)
# mf_map(zoom_lign, add = TRUE)
# 
# poly <- rbind(poly, zoom_poly)
# poin <- rbind(poin, zoom_poin)
# lign <- rbind(lign, zoom_lign)
# 
# mf_map(poly)
# mf_map(poin, add = TRUE)
# mf_map(lign, col = "white", add = TRUE)



# Fond de carte ---------------------------------------------------------------
mar <- asf_mar(md = "iris_xxxx", ma = "com_r2", geom = TRUE)

tabl <- mar$tabl
geom <- mar$geom

# Repositionnement des DROM 
geom <- asf_drom(geom, id = "IRISF_CODE")

# Creation des communes agregees a partir des iris de reference
comr <- asf_fond(geom, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR2_CODE", 
                 keep = "DEP") 

# Creation de zooms
z <- asf_zoom(comr, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

# Simplification des geometries du fond de carte
comr_simply <- asf_simplify(comr)


# Tableau de donnees ----------------------------------------------------------
data <- read.csv("input/csp_2020.csv")

comr_data <- asf_data(data, 
                      tabl, 
                      by.x = "IRIS", 
                      by.y = "IRIS_CODE", 
                      maille = "COMR2_CODE", 
                      vars = c(4:13), 
                      funs = c("sum"))


# Jointure --------------------------------------------------------------------
fondata <- asf_fondata(comr_simply, 
                       zoom, 
                       comr_data, 
                       by = "COMR2_CODE")


# Creation de cartes ----------------------------------------------------------
# Calcul de la part des ouvriers et ouvrieres
fondata$pct_var <- fondata$C20_POP15P_CS6 / fondata$C20_POP15P * 100

# Definition des bornes des classes
q6 <- quantile(fondata$pct_var, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

# Creation des limites departementales
geom$DEP_CODE <- substr(geom$COMF_CODE, 1, 2)
dep <- asf_borders(geom, by = "DEP_CODE", keep = 0.01)

# Choix d'une palette de couleurs
pal <- asf_palette("seq")

# Utilisation du package mapsf
mf_map(fondata, 
       var = "pct_var", 
       type = "choro", 
       breaks = q6, 
       pal = pal, 
       border = NA)

mf_map(dep, 
       col = "#ffffff", 
       lwd = 0.5, 
       add = TRUE)

mf_map(point, 
       col = "red", 
       add = TRUE)

mf_label(label, 
         var = "label", 
         cex = 0.8)


# Creation de graphiques ------------------------------------------------------
data <- read.csv("input/csp_2020.csv")

tabl <- asf_mar(md = "iris", ma = "irisr")

tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE)

pal <- asf_palette(pal = "tulipe", nb = 8)
asf_plot_typo(tmp,
              vars = c(6:13),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7), 
              order.t = c("5", "1", "2", "3", "4", "0"),
              pal = pal
              )

pal <- asf_palette(pal = "tulipe", nb = 6)
asf_plot_vars(tmp,
              vars = c(6:13),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7),
              order.t = c("5", "1", "2", "3", "4", "0"),
              pal = pal
              )

pal <- asf_palette(pal = "tulipe", nb = 6)
asf_plot_vars(tmp,
              vars = c(6),
              typo = "TAAV2017",
              pal = pal
              )
