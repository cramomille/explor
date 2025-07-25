
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

# remove.packages("rmapshaper")
# remove.packages("mapinsetr")
remove.packages("asf")

remotes::install_gitlab("atlas-social-de-la-france/asf",
                        host = "gitlab.huma-num.fr",
                        build_vignettes = TRUE)

library(sf)
library(mapsf)
library(asf)


# Fond de carte ---------------------------------------------------------------
mar <- asf_mar(maille = "irisrs")

irisf <- mar$geom
tabl <- mar$tabl

# Repositionnement des DROM 
irisf <- asf_drom(irisf, 
                  id = "IRISF_CODE")

# Creation du fond des irisr a partir des irisf
irisr <- asf_fond(irisf, 
                  tabl, 
                  by = "IRISF_CODE", 
                  maille = "IRISrS_CODE") 

# Creation de zooms
z <- asf_zoom(irisr, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

# Simplification des geometries du fond de carte
irisr_simply <- asf_simplify(irisr)


# Data ------------------------------------------------------------------------
iris_data <- read.csv("input/csp_2020.csv")

irisr_data <- asf_data(iris_data, 
                       tabl, 
                       by.x = "IRIS",
                       by.y = "IRIS_CODE", 
                       maille = "IRISrS_CODE",
                       vars = c(4:13), 
                       funs = c("sum"))


# Jointure --------------------------------------------------------------------
fondata <- asf_fondata(irisr_simply,
                       zoom,
                       irisr_data,
                       by = "IRISrS_CODE")


# Creation de cartes ----------------------------------------------------------
# Creation des limites departementales
irisf$DEP_CODE <- substr(irisf$IRISF_CODE, 1, 2)

dep <- asf_borders(irisf, by = "DEP_CODE", keep = 0.1)

# Choix d'une palette de couleurs
pal <- asf_palette("seq")

# Utilisation du package mapsf
mf_map(fondata, 
       var = "C20_POP15P_CS6", 
       type = "choro", 
       nbreaks = 6, 
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
tabl <- asf_mar(maille = "irisrs", 
                geom = FALSE)

data <- read.csv("input/csp_2020.csv")

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


