
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

library(sf)
library(mapsf)
library(asf)


# Telechargement des objets d'Aliette deposes sur le sharedocs
mar <- asf_mar()


# Fond de carte ---------------------------------------------------------------
irisf <- mar$sf
tabl <- mar$r

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

dep <- asf_borders(irisf,
                   by = "DEP_CODE", 
                   keep = 0.05)

# Choix d'une palette de couleurs
pal <- asf_palette("kelp")

# Utilisation du package mapsf
mf_map(fondata, 
       var = "C20_POP15P_CS6", 
       type = "choro", 
       nbreaks = 6, 
       border = NA)

mf_map(point, 
       add = TRUE)

mf_label(label, 
         var = "label", 
         cex = 0.8)

mf_map(dep, 
       col = "#ffffff", 
       lwd = 0.5, 
       add = TRUE)


# Creation de graphiques ------------------------------------------------------
mar <- asf_mar(sf = FALSE)

data <- read.csv("input/csp_2020.csv")
tabl <- mar$r

tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE) 
tmp <- tmp[, c(1, 15, 4:13)]

tabl <- mar$ar02$d.irisr.pass
tmp <- merge(tmp, tabl, by = "IRISF_CODE", all.x = TRUE)
tmp <- tmp[, c(2, 1, 16, 18, 3:12)]

tabl <- mar$ar02$d.irisr.app
tmp <- merge(tmp, tabl, by = "IRISrD_CODE", all.x = TRUE)
tmp <- tmp[, c(1, 5:14, 36:39)]

asf_plot_typo(tmp,
              vars = c(4:11),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7), 
              order.t = c("5", "1", "2", "3", "4", "0")
              )

asf_plot_vars(tmp,
              vars = c(4:11),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7),
              order.t = c("5", "1", "2", "3", "4", "0")
              )

asf_plot_vars(tmp,
              vars = c(4),
              typo = "TAAV2017"
              )
