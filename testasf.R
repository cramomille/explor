
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

library(sf)
library(mapsf)
library(asf)


###############################################################################
######################################## TEST MAILLAGE COMPOSITE D'ALIETTE ROUX

# Telechargement des objets d'Aliette deposes sur le sharedocs
mar <- asf_mar()

# Fond de carte ---------------------------------------------------------------
fond <- mar$ar01$sf.irisf
tabl <- mar$ar02$d.irisr.pass

# Repositionnement des DROM
fond <- asf_drom(fond, 
                 id = "IRISF_CODE")

# Creation des limites departementales
dep <- asf_dep(fond,
               id = "IRISF_CODE", 
               keep = 0.05)

# Creation du fond des irisr a partir des irisf
fond_aggreg <- asf_fond(fond, 
                        tabl, 
                        by = "IRISF_CODE", 
                        maille = "IRISrS_CODE") 

# Creation de zooms
z <- asf_zoom(fond_aggreg, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels

# Simplification des geometries du fond de carte
fond_simply <- asf_simplify(fond_aggreg)


# Data ------------------------------------------------------------------------
data <- mar$data$d.datatest

data_aggreg <- asf_data(data, 
                        tabl, 
                        by.x = "IRIS",
                        by.y = "IRIS_CODE", 
                        maille = "IRISrS_CODE",
                        vars = c(4:13), 
                        funs = c("sum"))


# Jointure --------------------------------------------------------------------
fondata <- asf_fondata(data_aggreg, 
                       fond_simply, 
                       zoom, 
                       id = c("IRISrS_CODE", "IRISrS_CODE"))


# Creation de cartes ----------------------------------------------------------
mf_map(fondata, 
       var = "C20_POP15P_CS6", 
       type = "choro", 
       nbreaks = 6, 
       border = NA)

mf_label(label, 
         var = "nom", 
         cex = 0.8)

mf_map(dep, 
       col = "white", 
       lwd = 0.5, 
       add = TRUE)


# Creation de graphiques ------------------------------------------------------
mar <- asf_mar(sf = FALSE)

data <- mar$data$d.datatest
tabl <- mar$ar01$d.irisf.pass
tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE) 
tmp <- tmp[, c(1, 15, 4:13)]

tabl <- mar$ar02$d.irisr.pass
tmp <- merge(tmp, tabl, by = "IRISF_CODE", all.x = TRUE)
tmp <- tmp[, c(2, 1, 16, 18, 3:12)]

tabl <- mar$ar02$d.irisr.app
tmp <- merge(tmp, tabl, by = "IRISrD_CODE", all.x = TRUE)
tmp <- tmp[, c(1, 5:14, 36:39)]

asf_plotypo(data = tmp,
            vars = c(4:11),
            typo = "TAAV2017", 
            order_vars = c(1:6, 8, 7), 
            order_typo = c("5", "1", "2", "3", "4", "0"))

asf_plotvar(data = tmp,
            vars = c(4:11),
            typo = "TAAV2017", 
            order_vars = c(1:6, 8, 7),
            order_typo = c("5", "1", "2", "3", "4", "0"))

asf_plotvar(data = tmp,
            vars = c(4),
            typo = "TAAV2017")



