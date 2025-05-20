
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

library(sf)
library(mapsf)
library(asf)


mar <- asf_mar()

sf.irisf <- mar$ar01$sf.irisf
sf.comf <- mar$ar01$sf.comf

d.comf.pass <- mar$ar01$d.comf.pass
d.irisf.pass <- mar$ar01$d.irisf.pass
d.comf.app <- mar$ar01$d.comf.app

tabl <- merge(d.irisf.pass, d.comf.pass, by = "COMF_CODE", all = TRUE)
tabl <- tabl[, c(3:5, 7, 1, 10)]

tmp <- tabl[apply(is.na(tabl), 1, any), ]


tabl <- merge(tabl, d.comf.app[, -c(1, 3)], by = "COMF_CODE", all = TRUE)

tabl1 <- tabl[, c(2:5, 1, 6:23)]

rm(sf.irisf,
   sf.comf,
   d.comf.pass,
   d.irisf.pass,
   d.comf.app,
   tabl,
   tmp)


###############################################################################

sf.irisr.d <- mar$ar02$sf.irisr.d
sf.irisr.s <- mar$ar02$sf.irisr.s

d.irisr.pass <- mar$ar02$d.irisr.pass
d.irisr.app <- mar$ar02$d.irisr.app

tabl <- merge(d.irisr.pass, d.irisr.app[, -c(3)], by = "IRISrD_CODE")
tabl2 <- tabl[, c(3:7, 1, 8, 12, 13)]


tmp <- tabl[, c(
  "OM_CODE",
  
  "EPCI", 
  "NATURE_EPCI",
  
  "ARR",    
  "CV",             
  "UU2020",         
  "TUU2017",          
  "TDUU2017",         
  "BV2022",        
  "ZE2020",
  
  "AAV2020",          
  "TAAV2017",         
  "TDAAV2017",        
  "CATEAAV2020",
  
  "DEP",
  "REG"
  )]

names(tabl)

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
point <- z$points

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
fondata <- asf_fondata(fond_simply,
                       zoom,
                       data_aggreg,
                       by = "IRISrS_CODE")


# Creation de cartes ----------------------------------------------------------
# Creation des limites departementales
dep <- fond
dep$DEP_CODE <- substr(dep$IRISF_CODE, 1, 2)
dep <- asf_borders(dep,
                   by = "DEP_CODE", 
                   keep = 0.05)

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




























































