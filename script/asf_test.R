
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

di_2020 <- read.csv("output/datatest/csp_i_2020.csv")[, -1]
di_2023 <- read.csv("output/datatest/csp_i_2023.csv")[, -1]
dc_2020 <- read.csv("output/datatest/csp_c_2020.csv")[, -1]
dc_2023 <- read.csv("output/datatest/csp_c_2023.csv")[, -1]

f <- asf_maa(geom = TRUE)
f <- asf_drom(f, id = "IRISF_CODE")
f <- asf_simplify(f)

mf_map(f)



# .ixxxx_to_i2023 ----
x <- di_2020
t <- asf_maa(md = "iris", ma = "irisf")
y <- asf_data(x, t, by = "IRIS_CODE",
              maille = "IRISF_CODE",
              vars = c(2:11),
              funs = "sum")

sum(x$P20_POP) - sum(y$P20_POP)

z <- asf_fondata(f = f, d = y, by = "IRISF_CODE")
z$test <- z$C20_POP15P_CS3 / z$C20_POP15P * 100

mf_map(z,
       var = "test",
       type = "choro",
       nbreaks = 6,
       border = NA)
       


# .ixxxx_to_i2023r ----
x <- di_2020
t <- asf_maa(md = "iris", ma = "irisr")
y <- asf_data(x, t, by = "IRIS_CODE",
              maille = "IRISrD_CODE",
              vars = c(2:11),
              funs = "sum")

sum(x$P20_POP) - sum(y$P20_POP)

g <- asf_fond(f, t, by = "IRISF_CODE", maille = "IRISrD_CODE")

z <- asf_fondata(f = g, d = y, by = "IRISrD_CODE")
z$test <- z$C20_POP15P_CS3 / z$C20_POP15P * 100

mf_map(z,
       var = "test",
       type = "choro",
       nbreaks = 6,
       border = NA)



# .ixxxx_to_c2023  ----
x <- di_2020
t <- asf_maa(md = "iris", ma = "comf")
y <- asf_data(x, t, by = "IRIS_CODE",
              maille = "COMF_CODE",
              vars = c(2:11),
              funs = "sum")

sum(x$P20_POP) - sum(y$P20_POP)

f <- asf_fond(f, maille = "COMF_CODE")

z <- asf_fondata(f = f, d = y, by = "COMF_CODE")
z$test <- z$C20_POP15P_CS3 / z$C20_POP15P * 100

mf_map(z,
       var = "test",
       type = "choro",
       nbreaks = 6,
       border = NA)



# .ixxxx_to_c2023r ----
x <- di_2020
t <- asf_maa(md = "iris", ma = "comr")
y <- asf_data(x, t, by = "IRIS_CODE",
              maille = "COMR_CODE",
              vars = c(2:11),
              funs = "sum")

sum(x$P20_POP) - sum(y$P20_POP)

g <- asf_fond(f, t, by = "COMF_CODE", maille = "COMR_CODE")

z <- asf_fondata(f = g, d = y, by = "COMR_CODE")
z$test <- z$C20_POP15P_CS3 / z$C20_POP15P * 100

mf_map(z,
       var = "test",
       type = "choro",
       nbreaks = 6,
       border = NA)



# .i2023_to_i2023r ----
x <- di_2023
t <- asf_maa(md = "iris", ma = "irisr", a2023 = TRUE)
y <- asf_data(x, t, 
              by = "IRISF_CODE",
              maille = "IRISrS_CODE",
              vars = c(2:11),
              funs = "sum")

sum(x$P20_POP) - sum(y$P20_POP)

g <- asf_fond(f, t, by = "IRISF_CODE", maille = "IRISrS_CODE")

z <- asf_fondata(f = g, d = y, by = "IRISrS_CODE")
z$test <- z$C20_POP15P_CS3 / z$C20_POP15P * 100

mf_map(z,
       var = "test",
       type = "choro",
       nbreaks = 6,
       border = NA)























# .i2023_to_c2023  ----
t <- asf_maa(md = "iris", ma = "comf", a2023 = TRUE)

# .i2023_to_c2023r ----
t <- asf_maa(md = "iris", ma = "comr", a2023 = TRUE)



# .cxxxx_to_c2023  ----
t <- asf_maa(md = "com", ma = "comf")

# .cxxxx_to_c2023r ----
t <- asf_maa(md = "com", ma = "comr")

# .c2023_to_c2023r ----
t <- asf_maa(md = "com", ma = "comr", a2023 = TRUE)




























t <- asf_maa(md = "iris", ma = "irisr")

y <- asf_data(x, 
              t, 
              by.x = "IRIS", 
              by.y = "IRIS_CODE", 
              maille = "IRISrD_CODE", 
              vars = c(4:13), 
              funs = "sum")

sum(x$P20_POP, na.rm = TRUE)
sum(y$P20_POP, na.rm = TRUE)







# Fond de carte ---------------------------------------------------------------
mar <- asf_maa(md = "iris", ma = "comr", geom = TRUE)

tabl <- mar$tabl
geom <- mar$geom

# Repositionnement des DROM 
geom <- asf_drom(geom, 
                 id = "IRISF_CODE")

# Creation du fond des comr a partir des comf
comr <- asf_fond(geom, 
                 tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR_CODE") 

# Creation de zooms
z <- asf_zoom(comr, 
              places = c("Paris", 
                         "Avignon", 
                         "Bergerac", 
                         "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

# Simplification des geometries du fond de carte
comr_simply <- asf_simplify(comr)


# Tableau de donnees ----------------------------------------------------------
data <- read.csv("input/csp_2020.csv")

comr_data <- asf_data(data, 
                      tabl, 
                      by.x = "COM", 
                      by.y = "COM_CODE", 
                      maille = "COMR_CODE", 
                      vars = c(4:13), 
                      funs = c("sum"))


# Jointure --------------------------------------------------------------------
fondata <- asf_fondata(comr_simply, 
                       zoom, 
                       comr_data, 
                       by = "COMR_CODE")


# Creation de cartes ----------------------------------------------------------
# Creation des limites departementales
geom$DEP_CODE <- substr(geom$COMF_CODE, 1, 2)

dep <- asf_borders(geom, by = "DEP_CODE", keep = 0.01)

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
