
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
library(asf)
library(mapsf)


# FOND DE CARTE ---------------------------------------------------------------
## asf_mar() ----
# a - Telechargement (depuis le Sharedocs) et lecture des geometries de 
# reference (IRIS 2023)
geom <- asf_mar(
  geom = TRUE
)

# b - Lecture (depuis ma machine) des geometries de reference (IRIS 2023)
geom <- asf_mar(
  geom = TRUE, 
  dir = "chemin/vers/mar"
)

# c - Telechargement (depuis le Sharedocs) et lecture d'une table de passage 
# entre les IRIS et les communes regroupees (2 000 hab)
tabl <- asf_mar(
  md = "iris_xxxx", 
  ma = "com_r2"
)

# d - Lecture (depuis ma machine) d'une table de passage entre les IRIS et les 
# communes regroupees (2 000 hab)
tabl <- asf_mar(
  md = "iris_xxxx", 
  ma = "com_r2", 
  dir = "chemin/vers/mar"
)

# e - Telechargement (depuis le Sharedocs) et lecture d'une table de passage 
# entre les IRIS et les communes regroupees (2 000 hab) et des geometries de 
# reference (IRIS 2023)
x <- asf_mar(
  md = "iris_xxxx", 
  ma = "com_r2", 
  geom = TRUE
)

geom <- x$geom
tabl <- x$tabl

# f - Lecture (depuis ma machine) d'une table de passage entre les IRIS et les 
# communes regroupees (2 000 hab) et des geometries de reference (IRIS 2023)
x <- asf_mar(
  md = "iris_xxxx",
  ma = "com_r2",
  geom = TRUE,
  dir = "chemin/vers/mar"
)

geom <- x$geom
tabl <- x$tabl


## asf_drom() ----
# a - Repositionement des geometries des DROM
geom_drom <- asf_drom(
  f = geom, 
  id = "IRISF_CODE"
)

# b - Repositionement des geometries des DROM et reprojection du resultat
geom_drom <- asf_drom(
  f = geom, 
  id = "IRISF_CODE", 
  epsg = "32631"
)


## asf_fond() ----
# a - Agregation des entites d'une couche pour former un autre maillage (les 
# identifiants de cet autre maillage se trouvent dans la couche)
com_r2 <- asf_fond(
  f = geom_drom, 
  maille = "COMR2_CODE"
) 

# b - Agregation des entites d'une couche pour former un autre maillage (les 
# identifiants de cet autre maillage se trouvent dans une table de passage)
com_r2 <- asf_fond(
  f = geom_drom, 
  t = tabl, 
  by = "COMF_CODE", 
  maille = "COMR2_CODE"
) 

# c - Agregation des entites d'une couche pour former un autre maillage et
# conservation d'autres identifiants d'agregation
com_r2 <- asf_fond(
  f = geom_drom,
  t = tabl, 
  by = "COMF_CODE", 
  maille = "COMR2_CODE", 
  keep = c("DEP", "REG")
)


## asf_zoom() ----
# a - Creation de zooms de 20 km de rayon autour de communes pre-renseignees a 
# partir de leur nom
z <- asf_zoom(
  f = com_r2, 
  places = c("Paris", "Avignon", "Bergerac", "Annecy"),
  r = 20000
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# b - Creation de zooms de 20 km de rayon autour de communes pre-renseignees a 
# partir de leur categorie d'aav
z <- asf_zoom(
  f = com_r2, 
  places = c("4"),
  r = 20000
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# c - Creation de zooms de 20 km de rayon autour de communes pre-renseignees a 
# partir de leur nom ou de leur categorie d'aav
z <- asf_zoom(
  f = com_r2, 
  places = c("Paris", "4", "Avignon", "Bergerac", "Annecy"),
  r = 20000
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# d - Creation de zooms autour de differents points
z <- asf_zoom(
  f = com_r2, 
  coords = c(2.0317, 44.6085, 3.5731, 47.7979, -2.0477, 48.4540)
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# e - Creation de zooms autour de differents points auxquels on attribue un nom
z <- asf_zoom(
  f = com_r2, 
  coords = c(2.0317, 44.6085, 3.5731, 47.7979, -2.0477, 48.4540), 
  labels = c("Figeac", "Auxerre", "Dinan")
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# f - Creation de zooms avec toutes ces fonctionnalites
z <- asf_zoom(
  f = com_r2, 
  places = c("Paris", "4", "Avignon", "Bergerac", "Annecy"),
  coords = c(2.0317, 44.6085, 3.5731, 47.7979, -2.0477, 48.4540), 
  labels = c("Figeac", "Auxerre", "Dinan")
)

zoom <- z$zooms
label <- z$labels
point <- z$points

# g - Creation de zooms sur deux couches differentes qu'on souhaite ensuite 
# superposer
z1 <- asf_zoom(
  f = com_r2, 
  places = c("Paris", "Avignon", "Bergerac", "Annecy")
)

z2 <- asf_zoom(
  f = st_centroid(com_r2), 
  places = c("Paris", "Avignon", "Bergerac", "Annecy"), 
  f_ref = com_r2
)

zoom1 <- z1$zooms
zoom2 <- z2$zooms

label <- z1$labels
point <- z1$points


# TABLEAU DE DONNEES ----------------------------------------------------------
## asf_data() ----
# a - Agregation des cellules d'un tableau de donnees (la colonne d'identifiants 
# utilisee pour l'agregation se trouvent dans la couche)
data_r2 <- asf_data(
  d = data,
  maille = "COMR2_CODE", 
  vars = c(3:6), 
  funs = "sum"
)

# b - Agregation des cellules d'un tableau de donnees (la colonne d'identifiants 
# utilisee pour l'agregation se trouvent dans une table de passage)
data_r2 <- asf_data(
  d = data, 
  t = tabl, 
  by = "COM_CODE", 
  maille = "COMR2_CODE",
  vars = c(3:6), 
  funs = "sum"
)

# c - Agregation des cellules d'un tableau de donnees et conservation d'autres 
# identifiants d'agregation
data_r2 <- asf_data(
  d = data, 
  t = tabl, 
  by = "COM_CODE", 
  maille = "COMR2_CODE",
  vars = c(3:6), 
  funs = "sum",
  keep = c("DEP", "REG")
)


## asf_fondata() ----
# a - Jointure entre un fond et des donnees
fondata_r2 <- asf_fondata(
  f = com_r2,
  d = data_r2, 
  by = "COMR2_CODE"
)

# b - Jointure entre un fond, des zooms et des donnees
fondata_r2 <- asf_fondata(
  f = com_r2,
  z = zoom,
  d = data_r2, 
  by = "COMR2_CODE"
)

# b - Jointure entre des zooms et des donnees
fondata_r2 <- asf_fondata(
  z = zoom,
  d = data_r2, 
  by = "COMR2_CODE"
)


# CREATION DE CARTES ----------------------------------------------------------
## asf_simplify() ----
# a - Simplification des geometries d'une couche (par defaut : conservation de 
# 10% des points)
com_r2_simply <- asf_simplify(
  f = com_r2
)

# b - Simplification des geometries d'une couche (conservation de la moitie des
# points)
com_r2_simply <- asf_simplify(
  f = com_r2, 
  keep = 0.5
)


## asf_borders() ----
# b - Recuperation des limites communales
dep <- asf_borders(
  f = com_r2, 
  by = "COMR2_CODE"
)

# a - Recuperation des limites departementales
dep <- asf_borders(
  f = com_r2, 
  by = "DEP"
)

# a - Recuperation des limites departementales et simplification des geometries
dep <- asf_borders(
  f = com_r2, 
  by = "DEP",
  keep = 0.1 
)


## asf_palette() ----



# CREATION DE GRAPHIQUES ------------------------------------------------------

## asf_plot_glis() ----

## asf_plot_typo() ----

## asf_plot_vars() ----









z <- asf_zoom(comr, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

comr_simply <- asf_simplify(comr)



data <- read.csv("input/csp_2020.csv")

comr_data <- asf_data(data, 
                      tabl, 
                      by.x = "IRIS", 
                      by.y = "IRIS_CODE", 
                      maille = "COMR2_CODE", 
                      vars = c(4:13), 
                      funs = c("sum"))



fondata <- asf_fondata(comr_simply, 
                       zoom, 
                       comr_data, 
                       by = "COMR2_CODE")


fondata$pct_var <- fondata$C20_POP15P_CS6 / fondata$C20_POP15P * 100

q6 <- quantile(fondata$pct_var, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

geom$DEP_CODE <- substr(geom$COMF_CODE, 1, 2)
dep <- asf_borders(geom, by = "DEP_CODE", keep = 0.01)

pal <- asf_palette("seq")


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


data <- read.csv("input/csp_2020.csv")

tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r2", dir = "input/mar")

tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE)

pal <- asf_palette(pal = "tulipe", nb = 8)
asf_plot_typo(tmp,
              vars = c(6:13),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7), 
              order.t = c("5", "1", "2", "3", "4", "0"),
              pal = pal
              )

asf_plot_typo(tmp,
              vars = c(6:13),
              typo = c("TAAV2017", "CATEAAV2020"), 
              order.t = c("30", "20", "13", "12", "11"),
              pal = pal, 
              eff = TRUE
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




#

data <- data.frame(
  com = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
  tav = c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3"),
  cav = c("1", "1", "2", "2", "1", "1", "2", "2", "1", "2"),
  pop = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
  m50 = c(20, 30, 50, 80, 130, 210, 220, 230, 450, 460),
  p50 = c(30, 70, 100, 120, 120, 90, 130, 170, 0, 40),
  cah = c("c1", "c2", "c2", "c1", "c2", "c2", "c1", "c2", "c1", "c1")
)

palette <- asf_palette(type = "qua")



asf_plot_typa(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_typa(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_typa(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_typa(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_typa(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_typa(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_typa(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_typa(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)


asf_plot_varo(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_varo(data, vars = "m50", typo = "tav", pal = palette)
asf_plot_varo(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_varo(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_varo(data, vars = "m50", typo = c("tav", "cav"), pal = palette)
asf_plot_varo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_varo(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_varo(data, vars = "m50", typo = "tav", pal = palette, eff = TRUE)
asf_plot_varo(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_varo(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_varo(data, vars = "m50", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_varo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)

