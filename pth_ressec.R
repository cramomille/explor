
#                                     SPATIALISATION DES RESIDENCES SECONDAIRES
#
#                                                                antoine beroud
#                                                                      mai 2026

library(sf)
library(asf)
library(mapsf)


# Fond geographique
mar <- asf_mar(
  md = "iris_xxxx", 
  ma = "iris_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE")
fond <- asf_drom(fond)
fond_05 <- asf_simplify(fond, keep = 0.1)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = c("5", "4", "Dijon", "Reims", "Rouen"),
              nb_cols = 7)

palette <- c(
  "jj" = "#e9e5ec",
  "lj" = "#ded8de",
  "mj" = "#a5d7d5",
  "hj" = "#5abeb9",
  
  "jl" = "#ded8de",
  "ll" = "#ded8de",
  "ml" = "#a5d7d5",
  "hl" = "#5abeb9",
  
  "jm" = "#f5b3bd",
  "lm" = "#f5b3bd",
  "mm" = "#b3a6af",
  "hm" = "#00889d",
  
  "jh" = "#ed6c77",
  "lh" = "#ed6c77",
  "mh" = "#c2435e",
  "hh" = "#564770"
)

# Donnees sur les residences secondaires selon le decile de revenus du menage 
# proprietaire
x <- read.csv("input/ql_ressecdecile_irisr2.csv")

class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}

x$v1_class <- class(x$ql_nb_ressec_D9)
x$v2_class <- class(x$ql_nb_ressec_D10)

x$v1_v2_class <- paste0(x$v1_class, x$v2_class)

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

mf_map(c, "v1_v2_class", type = "typo", pal = palette, border = NA, 
       val_order = c("jj", "lj", "mj", "hj",
                     "jl", "ll", "ml", "hl",
                     "jm", "lm", "mm", "hm",
                     "jh", "lh", "mh", "hh"))
