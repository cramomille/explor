
#                              SPATIALISATION DES DIFFERENTS DECILES DE REVENUS
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



# Donnees sur les menages selon leur decile de revenus
x <- read.csv("input/ql_mendecile_irisr2.csv")

class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}

x$v1_class <- class(x$ql_nb_men_D3)
x$v2_class <- class(x$ql_nb_men_D9)

x$v1_v2_class <- paste0(x$v1_class, x$v2_class)

c <- asf_fondata(f = fond_05, z = z[[1]], d = x, by = "IRISrD_CODE")

mf_map(c, "v1_v2_class", type = "typo", pal = palette, border = NA, 
       val_order = c("jj", "lj", "mj", "hj",
                     "jl", "ll", "ml", "hl",
                     "jm", "lm", "mm", "hm",
                     "jh", "lh", "mh", "hh"))





# Donnees test pyramide ----
x <- read.csv("input/ql_mendecile_irisr2.csv")

class_decile <- function(x) {
  cut(
    x,
    breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
    labels = c("j", "l", "m", "h"),
    include.lowest = TRUE
  )
}

deciles <- paste0("ql_nb_men_D", 1:10)

# matrice de layout triangulaire (9 x 9)
lay <- matrix(0, nrow = 9, ncol = 9)

k <- 1
for (i in 1:9) {
  for (j in i:9) {
    lay[i, j] <- k
    k <- k + 1
  }
}

pdf("output/planche_deciles_triangle2.pdf", width = 18, height = 18)

layout(lay)

par(
  mar = c(0.2, 0.2, 1.2, 0.2),
  oma = c(1, 1, 2, 1)
)

for (i in 1:9) {
  for (j in i:9) {
    
    v1 <- deciles[i]
    v2 <- deciles[j + 1]
    
    x$v1_class <- class_decile(x[[v1]])
    x$v2_class <- class_decile(x[[v2]])
    
    x$v1_v2_class <- paste0(x$v1_class, x$v2_class)
    
    c <- asf_fondata(
      f = fond_05,
      d = x,
      by = "IRISrD_CODE"
    )
    
    mf_map(
      c,
      "v1_v2_class",
      type = "typo",
      pal = palette,
      border = NA,
      leg_pos = NA,
      val_order = c(
        "jj", "lj", "mj", "hj",
        "jl", "ll", "ml", "hl",
        "jm", "lm", "mm", "hm",
        "jh", "lh", "mh", "hh"
      )
    )
    
    title(
      main = paste0("D", i, " / D", j + 1),
      cex.main = 0.8,
      line = 0
    )
  }
}

mtext("Croisement des déciles (triangle supérieur)", outer = TRUE, cex = 1.2)

dev.off()
