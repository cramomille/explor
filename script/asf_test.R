
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

# remove.packages("rmapshaper")
# remove.packages("mapinsetr")
#
# remotes::install_gitlab(repo = "atlas-social-de-la-france/asf",
#                         host = "gitlab.huma-num.fr",
#                         build_vignettes = TRUE,
#                         force = TRUE,
#                         upgrade = "never")

library(sf)
library(asf)
library(mapsf)

# Test asf_mar() --------------------------------------------------------------
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_f")
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r2")
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r5")
tabl <- asf_mar(md = "iris_xxxx", ma = "com_f")
tabl <- asf_mar(md = "iris_xxxx", ma = "com_r2")
tabl <- asf_mar(md = "iris_xxxx", ma = "com_r5")

tabl <- asf_mar(md = "iris_2023", ma = "iris_r2")
tabl <- asf_mar(md = "iris_2023", ma = "iris_r5")
tabl <- asf_mar(md = "iris_2023", ma = "com_f")
tabl <- asf_mar(md = "iris_2023", ma = "com_r2")
tabl <- asf_mar(md = "iris_2023", ma = "com_r5")

tabl <- asf_mar(md = "com_xxxx", ma = "com_f")
tabl <- asf_mar(md = "com_xxxx", ma = "com_r2")
tabl <- asf_mar(md = "com_xxxx", ma = "com_r5")

tabl <- asf_mar(md = "com_2023", ma = "com_r2")
tabl <- asf_mar(md = "com_2023", ma = "com_r5")


# Test utilisation du package -------------------------------------------------
data <- read.csv("input/csp_2020.csv")
geom <- asf_mar(geom = TRUE)
tabl <- asf_mar(md = "iris_xxxx", ma = "com_r5")

data_r <- asf_data(data, tabl, 
                   by.x = "IRIS", by.y = "IRIS_CODE", 
                   maille = "COMr5_CODE", 
                   vars = c(4:13), funs = "sum")

sum(data$P20_POP)
sum(data_r$P20_POP)

geom <- asf_drom(geom)

geom_r <- asf_fond(geom, tabl, 
                   by = "COMF_CODE", 
                   maille = "COMr5_CODE")

mf_map(geom_r)

z <- asf_zoom(geom_r, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

mf_map(z[[1]])

geom_r_simply <- asf_simplify(geom_r)


fondata <- asf_fondata(geom_r_simply, 
                       z[[1]], 
                       data_r, 
                       by = "COMr5_CODE")


# Test carto stat -------------------------------------------------------------
x <- fondata

x$pct_cs6 <- x$C20_POP15P_CS6 / x$C20_POP15P * 100

q6 <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
b9 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9, 1)
bd <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

breaks <- q6

b <- quantile(x$pct_cs6, probs = breaks, na.rm = TRUE)

pal <- asf_palette("div", nb = length(b)-1)

mf_map(x, 
       var = "pct_cs6", 
       type = "choro", 
       breaks = b, 
       pal = pal, 
       border = NA)


y <- fondata

y$pct_cs5 <- y$C20_POP15P_CS5 / y$C20_POP15P * 100
y$pct_cs3 <- y$C20_POP15P_CS3 / y$C20_POP15P * 100

class3 <- function(x) {
  cuts <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x,
      breaks = c(-Inf, cuts, Inf),
      labels = c("l", "m", "h"),
      include.lowest = TRUE)
}

y$cs5_class <- class3(y$pct_cs5)
y$cs3_class <- class3(y$pct_cs3)

y$class <- paste0(y$cs5_class, y$cs3_class)

palette <- c("ll" = "#e3e3e3","lm" = "#8ccaae","lh" = "#00a183",
             "ml" = "#f28d65","mm" = "#a08a6e","mh" = "#00725c",
             "hl" = "#dc0d15","hm" = "#981108","hh" = "#2e2d2d"
             )

mf_map(y, 
       var = "class",
       type = "typo",
       val_order = c("ll","lm","lh","ml","mm","mh","hl","hm","hh"),
       pal = palette,
       border = NA)





