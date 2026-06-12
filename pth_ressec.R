
#                                     SPATIALISATION DES RESIDENCES SECONDAIRES
#
#                                                                antoine beroud
#                                                                      mai 2026

library(sf)
library(asf)
library(mapsf)
library(readxl)
library(janitor)


# UK --------------------------------------------------------------------------
# Fond geographique
dz <- st_read("input/uk/geom/SG_DataZoneBdry_2022/SG_DataZone_Bdry_2022.shp") |>
  st_make_valid() |>
  asf_simplify(keep = 0.2)

world <- st_read("input/monde_1M.gpkg") |>
  st_transform(crs = "EPSG:27700")

world <- world[world$ISO3_CODE %in% c("GBR", "IRL"), ]


# Donnees
data <- read_excel("input/uk/data/household-estimates-by-2022-data-zones.xlsx", 
                   sheet = "2024", 
                   skip = 3) |>
  clean_names()


# Cartographie
c <- merge(dz, data, by.x = "dzcode", by.y = "data_zone_code", all.x = TRUE)


# Map 1 - Second home percent
mf_distr(c$second_homes_percent)

mf_map(c, 
       var = "second_homes_percent", 
       type = "choro", 
       breaks = c(0, 1, 2, 4, 6, max(c$second_homes_percent)), 
       border = NA)

mf_map(c, 
       var = "second_homes", 
       type = "prop", 
       inches = 0.2, 
       col = NA, 
       border = "#000", 
       leg_pos = "topright")

mf_map(world, 
       col = NA, 
       add = TRUE)


# Map 2 - Second home location quotient
mean <- sum(c$second_homes, na.rm = TRUE) /
  sum(c$total_number_of_dwellings, na.rm = TRUE)

c$lq_second_homes <- (
  c$second_homes / c$total_number_of_dwellings
) / mean

mf_distr(c$lq_second_homes)

palette <- c("#57b998", 
             "#8ccaae", 
             "#c2dfc4", 
             "#e3eed6", 
             "#ffefb0", 
             "#ffdb7d", 
             "#fbbf6b", 
             "#f28b52", 
             "#eb5e4f", 
             "#d72739")

mf_map(
  c,
  var = "lq_second_homes",
  type = "choro",
  breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 10, Inf), 
  pal = palette,
  border = NA
)

mf_map(world, 
       col = NA, 
       add = TRUE)



# FR --------------------------------------------------------------------------
# Fond geographique
mar <- asf_mar(
  md = "iris_xxxx", 
  ma = "iris_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "IRISrD_CODE", keep = c("TAAV2017", "CATEAAV2020"))
fond <- asf_drom(fond)
fond_05 <- asf_simplify(fond, keep = 0.1)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = "Lyon", r = 25000)

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
v <- read.csv("input/ql_mendecile_irisr2.csv")
w <- read.csv("input/ql_ressecdecile_irisr2.csv")

x <- merge(v[, -1], w[-1], by = "IRISrD_CODE")

class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}

x$v1_class <- class(x$ql_nb_men_D10)
x$v2_class <- class(x$ql_nb_ressec_D10)

x$v1_v2_class <- paste0(x$v1_class, x$v2_class)

c <- asf_fondata(
  f = fond_05,
  # z = z[[1]],
  d = x,
  by = "IRISrD_CODE")


mf_map(c, "v1_v2_class", type = "typo", pal = palette, border = NA, 
       val_order = c("jj", "lj", "mj", "hj",
                     "jl", "ll", "ml", "hl",
                     "jm", "lm", "mm", "hm",
                     "jh", "lh", "mh", "hh"))

mf_map(c,
       var = "tot_ressec",
       type = "prop",
       inches = 0.2,
       col = NA,
       border = "#000",
       leg_pos = "topright")



mf_map(c, var = "ql_nb_ressec_D9", type = "choro", breaks = "q6", border = NA)
mf_map(c, var = "ql_nb_ressec_D10", type = "choro", breaks = "q6", border = NA)




c <- asf_fondata(z = z[[1]], d = x, by = "COMr2_CODE")

mf_map(c, var = "ql_nb_ressec_D8", type = "choro", breaks = "q6", border = NA)

