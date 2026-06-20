
#                                     SPATIALISATION DES RESIDENCES SECONDAIRES
#                                       ET DES MENAGES SELON LEUR NIVEAU DE VIE
#
#                                                                antoine beroud
#                                                                      mai 2026

library(sf)
library(asf)
library(mapsf)
library(readxl)
library(janitor)


# UK --------------------------------------------------------------------------
# Fonds geographiques
world <- st_read("input/monde_1M.gpkg") |>
  st_transform(crs = "EPSG:27700") |>
  subset(ISO3_CODE %in% c("GBR", "IRL")) |>
  asf_simplify(keep = 0.1)

dz <- st_read("input/pth/uk/geom/SG_DataZoneBdry_2022/SG_DataZone_Bdry_2022.shp") |>
  st_make_valid() |>
  asf_simplify(keep = 0.2)

names(dz)[1] <- "data_zone_code"
  
# ca <- asf_fond(dz[, 1], data[, c(1, 3)], 
#                by.x = "dzcode", by.y = "data_zone_code",
#                maille = "council_area_code")

z <- asf_zoom(dz, coords = c(-4.2583, 55.8616,
                             -3.1882, 55.9532))



# Donnees
data <- read_excel("input/pth/uk/data/household-estimates-by-2022-data-zones.xlsx", 
                   sheet = "2024", 
                   skip = 3) |>
        clean_names()

# data <- asf_data(data, 
#                  maille = "council_area_code", 
#                  vars = c(5:12), funs = "sum", 
#                  keep = "council_area_name")
# 
# c <- merge(ca, data, by = "council_area_code", all.x = TRUE)



# Cartographie
c <- asf_fondata(dz, z[[1]], data, by = "data_zone_code")

# Map 1 - Second home percent
sum(c$second_homes) / sum(c$total_number_of_dwellings) * 100

c$second_homes_pct <- c$second_homes / c$total_number_of_dwellings *100

mf_distr(c$second_homes_pct)

mf_map(c, 
       var = "second_homes_pct", 
       type = "choro", 
       breaks = c(0, 1, 2, 4, 6, max(c$second_homes_pct)), 
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
## irisr2 ----
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
fond_01 <- asf_simplify(fond, keep = 0.1)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = "Lyon", r = 25000)

# Donnees sur les residences secondaires selon le decile de revenus du menage 
# proprietaire
v <- read.csv("input/pth/ql_mendecile_irisr2.csv")
w <- read.csv("input/pth/ql_ressecdecile_irisr2.csv")

x <- merge(v[, -1], w[-1], by = "IRISrD_CODE")

c <- asf_fondata(
  f = fond_01,
  # z = z[[1]],
  d = x,
  by = "IRISrD_CODE")



## comr2 ----
mar <- asf_mar(
  md = "com_xxxx", 
  ma = "com_r2", 
  geom = TRUE, 
  dir = "input/mar/"
)

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(geom, tabl, by = "COMF_CODE", maille = "COMr2_CODE", keep = c("TAAV2017", "CATEAAV2020"))
fond <- asf_drom(fond)
fond_01 <- asf_simplify(fond, keep = 0.1)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = "Lyon", r = 25000)

# Donnees sur les residences secondaires selon le decile de revenus du menage 
# proprietaire
v <- read.csv("input/ql_mendecile_comr2.csv")
w <- read.csv("input/ql_ressecdecile_comr2.csv")

x <- merge(v[, -1], w[-1], by = "COMr2_CODE")

c <- asf_fondata(
  f = fond_01,
  # z = z[[1]],
  d = x,
  by = "COMr2_CODE")





## cartographie ----
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

class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}


c$v1_class <- class(c$ql_nb_men_D9)
c$v2_class <- class(c$ql_nb_ressec_D9)

c$v1_v2_class <- paste0(c$v1_class, c$v2_class)


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







data <- readRDS("input/pth/riche_ql_2022_secret.rds")

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
fond_01 <- asf_simplify(fond, keep = 0.1)
fond_09 <- asf_simplify(fond, keep = 0.9)

z <- asf_zoom(f = fond_09, 
              places = c("Paris", "Lyon"), 
              r = 25000, 
              nb_cols = 2)

c <- asf_fondata(
  f = fond_01,
  z = z[[1]],
  d = data,
  by = "IRISrD_CODE")


class <- function(x) {
  cut(x,
      breaks = c(min(x, na.rm = TRUE), 0.6, 1, 1.5, max(x, na.rm = TRUE)),
      labels = c("j", "l", "m", "h"),
      include.lowest = TRUE)
}



c$v1_class <- class(c$ql_c80_90)
c$v2_class <- class(c$ql_c90_95)

c$v1_class <- class(c$ql_c80_90)
c$v2_class <- class(c$ql_c90_100)

c$v1_class <- class(c$ql_c90_99)
c$v2_class <- class(c$ql_c100)

c$v1_v2_class <- paste0(c$v1_class, c$v2_class)


mf_map(c, "v1_v2_class", type = "typo", pal = palette, border = NA, 
       val_order = c("jj", "lj", "mj", "hj",
                     "jl", "ll", "ml", "hl",
                     "jm", "lm", "mm", "hm",
                     "jh", "lh", "mh", "hh"))







