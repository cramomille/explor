library(sf)
library(mapsf)
library(mapinsetr)
library(rmapshaper)
library(ggplot2)
library(readxl)

# library(remotes)
# install_git(url = "https://gitlab.huma-num.fr/atlas-social-de-la-france/asf", build_vignettes = TRUE)
library(asf)
vignette("asf")
vignette("mapsf")


example <- get_example()

fond <- example$fond_ex
tabl <- example$tabl_ex
data <- example$data_ex

# ################################################################################ 1. FOND
# create_fond(input_path = "test/input/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/",
#             output_path = "test/output",
#             year = "2020")
# 
# ################################################################################ 2. TABLE
# create_tabl(tabl_file = "test/input/table_2020.xlsx",
#             popu_file = "test/input/TD_POP1A_2020.csv",
#             iris_file = "test/output/fond_2020.gpkg",
#             output_path = "test/output",
#             year = "2020",
#             arr = TRUE)
# 
# ################################################################################ 3. IMPORT
# fond <- st_read("test/output/fond_2020.gpkg")
# tabl <- read.csv("test/output/tabl_2020.csv")
# data <- read.csv("test/output/data_csp_2020.csv")
# 
# ################################################################################
# data <- aggreg_data(tabl = tabl,
#                     data = data,
#                     vars = list(tot = "C20_POP15P",
#                                 va1 = "C20_POP15P_CS1",
#                                 va2 = "C20_POP15P_CS2",
#                                 va3 = "C20_POP15P_CS3",
#                                 va4 = "C20_POP15P_CS4",
#                                 va5 = "C20_POP15P_CS5",
#                                 va6 = "C20_POP15P_CS6",
#                                 va7 = "C20_POP15P_CS7",
#                                 va8 = "C20_POP15P_CS8"),
#                     funs = c("sum", "sum", "sum", "sum", "sum", "sum", "sum", "sum", "sum"),
#                     id = c("CODE_IRIS", "IRIS"),
#                     maille = "ce")
# 
# ################################################################################
# fond <- aggreg_fond(fond = fond,
#                     tabl = tabl,
#                     id = c("CODE_IRIS", "CODE_IRIS"),
#                     maille = "ce")
# 
# ################################################################################
# zoom <- create_zoom(fond = fond,
#                     villes = c("Aix-en-Provence","Ajaccio","Brest","Caen",
#                                "Dijon","Lyon","Strasbourg","Toulon"),
#                     buffer = 10000)
# 
# zooms <- zoom$zooms
# labels <- zoom$labels
# 
# ################################################################################
# fond <- simplify_geom(fond, keep = 0.2)
# 
# fondata <- merge_fondata(fond = fond,
#                          zoom = zooms,
#                          data = data,
#                          id = c("ce", "ce"))
# 
# st_write(fondata, "test_ce.gpkg")
# 
# ################################################################################ 4. CARTE
# 
# map_bi(fondata, "tot", "va6", "va3",
#        zoomlabel = labels,
#        palette = "rouver")
# 
# map_ql(fondata, tot = "tot", var = "va6",
#        zoomlabel = labels,
#        palette = "rou")
# 
# map_q(fondata, tot = "tot", var = "va6",
#       zoomlabel = labels,
#       palette = "rou")





# ################################################################################ TEST LOGEMENTS
# lv <- read_xlsx("DATA/insee_rp_hist_1968_lv.xlsx", skip = 4, sheet = "Data")
# rs <- read_xlsx("DATA/insee_rp_hist_1968_rs.xlsx", skip = 4, sheet = "Data")
# 
# data <- st_read("DATA/filosofi_2019.csv", quiet = TRUE)
# 
# lv2020 <- lv[lv$an == "2020", ]
# rs2020 <- rs[rs$an == "2020", ]
# log <- merge(lv2020, rs2020, by = "codgeo")
# colnames(log)[colnames(log) == "codgeo"] <- "COM"
# log <- log[, c(1,4,7)]
# 
# data <- data[, c(1,15)]
# colnames(data)[colnames(data) == "CODE_COM"] <- "COM"
# 
# test <- merge(data, log, by = "COM")
# 
# colnames(test)[colnames(test) == "p_rsecocc"] <- "ressec"
# test$logvac <- as.numeric(test$part_logt_vacant) *100 / as.numeric(test$log)
# 
# data <- test[, c(1,2,4,5)]
# 
# ################################################################################
# fond <- st_read("test/output/fond_2020.gpkg")
# tabl <- read.csv("test/output/tabl_2020.csv")
# 
# data <- aggreg_data(tabl = tabl,
#                     data = data,
#                     vars = c("log",
#                              "ressec",
#                              "logvac"),
#                     funs = c("sum", "sum", "sum"),
#                     id = c("INSEE_COM", "COM"),
#                     maille = "ice")
# 
# ################################################################################
# fond <- aggreg_fond(tabl = tabl,
#                     fond = fond,
#                     id = c("CODE_IRIS", "CODE_IRIS"),
#                     maille = "ice")
# 
# ################################################################################
# zoom <- create_zoom(fond = fond,
#                     # villes = c("Paris", "Lyon", "Marseille", "Toulouse", "Avignon"),
#                     lon = c(2.692),
#                     lat = c(47.648),
#                     buffer = 10000)
# 
# zooms <- zoom$zooms
# labels <- zoom$labels
# 
# ################################################################################
# fond <- simplify_geom(fond, keep = 0.2)
# 
# fondata <- merge_fondata(data, fond, zooms, id = c("ice", "ice"))
# 
# map_q(fondata, "log", "ressec",
#       zoomlabel = labels,
#       palette = "rou")
# 
# map_bi(fondata, "log", "ressec", "logvac",
#        zoomlabel = labels,
#        palette = "rouver")
# 
# 
# st_write(fondata, "test.gpkg")





################################################################################ TEST AAV
fond <- st_read("test/output/fond_2020.gpkg")
tabl <- read.csv("test/output/tabl_2020.csv")
data <- read.csv("test/output/data_csp_2020.csv")

# Import des informations sur la catégorie aav de chaque commune 
aav <- st_read("test/input/aav_2023.gpkg")
aav <- aav[, c("codgeo", "cateaav", "taav")]
aav$geom <- NULL

id <- "INSEE_COM"

# Jointure de ces informations avec le fond de carte créé plus haut
x <- merge(fond, aav, 
           by.x = id, by.y = "codgeo",
           all.x = TRUE)

# vecteur des arr de Paris
arrPari <- x[[id]][grepl("^75", x[[id]]) & is.na(x$cateaav)]
# vecteur des arr de Marseille
arrMars <- x[[id]][grepl("^13", x[[id]]) & is.na(x$cateaav)]
# vecteur des arr de Lyon
arrLyon <- x[[id]][grepl("^69", x[[id]]) & is.na(x$cateaav)]

# Remplacer les NA des arrondissements (Paris, Lyon, Marseille) par des Commune-Centre
x$cateaav[is.na(x$cateaav)] <- "Commune-Centre"

# Remplacer les NA des arrondissements (Lyon et Marseille) par des + 700 000
x$taav <- ifelse(
  (x[[id]] %in% arrMars | x[[id]] %in% arrLyon) & is.na(x$taav),
  "Aire de 700 000 habitants ou plus (hors Paris)",
  x$taav
)
# Remplacer les NA des arrondissements (Paris) par Aire de Paris
x$taav <- ifelse(
  (x[[id]] %in% arrPari) & is.na(x$taav),
  "Aire de Paris",
  x$taav
)
# Remplacer les NA restant par une nouvelle catégorie
x$taav[is.na(x$taav)] <- "Hors aire"

x$geom <- NULL

tmp <- unique(x$cateaav)

# Ajout des données
x <- merge(x, data, by.x = "CODE_IRIS", by.y = "IRIS")

################################################################################ 
w <- plot_glis(data = x, vars = c(13:20), tot = "C20_POP15P")

plot_typo(data = x,
          vars = c(13:20), 
          typo = "cateaav",
          order = c(3,4,5,1,2),
          colors = c("#8fbc9e","#f5a26c","#908dc4","#fed17e","#8ab5e1","#6984bc","#f192b0","#e56a6a"))

plot_typo(data = x, 
          vars = c(13:20), 
          typo = "taav",
          order = c(6,3,5,4,1,2), 
          colors = c("#8fbc9e","#f5a26c","#908dc4","#fed17e","#8ab5e1","#6984bc","#f192b0","#e56a6a"))

plot_vars(data = x,
          vars = c(13:20), 
          typo = "cateaav",
          order = c(3,4,5,1,2))

plot_vars(data = x,
          vars = c(13:20), 
          typo = "taav",
          order = c(6,3,5,4,1,2))
