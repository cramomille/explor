
#                                           ACCESSIBILITE POTENTIELLE LOCALISEE
#                                                         ET MAILLAGE COMPOSITE
#
#                                                                antoine beroud
#                                                                  juillet 2025

library(sf)
library(asf)
library(mapsf)
library(readxl)


###############################################################################
#################################################################### GEOMETRIES
# Donnees sur le maillage des communes regroupees
mar <- asf_mar(maille = "comr")

tabl <- mar$tabl
geom <- mar$geom

# Agregation des iris en communes regroupees
fond <- asf_fond(f = geom, 
                 t = tabl, 
                 by = "COMF_CODE", 
                 maille = "COMR_CODE", 
                 keep = "DEP")

# Repositionnement des DROM
fond <- asf_drom(f = fond, 
                 id = "COMR_CODE")

# Creation de zooms
z <- asf_zoom(f = fond, 
              places = c("4", "5"), 
              r = 20000)

zoom <- z$zooms
label <- z$labels

# Recuperation des limites departementales
dep <- asf_borders(f = fond, by = "DEP", keep = 0.01)


###############################################################################
####################################################################### DONNEES
# Donnees d'accessibilite potentielle localisee
fichier <- "input/Indicateur d'accessibilite potentielle localisee (APL) aux medecins generalistes.xlsx"
fichier <- "input/Indicateur d'accessibilité potentielle localisée (APL) aux sages-femmes.xlsx"

apl_2022 <- read_xlsx(fichier, sheet = "APL 2022", skip = 7)
apl_2023 <- read_xlsx(fichier, sheet = "APL 2023", skip = 7)

apl_2022 <- apl_2022[-1, ]
apl_2023 <- apl_2023[-1, ]

apl_2022[-c(1, 2)] <- lapply(apl_2022[-c(1, 2)], as.numeric)
apl_2023[-c(1, 2)] <- lapply(apl_2023[-c(1, 2)], as.numeric)

data <- apl_2023

# Agregation des donnees en communes regroupees
data <- asf_data(d = data, 
                 t = tabl, 
                 by.x = "Code commune INSEE",
                 by.y = "COM_CODE", 
                 maille = "COMR_CODE", 
                 keep = "COMR_LIB", 
                 vars = c(3, 5), 
                 funs = c("median", "sum"))


###############################################################################
###################################################################### JOINTURE
# Jointure entre les donnees, le fond et les zooms
fondata <- asf_fondata(f = fond,
                       z = zoom,
                       d = data, 
                       by = "COMR_CODE")


###############################################################################
######################################################################## CARTES
# Definition de la variable d'interet
varname <- "APL aux médecins généralistes"
varname <- "APL aux sages-femmes"

# Palette
pal <- asf_palette(pal = "peche", nb = 6)

# Seuils
q6 <- quantile(fondata[[varname]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Carte choroplethe
mf_map(fondata,
       var = varname,
       type = "choro",
       breaks = q6,
       pal = pal,
       border = NA)

# Contours departements
mf_map(dep,
       col = "white",
       lwd = 1,
       add = TRUE)

# Labels des zooms
mf_label(label,
         var = "label",
         col = "#000000",
         font = 1)




























# Donnees sur le maillage des communes
mar <- asf_mar(maille = "comf")

tabl <- mar$tabl
geom <- mar$geom

fond <- asf_fond(f = geom, 
                 t = tabl, 
                 by = "COMF_CODE", 
                 maille = "COMF_CODE", 
                 keep = "DEP")

fond <- asf_drom(f = fond, 
                 id = "COMF_CODE")

z <- asf_zoom(f = fond, 
              places = c("4", "5"), 
              r = 20000)

zoom <- z$zooms
label <- z$labels


data <- apl_2023

data <- asf_data(d = data, 
                 t = tabl, 
                 by.x = "Code commune INSEE",
                 by.y = "COM_CODE", 
                 maille = "COMF_CODE", 
                 keep = "COMF_LIB", 
                 vars = c(3, 5), 
                 funs = c("median", "sum"))

fondata <- asf_fondata(f = fond,
                       z = zoom,
                       d = data, 
                       by = "COMF_CODE")



# Definition de la variable d'interet
varname <- "APL aux médecins généralistes"
varname <- "APL aux sages-femmes"

# Palette
pal <- asf_palette(pal = "peche", nb = 6)

# Seuils
q6 <- quantile(fondata[[varname]], probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Carte choroplethe
mf_map(fondata, 
       var = varname, 
       type = "choro", 
       breaks = q6, 
       pal = pal, 
       border = NA)

# Contours departements
mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)

# Labels des zooms
mf_label(label, 
         var = "label", 
         col = "#000000", 
         font = 1)




asf_plot_typo(d = data, vars = )
