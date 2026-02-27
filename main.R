
#                                                        EXPLORATIONS ET TEST R
#
#                                                                antoine beroud
#                                                                  janvier 2025

invisible(sapply(list.files("script/function", 
                            pattern = "\\.R$", 
                            full.names = TRUE), 
                 source))

library(sf)
library(asf)
library(mapsf)



# CREATION DE FICHIERS .PARQUET -----------------------------------------------
# Creation de fichiers de donnees test
set.seed(1312)
n <- 1e7

df <- data.frame(
  id   = sprintf("%07d", 1:n),
  iris = sprintf("%04d", 1:n),
  com  = sprintf("%05d", 1:n),
  var1 = sample(10:100000, n, replace = TRUE),
  var2 = sample(10:100000, n, replace = TRUE),
  var3 = sample(c("A", "B", "C"), n, replace = TRUE),
  var4 = sample(c("X", "Y", "Z"), n, replace = TRUE)
)

DF <- data.frame(
  ID   = sprintf("%07d", 1:n),
  IRIS = sprintf("%04d", 1:n),
  COM  = sprintf("%05d", 1:n),
  VAR1 = sample(10:100000, n, replace = TRUE),
  VAR2 = sample(10:100000, n, replace = TRUE),
  VAR3 = sample(c("A", "B", "C"), n, replace = TRUE),
  VAR4 = sample(c("X", "Y", "Z"), n, replace = TRUE)
)

# Conversion en fichier.sas7bdat
write_sas(df, "output/sas/data1.sas7bdat")
write_sas(DF, "output/sas/data2.sas7bdat")

# Transformation de fichiers .sas7bdat en .parquet
parquet_convert(sas = c("output/sas/data1.sas7bdat",
                        "output/sas/data2.sas7bdat"),
                parquet = "output/parquet/",
                chunk = 1000000)



# OUVERTURE DE FICHIERS .PARQUET ----------------------------------------------
# Recuperation des noms d'un fichier parquet
parquet_colname(dir = "output/parquet/data1")
parquet_colname(dir = "output/parquet/data2")

# Ouverture d'un fichier .parquet a partir du dossier contenant les chunks qui 
# le composent 
result <- parquet_open(dir = "output/parquet/",
                       file = "data1",
                       cols = c(1, 4:5))

data1 <- result[[1]]
write_parquet(data1, "output/parquet/data3.parquet") # pour les exemples suivants
parquet_colname("output/parquet/data3.parquet")

# Ouverture d'un fichier .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = "data3.parquet",
                       cols = c(1:3))

data3 <- result[[1]]


# Ouverture de plusieurs fichiers .parquet (avec des colonnes equivalentes mais 
# nommees differemment dans les differents fichiers)
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
                       cols = list(c("id", "ID"),
                                   c("var1", "VAR1"),
                                   c("var2", "VAR2")))
 
data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]



# SECRET STATISTIQUE ----------------------------------------------------------
x <- data.frame(
  com = c("01", "02", "03", "04", "05", "06", "07"),
  tot = c(90, 20, 60, 90, 20, 60, 20),
  ca1 = c(20,  0, 20, 20,  5,  0,  0),
  ca2 = c(20,  0, 10, 30,  5, 20,  0),
  ca3 = c(30,  0, 10, 30,  5, 20,  0),
  ca4 = c(20, 20, 20, 10,  5, 20, 20)
)

y <- data.frame(
  com = c("01", "02", "03", "04", "05", "06", "07"),
  tot = c(90, 20, 60, 90, 20, 60, 20),
  ca1 = c(20,  0, 20, NA, NA,  0,  0),
  ca2 = c(20,  0, NA, 30, NA, 20,  0),
  ca3 = c(30,  0, NA, 30, NA, 20,  0),
  ca4 = c(20, 20, 20, NA, NA, 20, 20)
)

result <- secret_data(
  d = x, 
  vars = c(3:6), 
  limit = 11, 
  unique = FALSE
)



# TABLEAU CROISE --------------------------------------------------------------
x <- data.frame(
  csp  = sample(c("agriculteurice", "ouvriere", "cadre"), 20, replace = TRUE),
  sexe = sample(c("homme", "femme"), 20, replace = TRUE),
  poids = sample(1:5, 20, replace = TRUE)
)

result <- create_xtab(x, "csp", "sexe", "poids")
result <- create_xtab(x, "csp", "sexe")

# Selection des colonnes numeriques
num_cols <- colnames(result)[-1]

# % lignes
tab_pct_row <- result
tab_pct_row[num_cols] <- t(apply(result[num_cols], 1, function(x) round(100 * x / sum(x), 2)))

# % colonnes
tab_pct_col <- result
tab_pct_col[num_cols] <- apply(result[num_cols], 2, function(x) round(100 * x / sum(x), 2))



# # MATRICE DE FLUX TEST ----
# df <- data.frame(
#   id       = sprintf("%07d", 1:20),
#   csp      = sample(c("51", "52", "53", "64", "65", "66"), 20, replace = TRUE),
#   genre    = sample(c("1", "2", "3"), 20, replace = TRUE),
#   com_resi = sample(c("A", "B", "C"), 20, replace = TRUE),
#   com_trav = sample(c("A", "B", "C"), 20, replace = TRUE),
#   pond     = sample(c(0, 0.5, 1), 20, replace = TRUE),
# )
# 
# tab_flux <- function(df,
#                      com_code,
#                      csp_code,
#                      sexe_col,
#                      pond_col,
#                      filt_col,   # colonne pour filtrer
#                      flux_col    # colonne pour l’agregation
#                      ) {
#   
#   # Creation d'une liste vide de la longueur du nombre de CSP
#   res <- vector("list", length(csp))
#   names(res) <- paste0("csp_", csp)
#   
#   for (i in seq_along(csp)) {
#     # Filtrage des individus correspondant a la CSP et a la commune
#     d <- df$csp == csp[i] &
#       df[[com_filtre]] == com
#     
#     df_i <- df[d, ]
#     
#     # S'il n'y a aucun individu apres le filtrage : NULL
#     if (nrow(df_i) == 0) {
#       res[[i]] <- NULL
#       next
#     }
#     
#     # Agregation : somme des ponderations par commune de flux et sexe
#     agg <- aggregate(
#       pond ~ get(com_flux) + sexe,
#       data = df_i,
#       FUN = sum
#     )
#     
#     # Renommage de la premiere colonne avec le nom du flux
#     names(agg)[1] <- com_flux
#     
#     # Transformation en format large
#     wide <- reshape(
#       agg,
#       idvar     = com_flux,
#       timevar   = "sexe",
#       direction = "wide"
#     )
#     
#     # Renommage des colonnes
#     names(wide) <- sub("pond\\.", "nb_", names(wide))
#     
#     # Recuperation du nom des colonnes sexe
#     cols_sexe <- grep("^nb_", names(wide), value = TRUE)
#     
#     # Ajout d'une colonne TOT = somme hommes + femmes
#     wide$nb_trav <- rowSums(
#       wide[, cols_sexe, drop = FALSE],
#       na.rm = TRUE
#     )
#     
#     # Remplacement des NA par des 0
#     wide[, c("nb_trav", cols_sexe)][is.na(wide[, c("nb_trav", cols_sexe)])] <- 0
#     
#     # Ajout d'une colonne avec pour modalite la commune filtre
#     wide[[com_filtre]] <- com
#     
#     # Reorganisation des colonnes
#     res[[i]] <- wide[, c(com_filtre, com_flux, "nb_trav", cols_sexe)]
#   }
#   
#   return(res)
# }
# 
# 
# 
# export_tabs_csv <- function(tabs, 
#                             dir_out = "output") {
#   
#   # Creation du dossier s'il n'existe pas
#   if (!dir.exists(dir_out)) dir.create(dir_out)
#   
#   lapply(names(tabs), function(nm) {
#     
#     tab <- tabs[[nm]]
#     if (is.null(tab)) return(NULL)
#     
#     # Nommage dynamique des colonnes
#     com_filtre_name <- names(tab)[1]   # commune filtree
#     com_flux_name   <- names(tab)[2]   # flux
#     
#     # Nommage du fichier .csv
#     file_out <- file.path(
#       dir_out,
#       paste0(
#         com_filtre_name, "_",            # nom colonne filtree
#         tab[[1, com_filtre_name]], "_",  # valeur commune filtree
#         com_flux_name, "_",              # nom colonne flux
#         nm, ".csv"
#       )
#     )
#     
#     write.csv(tab, file = file_out, row.names = FALSE)
#   })
# }
# 
# 
# 
# 
# 
# tabs <- tab_flux(
#   df = df,
#   com = "A",
#   csp = c(51, 54, 62, 67),
#   com_filtre = "res_com",
#   com_flux = "trav_com"
# )
# 
# tabs[[2]]
# 
# export_tabs_csv(tabs, dir_out = "output")