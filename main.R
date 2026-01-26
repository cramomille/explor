
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
n <- 1e6

df <- data.frame(
  id = sprintf("%07d", 1:n),
  value1 = sample(10:100000, n, replace = TRUE),
  value2 = sample(10:100000, n, replace = TRUE)
)

DF <- data.frame(
  ID = sprintf("%07d", 1:n),
  VALUE1 = sample(10:100000, n, replace = TRUE),
  VALUE2 = sample(10:100000, n, replace = TRUE)
)

# Conversion en fichier.sas7bdat
write_sas(df, "output/sas/data1.sas7bdat")
write_sas(DF, "output/sas/data2.sas7bdat")

# Transformation de fichiers .sas7bdat en .parquet
parquet_convert(sas = c("output/sas/data1.sas7bdat",
                        "output/sas/data2.sas7bdat"),
                parquet = "output/parquet/",
                chunk = 100000)


# OUVERTURE DE FICHIERS .PARQUET ----------------------------------------------
# Recuperation des noms d'un fichier parquet
names <- parquet_colname(dir = "output/parquet/data1")
names <- parquet_colname(dir = "output/parquet/data2")
names <- parquet_colname(dir = "output/parquet/data3.parquet")

# Ouverture d'un fichier .parquet
result <- parquet_open(dir = "output/parquet/",
                       file = "data3.parquet",
                       cols = c(1:3))

data3 <- result[[1]]

# Ouverture d'un fichier .parquet a partir du dossier contenant les chunks qui 
# le composent 
result <- parquet_open(dir = "output/parquet/",
                       file = "data1",
                       cols = c(1:3))

data1 <- result[[1]]

# Ouverture de plusieurs fichiers .parquet (avec des colonnes equivalentes mais 
# nommees differemment dans les differents fichiers)
result <- parquet_open(dir = "output/parquet/",
                       file = c("data1", "data2", "data3.parquet"),
                       cols = list(c("id", "ID"),
                                   c("value1", "VALUE1"),
                                   c("value2", "VALUE2")))
 
data1 <- result[[1]]
data2 <- result[[2]]
data3 <- result[[3]]


# SECRET STATISTIQUE ----------------------------------------------------------
# Creation d'un data.frame d'exemple
x <- data.frame(
  commune = c("com1", "com2", "com3", "com4", "com5"),
  tot = c(100, 100, 50, 60, 20),
  ca1 = c(20, 40, 10, 0, 0),
  ca2 = c(10, 10, 10, 20, 0),
  ca3 = c(30, 10, 10, 20, 0),
  ca4 = c(40, 40, 10, 20, 20)
)

y <- dput(x) # test de cette fonction

result <- secret_data(x, vars = c(3:6), limit = 11, unique = FALSE)


# TABLEAU CROISE --------------------------------------------------------------
# Creation d'un data.frame d'exemple
set.seed(1312)

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















# TABLE -----------------------------------------------------------------------

df <- data.frame(
  id       = 1:20,
  csp      = c(51,51,54,54,62,62,67,67,51,54,
               62,67,51,54,62,67,51,54,62,67),
  sexe     = c("H","F","H","F","H","F","H","F","H","F",
               "F","H","F","H","H","F","H","F","F","H"),
  res_com  = c("A","A","A","B","B","B","C","C","A","B",
               "C","A","B","C","A","B","C","A","B","C"),
  trav_com = c("B","C","B","A","C","A","A","B","C","A",
               "A","B","B","C","C","A","A","B","C","B"),
  pond     = c(1,1.2,0.8,1,1.5,1,0.9,1.1,1,0.7,
               1.3,1,0.8,1.2,1,1.1,0.9,1,1.4,1)
)





tab_flux <- function(df,
                     com_code,
                     csp_code,
                     sexe_col,
                     pond_col,
                     filt_col,   # colonne pour filtrer
                     flux_col    # colonne pour l’agregation
) {
  
  # Creation d'une liste vide de la longueur du nombre de CSP
  res <- vector("list", length(csp))
  names(res) <- paste0("csp_", csp)
  
  for (i in seq_along(csp)) {
    # Filtrage des individus correspondant a la CSP et a la commune
    d <- df$csp == csp[i] &
      df[[com_filtre]] == com
    
    df_i <- df[d, ]
    
    # S'il n'y a aucun individu apres le filtrage : NULL
    if (nrow(df_i) == 0) {
      res[[i]] <- NULL
      next
    }
    
    # Agregation : somme des ponderations par commune de flux et sexe
    agg <- aggregate(
      pond ~ get(com_flux) + sexe,
      data = df_i,
      FUN = sum
    )
    
    # Renommage de la premiere colonne avec le nom du flux
    names(agg)[1] <- com_flux
    
    # Transformation en format large
    wide <- reshape(
      agg,
      idvar     = com_flux,
      timevar   = "sexe",
      direction = "wide"
    )
    
    # Renommage des colonnes
    names(wide) <- sub("pond\\.", "nb_", names(wide))
    
    # Recuperation du nom des colonnes sexe
    cols_sexe <- grep("^nb_", names(wide), value = TRUE)
    
    # Ajout d'une colonne TOT = somme hommes + femmes
    wide$nb_trav <- rowSums(
      wide[, cols_sexe, drop = FALSE],
      na.rm = TRUE
    )
    
    # Remplacement des NA par des 0
    wide[, c("nb_trav", cols_sexe)][is.na(wide[, c("nb_trav", cols_sexe)])] <- 0
    
    # Ajout d'une colonne avec pour modalite la commune filtre
    wide[[com_filtre]] <- com
    
    # Reorganisation des colonnes
    res[[i]] <- wide[, c(com_filtre, com_flux, "nb_trav", cols_sexe)]
  }
  
  return(res)
}



export_tabs_csv <- function(tabs, 
                            dir_out = "output") {
  
  # Creation du dossier s'il n'existe pas
  if (!dir.exists(dir_out)) dir.create(dir_out)
  
  lapply(names(tabs), function(nm) {
    
    tab <- tabs[[nm]]
    if (is.null(tab)) return(NULL)
    
    # Nommage dynamique des colonnes
    com_filtre_name <- names(tab)[1]   # commune filtree
    com_flux_name   <- names(tab)[2]   # flux
    
    # Nommage du fichier .csv
    file_out <- file.path(
      dir_out,
      paste0(
        com_filtre_name, "_",            # nom colonne filtree
        tab[[1, com_filtre_name]], "_",  # valeur commune filtree
        com_flux_name, "_",              # nom colonne flux
        nm, ".csv"
      )
    )
    
    write.csv(tab, file = file_out, row.names = FALSE)
  })
}





tabs <- tab_flux(
  df = df,
  com = "A",
  csp = c(51, 54, 62, 67),
  com_filtre = "res_com",
  com_flux = "trav_com"
)

tabs[[2]]

export_tabs_csv(tabs, dir_out = "output")






# TABLEAU ---------

data <- read.csv("input/csp_2020.csv")

data <- data[, c(1, 2, 5, 8, 11)]

mean_cs3 <- sum(data$C20_POP15P_CS3) / sum(data$C20_POP15P) * 100
mean_cs6 <- sum(data$C20_POP15P_CS6) / sum(data$C20_POP15P) * 100

data$ql_cs3 <- (data$C20_POP15P_CS3 / data$C20_POP15P * 100) / mean_cs3
data$ql_cs6 <- (data$C20_POP15P_CS6 / data$C20_POP15P * 100) / mean_cs6


r  <- cor(data$ql_cs3, data$ql_cs6, use = "complete.obs")
r2 <- r^2


a <- cov(data$ql_cs3, data$ql_cs6, use = "complete.obs") / var(data$ql_cs3, na.rm = TRUE)

b <- mean(data$ql_cs6, na.rm = TRUE) - a * mean(data$ql_cs3, na.rm = TRUE)

data$cs3_bis <- a * data$ql_cs3 + b 

mod <- lm(ql_cs3 ~ ql_cs6, data = data, na.action = na.exclude)

data$residus <- resid(mod)







data <- read.csv("input/csp_2020.csv")

data <- data[, c(1, 2, 5, 8, 11)]

data$pct_cs3 <- data$C20_POP15P_CS3 / data$C20_POP15P * 100
data$pct_cs6 <- data$C20_POP15P_CS6 / data$C20_POP15P * 100


r  <- cor(data$pct_cs3, data$pct_cs6, use = "complete.obs")
r2 <- r^2


a <- cov(data$pct_cs3, data$pct_cs6, use = "complete.obs") / var(data$pct_cs3, na.rm = TRUE)
b <- mean(data$pct_cs6, na.rm = TRUE) - (a * mean(data$pct_cs3, na.rm = TRUE))

data$cs6_bis <- a * data$pct_cs3 + b 
 







data <- read.csv("input/csp_2020.csv")

data <- data[, c(1, 2, 5, 8, 11)]

data$pct_cs3 <- data$C20_POP15P_CS3 / data$C20_POP15P * 100
data$pct_cs6 <- data$C20_POP15P_CS6 / data$C20_POP15P * 100


data$pct_cs3_em <- data$pct_cs3 - mean(data$pct_cs3, na.rm = TRUE)
data$pct_cs6_em <- data$pct_cs6 - mean(data$pct_cs6, na.rm = TRUE)

data$cov <- data$pct_cs3_em * data$pct_cs6_em

mean_cov <- mean(data$cov, na.rm = TRUE)

var_cs3 <- mean(data$pct_cs3_em^2, na.rm = TRUE)


a <- mean_cov / var_cs3
b <- mean(data$pct_cs6, na.rm = TRUE) - (a * mean(data$pct_cs3, na.rm = TRUE))

data$cs6_bis <- a * data$pct_cs3 + b 
















mod <- lm(ql_cs3 ~ ql_cs6, data = data, na.action = na.exclude)

data$residus <- resid(mod)






coef(mod)

cor.test(data$ql_cs3, data$ql_cs6)

summary(mod)$fstatistic

summary(mod)$coefficients







