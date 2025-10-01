






load("input/mar_metadata/donnees/AR03_maille_IRISr5.RData")
























###############################################################################
library(asf)

.add_mayotte <- function(df, f_code, f_lib, r_code, r_lib) {
  myt <- df[df$OM_CODE == "MYT", ]
  myt[[r_code]] <- myt[[f_code]]
  names(myt)[which(names(myt) == f_lib)] <- r_lib
  return(myt)
}


cols <- c("OM_CODE", "EPCI", "NATURE_EPCI", "ARR", "CV", 
          "UU2020", "TUU2017", "TDUU2017", "BV2022", "ZE2020", 
          "AAV2020", "TAAV2017", "TDAAV2017", "CATEAAV2020", 
          "DEP", "REG")

dir <- "input/mar"

d.irisr.pass <- read.csv(file.path(dir, "d.irisr.pass.csv"))
d.irisr.app  <- read.csv(file.path(dir, "d.irisr.app.csv"))
d.irisf.pass <- read.csv(file.path(dir, "d.irisf.pass.csv"))
d.comf.pass  <- read.csv(file.path(dir, "d.comf.pass.csv"))
d.comf.app   <- read.csv(file.path(dir, "d.comf.app.csv"))


# COM_F -----------------------------------------------------------------------
d.comf.app   <- read.csv(file.path(dir, "d.comf.app.csv"))
d.comf.pass  <- read.csv(file.path(dir, "d.comf.pass.csv"))

comf <- merge(d.comf.pass[, -c(2, 5)], d.comf.app, by = "COMF_CODE", all.x = TRUE)
names(comf)[5] <- "COM_TYPE"
comf <- comf[, c("COM_CODE", "COM_TYPE", 
                 "COMF_CODE", "COMF_LIB", 
                 cols)]
comf[] <- lapply(comf, as.character)
  
tabl <- comf

data_x <- read.csv("input/csp_2020.csv")

data_y <- asf_data(d = data_x, 
                   t = tabl, 
                   by.x = "COM", 
                   by.y = "COM_CODE", 
                   maille = "COMF_CODE", 
                   vars = c(4:13), 
                   funs = "sum")

sum(data_x$C20_POP15P_CS1, na.rm = TRUE) - sum(data_y$C20_POP15P_CS1, na.rm = TRUE)

# IRIS de data_x non presents dans tabl
missing <- setdiff(data_x$COM, tabl$COM_CODE)

# Voir combien il y en a
length(missing)


# COM_R -----------------------------------------------------------------------
d.irisr.app  <- read.csv(file.path(dir, "d.irisr.app.csv"))
d.comf.pass  <- read.csv(file.path(dir, "d.comf.pass.csv"))

comf <- comf

id_list <- strsplit(d.irisr.app$COMF_CODE_MULTI, " \\| ")
id_tabl <- data.frame(
  COMF_CODE = unlist(id_list),
  COMR_CODE = rep(d.irisr.app$COMF_CODE_MULTI, sapply(id_list, length))
)
id_tabl <- id_tabl[!duplicated(id_tabl$COMF_CODE), ]

comr <- merge(d.comf.pass, id_tabl, by = "COMF_CODE", all.x = TRUE)
d.irisr.app <- d.irisr.app[, c(5:6, 10:25)]
d.irisr.app <- d.irisr.app[!duplicated(d.irisr.app$COMF_CODE_MULTI), ]

comr <- merge(comr, d.irisr.app, by.x = "COMR_CODE", by.y = "COMF_CODE_MULTI", all.x = TRUE)
names(comr)[4] <- "COM_TYPE"
names(comr)[6] <- "COMF_LIB"
names(comr)[7] <- "COMR_LIB"

comr <- comr[, c("COM_CODE", "COM_TYPE", 
                   "COMF_CODE", 
                   "COMR_CODE", "COMR_LIB", 
                   cols)]

myt <- .add_mayotte(comf, "COMF_CODE", "COMF_LIB", "COMR_CODE", "COMR_LIB")
myt <- myt[, colnames(comr)]
comr <- rbind(comr[!grepl("^976", comr$COM_CODE), ], myt)

comr[] <- lapply(comr, as.character)

tabl <- comr

data_x <- read.csv("input/csp_2020.csv")

data_y <- asf_data(d = data_x, 
                   t = tabl, 
                   by.x = "COM", 
                   by.y = "COM_CODE", 
                   maille = "COMR_CODE", 
                   vars = c(4:13), 
                   funs = "sum")

sum(data_x$C20_POP15P_CS1, na.rm = TRUE) - sum(data_y$C20_POP15P_CS1, na.rm = TRUE)

# COM de data_x non presentes dans tabl
missing <- setdiff(data_x$COM, tabl$COM_CODE)

# Voir combien il y en a
length(missing)


# IRIS_F ----------------------------------------------------------------------
d.irisf.pass <- read.csv(file.path(dir, "d.irisf.pass.csv"))
d.comf.app   <- read.csv(file.path(dir, "d.comf.app.csv"))

irisf <- merge(d.irisf.pass, d.comf.app, by = "COMF_CODE")
irisf <- irisf[, c("IRIS_CODE",
                   "IRISF_CODE", "IRISF_LIB", 
                   cols)]
irisf[] <- lapply(irisf, as.character)

tabl <- irisf

data_x <- read.csv("input/csp_2020.csv")

data_y <- asf_data(d = data_x, 
                   t = tabl, 
                   by.x = "IRIS", 
                   by.y = "IRIS_CODE", 
                   maille = "IRISF_CODE", 
                   vars = c(4:13), 
                   funs = "sum")

sum(data_x$C20_POP15P_CS1, na.rm = TRUE) - sum(data_y$C20_POP15P_CS1, na.rm = TRUE)

# IRIS de data_x non presents dans tabl
missing <- setdiff(data_x$IRIS, tabl$IRIS_CODE)

# Voir combien il y en a
length(missing)


# IRIS_RS ---------------------------------------------------------------------
d.irisr.app  <- read.csv(file.path(dir, "d.irisr.app.csv"))
d.irisr.pass <- read.csv(file.path(dir, "d.irisr.pass.csv"))

irisf <- irisf

irisrs <- merge(d.irisr.pass, d.irisr.app[, c(1, 5, 6, 10:25)], by = "IRISrD_CODE")
irisrs <- irisrs[, c("IRIS_CODE", 
                     "IRISF_CODE", 
                     "IRISrS_CODE", "IRISrS_LIB", 
                     cols)]

myt <- .add_mayotte(irisf, "IRISF_CODE", "IRISF_LIB", "IRISrS_CODE", "IRISrS_LIB")
myt <- myt[, colnames(irisrs)]
irisrs <- rbind(irisrs, myt)

irisrs[] <- lapply(irisrs, as.character)

tabl <- irisrs

data_x <- read.csv("input/csp_2020.csv")

data_y <- asf_data(d = data_x, 
                   t = tabl, 
                   by.x = "IRIS", 
                   by.y = "IRIS_CODE", 
                   maille = "IRISrS_CODE", 
                   vars = c(4:13), 
                   funs = "sum")

sum(data_x$C20_POP15P_CS1, na.rm = TRUE) - sum(data_y$C20_POP15P_CS1, na.rm = TRUE)

# IRIS de data_x non presents dans tabl
missing <- setdiff(data_x$IRIS, tabl$IRIS_CODE)

# Voir combien il y en a
length(missing)

# Lignes completes de data_x correspondant aux IRIS manquants
data_x_missing <- data_x[data_x$IRIS %in% missing, ]


# IRIS_RD ---------------------------------------------------------------------
d.irisr.app  <- read.csv(file.path(dir, "d.irisr.app.csv"))
d.irisr.pass <- read.csv(file.path(dir, "d.irisr.pass.csv"))

irisf <- irisf

irisrd <- merge(d.irisr.pass, d.irisr.app[, c(1, 5, 6, 10:25)], by = "IRISrD_CODE")
irisrd <- irisrd[, c("IRIS_CODE", 
                     "IRISF_CODE", 
                     "IRISrD_CODE", "IRISrD_LIB", 
                     cols)]

myt <- .add_mayotte(irisf, "IRISF_CODE", "IRISF_LIB", "IRISrD_CODE", "IRISrD_LIB")
myt <- myt[, colnames(irisrd)]
irisrd <- rbind(irisrd, myt)

irisrd[] <- lapply(irisrd, as.character)


tabl <- irisrd

data_x <- read.csv("input/csp_2020.csv")

data_y <- asf_data(d = data_x, 
                   t = tabl, 
                   by.x = "IRIS", 
                   by.y = "IRIS_CODE", 
                   maille = "IRISrD_CODE", 
                   vars = c(4:13), 
                   funs = "sum")

sum(data_x$C20_POP15P_CS1, na.rm = TRUE) - sum(data_y$C20_POP15P_CS1, na.rm = TRUE)

# IRIS de data_x non presents dans tabl
missing <- setdiff(data_x$IRIS, d.irisr.pass$IRIS_CODE)

# Voir combien il y en a
length(missing)

# Lignes completes de data_x correspondant aux IRIS manquants
data_x_missing <- data_x[data_x$IRIS %in% missing, ]



















# remove.packages("asf")
# 
# remotes::install_gitlab("atlas-social-de-la-france/asf",
#                         host = "gitlab.huma-num.fr",
#                         build_vignettes = TRUE)

library(asf)


data <- read.csv("input/csp_2020.csv")

irisf <- read.csv("input/mar/d.irisf.pass.csv")
irisr <- read.csv("input/mar/d.irisr.pass.csv")[, c(1,2,4)]

  
data_if <- merge(irisf, data, by.x = "IRIS_CODE", by.y = "IRIS")
sum(data$C20_POP15P_CS1) - sum(data_if$C20_POP15P_CS1)

data_irs <- merge(irisr, data, by.x = "IRIS_CODE", by.y = "IRIS")
sum(data$C20_POP15P_CS1) - sum(data_irs$C20_POP15P_CS1)

data_ird <- merge(irisr, data, by.x = "IRIS_CODE", by.y = "IRIS")
sum(data$C20_POP15P_CS1) - sum(data_ird$C20_POP15P_CS1)

sum <- rowsum(data_if[, c(7:16)], group = data_if$COMF_CODE, na.rm = TRUE)
data_cf <- data.frame(COMF_CODE = rownames(sum), sum, row.names = NULL)
sum(data$C20_POP15P_CS1) - sum(data_cf$C20_POP15P_CS1)




data <- read.csv("input/csp_2020.csv")

mar <- asf_mar(maille = "irisrs")
tabl <- mar$tabl

datar <- merge(tabl, data, by.x = "IRIS_CODE", by.y = "IRIS")

sum(data$C20_POP15P_CS1, na.rm = TRUE) - sum(datar$C20_POP15P_CS1, na.rm = TRUE)


data <- read.csv("input/csp_2020.csv")

mar <- asf_mar(maille = "comr")
tabl <- mar$tabl

datax <- asf_data(d = data, 
                  t = tabl, 
                  by.x = "COM", 
                  by.y = "COM_CODE", 
                  maille = "COMR_CODE", 
                  vars = c(4:13), 
                  funs = "sum")

sum(data$C20_POP15P_CS1, na.rm = TRUE) - sum(datax$C20_POP15P_CS1, na.rm = TRUE)


















# Agregation avec le package asf
y_aggreg <- asf_dato(d = data.cp2,
                     t = tabl,
                     by.x = "IRIS",
                     by.y = "IRIS_CODE", 
                     maille = "IRISrD_CODE", 
                     keep = "IRISrD_LIB", 
                     vars = c(4:32),
                     funs = c("sum"))


# Verification de la coherence des resultats : pour x_aggreg, effet de l'iris en moins. Pour y_aggreg, il semble y avoir un problème.
sum(data.cp2$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(x_aggreg$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(y_aggreg$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)







datax <- merge(tabl, data.cp2, by.x = "IRIS_CODE", by.y = "IRIS")
datay <- merge(d.irisr.pass, data.cp2, by.x = "IRIS_CODE", by.y = "IRIS")

datag <- merge(d.irisf.pass, data.cp2, by.x = "IRIS_CODE", by.y = "IRIS")
datah <- merge(d.irisr.pass, datag[, -c(1, 3:6)], by = "IRISF_CODE")



sum(datax$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(datay$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(datag$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(datah$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)















library(dplyr)

d.irisf.pass <- read.csv("input/mar/d.irisf.pass.csv")
d.irisr.pass <- read.csv("input/mar/d.irisr.pass.csv")

# DPLYR -----------------------------------------------------------------------
datar <- data.cp2 |> 
  left_join(d.irisf.pass, by = c("IRIS" = "IRIS_CODE")) |> 
  left_join(d.irisr.pass |> distinct(IRISF_CODE, IRISrD_CODE, IRISrD_LIB),
            by = "IRISF_CODE") |> 
  group_by(IRISrD_CODE, IRISrD_LIB) |> 
  summarise_if(is.numeric, sum, na.rm = TRUE)

sum(data.cp2$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(datar$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)



# RBASE -----------------------------------------------------------------------
dataf <- merge(data.cp2, d.irisf.pass,
               by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE)

irisr.dist <- unique(d.irisr.pass[, c("IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB")])
datar <- merge(dataf, irisr.dist, by = "IRISF_CODE", all.x = TRUE)

# agregation (equivalent du summarise)
num_vars <- sapply(datar, is.numeric)
datar <- aggregate(datar[, num_vars],
                   by = list(IRISrD_CODE = datar$IRISrD_CODE,
                             IRISrD_LIB  = datar$IRISrD_LIB),
                   FUN = function(x) sum(x, na.rm = TRUE))

sum(data.cp2$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)
sum(datar$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)



# FONCTION EN RBASE -----------------------------------------------------------
irisr.dist <- unique(d.irisr.pass[, c("IRISF_CODE", "IRISrD_CODE", "IRISrD_LIB")])

datar <- asf_data(d = data.cp2,
                  t = irisr.dist,
                  by.x = "IRIS",
                  by.y = "IRISF_CODE",
                  maille = "IRISrD_CODE",
                  keep = "IRISrD_LIB",
                  vars = "BS18_S_T_CP1_C2.Arti",
                  funs = "sum")

sum(datar$BS18_S_T_CP1_C2.Arti, na.rm = TRUE)

class(data.cp2$IRIS)
class(irisr.dist$IRISF_CODE)
tmp <- data.cp2[!data.cp2$IRIS %in% irisr.dist$IRISF_CODE, ]

setdiff(data.cp2$IRIS, irisr.dist$IRISF_CODE)




