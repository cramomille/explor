# # Construction du dictionnaire communes/epci au 2024-01-01
library(readxl)


EPCI <- read_xlsx(system.file("/delete/EPCI_au_01-01-2024.xlsx", package = "asf"), sheet = "EPCI", skip = 5) |> as.data.frame()
CM <- read_xlsx(system.file("/delete/EPCI_au_01-01-2024.xlsx", package = "asf"), sheet = 2, skip = 5) |> as.data.frame()
CM <- merge(CM, EPCI[, c(-2, -4)], by = "EPCI", all.x = T)
CM[CM$NATURE_EPCI=="ZZ", "EPCI"] <- paste0("ZZ", CM[CM$NATURE_EPCI=="ZZ", "CODGEO"])
ARM <- read_xlsx(system.file("/delete//base-cc-serie-historique-2021.xlsx", package = "asf"), sheet = "ARM_2021",
                  skip = 5) |> as.data.frame()

CM_P <- CM[CM$CODGEO=="75056",]
ARM_P <- ARM[ARM$DEP=="75", c("CODGEO", "LIBGEO", "DEP", "REG")]
n <- nrow(ARM_P)
ARM_P <- cbind(EPCI = rep(CM_P$EPCI, n), ARM_P[, 1:2], LIBEPCI = rep(CM_P$LIBEPCI, n), ARM_P[, 3:4], NATURE_EPCI = rep(CM_P$NATURE_EPCI, n))

CM_L <- CM[CM$CODGEO=="69044",]
ARM_L <- ARM[ARM$DEP=="69", c("CODGEO", "LIBGEO", "DEP", "REG")]
n <- nrow(ARM_L)
ARM_L <- cbind(EPCI = rep(CM_L$EPCI, n), ARM_L[, 1:2], LIBEPCI = rep(CM_L$LIBEPCI, n), ARM_L[, 3:4], NATURE_EPCI = rep(CM_L$NATURE_EPCI, n))

CM_M <- CM[CM$CODGEO=="13001",]
ARM_M <- ARM[ARM$DEP=="13", c("CODGEO", "LIBGEO", "DEP", "REG")]
n <- nrow(ARM_M)
ARM_M <- cbind(EPCI = rep(CM_M$EPCI, n), ARM_M[, 1:2], LIBEPCI = rep(CM_M$LIBEPCI, n), ARM_M[, 3:4], NATURE_EPCI = rep(CM_M$NATURE_EPCI, n))

CM <- rbind(CM, ARM_P, ARM_L, ARM_M)






COM <-  read_xlsx(system.file("/delete//base-cc-serie-historique-2021.xlsx", package = "asf"), sheet = "COM_2021",
                  skip = 5) |> as.data.frame()


COM$P21_DEN <- COM$P21_POP / COM$SUPERF


x <- COM
y <- CM
x.id = "CODGEO"
y.id = "CODGEO"
by = "EPCI"
var = c("P21_POP", "SUPERF", "P21_DEN")
FUN = c('sum', 'sum', 'median')
head(COM)

agg_dataset <- function(x, y, x.id, y.id, by, var, FUN){
  fu <- merge(x[ , c(x.id, var)], y[, c(y.id, by)], x.id, y.id, all.x = T)

  data.frame(
    tapply(fu[, by]     , fu[, by], head, 1),
    tapply(fu[, var[1]]  , fu[, by], FUN[1])
  )
}
#
#   ,
#     MED      = tapply(m$MED         , m$fusion, mean),
#     geometry = tapply(st_geometry(m), m$fusion, st_union),
#     crs      = st_crs(m)
#   )
#   fus
#
#
#
# }
#