asf_mar <- function(ar01 = TRUE,
                    ar02 = TRUE,
                    sf = TRUE) {
  
  out_ar01 <- NULL
  out_ar02 <- NULL
  
  download <- "&mode=grid&download=1"
  
  if (ar01) {
    if (sf) {
      sf.comf <- sf::st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=F3QDGfpMaQ6llgzSELffrhkevwNhRmyR", download))
      sf.irisf <- sf::st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=hTkOOyJG3J1FjWFRjn3GL1CepTSsCjhI", download))
    } else {
      sf.comf <- NULL
      sf.irisf <- NULL
    }
    
    d.comf.pass <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=4zZAO8Tl2TB9EemgWJxVOm4XiE28xuPW", download))
    d.irisf.pass <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=aAZSfagRftWWzHgadZ9XuFbCGzdCOfiC", download))
    d.comf.app <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=5kID4Cjls6B5gIbWOwZhle4OFWrB4vae", download))
    
    out_ar01 <- list(sf.comf = sf.comf,
                     sf.irisf = sf.irisf,
                     d.comf.pass = d.comf.pass,
                     d.irisf.pass = d.irisf.pass,
                     d.comf.app = d.comf.app)
  }
  
  if (ar02) {
    if (sf) {
      sf.irisr.d <- sf::st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=htHxMi72rAgz3UMf4cPhgzXhzzwjvGr5", download))
      sf.irisr.s <- sf::st_read(paste0("https://sharedocs.huma-num.fr/wl/?id=gjsWtDqB8ojfLdtds225bgbosuBHOaAG", download))
    } else {
      sf.irisr.d  <- NULL
      sf.irisr.s  <- NULL
    }
    
    d.irisr.pass <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=1HjiqVhkuHjbUXviT8r8f0qaPAhcHNlO", download))
    d.irisr.app <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=W0bR4nwBQstoE6I1GHeUGKCg2gJ4nSsc", download))
    
    out_ar02 <- list(sf.irisr.d = sf.irisr.d,
                     sf.irisr.s = sf.irisr.s,
                     d.irisr.pass = d.irisr.pass,
                     d.irisr.app = d.irisr.app)
  }
  
  d.datatest <- utils::read.csv(paste0("https://sharedocs.huma-num.fr/wl/?id=wsQapNsSoxlBFPVp88TbFVmW6yzNQ0pa", download))
  out_data <- list(d.datatest = d.datatest)
  
  return(list(ar01 = out_ar01, ar02 = out_ar02, data = out_data))
}


mar <- asf_mar()

# a <- mar$ar01$sf.comf
# b <- mar$ar01$sf.irisf
# c <- mar$ar01$d.comf.pass
# d <- mar$ar01$d.irisf.pass
# e <- mar$ar01$d.comf.app
#
# f <- mar$ar02$sf.irisr.d
# g <- mar$ar02$sf.irisr.s
# h <- mar$ar02$d.irisr.pass
# i <- mar$ar02$d.irisr.app

################################################################### COM en COMF
d.comf.pass <- mar$ar01$d.comf.pass
d.comf.app <- mar$ar01$d.comf.app

nb <- unique(d.comf.pass$COM_CODE)

comf <- merge(d.comf.pass[, c(2,5)], d.comf.app, by = "COMF_CODE", all.x = TRUE)
names(comf)[5] <- "COM_TYPE"

na <- comf[apply(is.na(comf), 1, any), ]

comf <- comf[, c(
  "COM_CODE",
  "COM_TYPE",
  "COMF_CODE",
  "COMF_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf")))

################################################################### COM en COMR
d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(6,7)]

# Decomposition des identifiants agreges en une liste
id_list <- strsplit(d.irisr.app$COMF_CODE_MULTI, " \\| ")

# Creation d'une table d'association entre chaque commune et son COMF_CODE_MULTI
id_tabl <- data.frame(
  COMF_CODE = unlist(id_list),
  COMR_CODE = rep(d.irisr.app$COMF_CODE_MULTI, sapply(id_list, length))
)
id_tabl <- id_tabl[!duplicated(id_tabl$COMF_CODE), ]

summary(nchar(d.irisr.app$COMF_CODE_MULTI))
summary(nchar(id_tabl$COMR_CODE))

tmp <- comf[, c(1:4)]
nb <- unique(comf$COM_CODE)

comr <- merge(tmp, id_tabl, by = "COMF_CODE", all.x = TRUE)

d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(6,7, 11:26)]
d.irisr.app <- d.irisr.app[!duplicated(d.irisr.app$COMF_CODE_MULTI), ]

comr <- merge(comr, d.irisr.app, by.x = "COMR_CODE", by.y = "COMF_CODE_MULTI", all.x = TRUE)
names(comr)[5] <- "COMF_LIB"
names(comr)[6] <- "COMR_LIB"

comr <- comr[, c(
  "COM_CODE",
  "COM_TYPE",
  # "COMF_CODE",
  # "COMF_LIB",
  "COMR_CODE",
  "COMR_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr")))

################################################################# IRIS en IRISF
d.irisf.pass <- mar$ar01$d.irisf.pass
nb <- unique(d.irisf.pass$IRIS_CODE)

tmp <- comf[, -c(1,2)]
tmp <- tmp[!duplicated(tmp$COMF_CODE), ]

irisf <- merge(d.irisf.pass[, -1], tmp, by = "COMF_CODE")
nb <- unique(irisf$IRIS_CODE)

irisf <- irisf[, c(
  "IRIS_CODE",
  "IRISF_CODE",
  "IRISF_LIB",
  "COMF_CODE",
  "COMF_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr", "irisf")))

################################################################# IRIS en IRISR
d.irisr.pass <- mar$ar02$d.irisr.pass
d.irisr.app <- mar$ar02$d.irisr.app
d.irisr.app <- d.irisr.app[, c(2,6,7,11:26)]

irisr <- merge(d.irisr.pass[, -1], d.irisr.app, by = "IRISrD_CODE")
names(irisr)[8] <- "COMR_CODE"
names(irisr)[9] <- "COMR_LIB"

irisr <- irisr[, c(
  "IRIS_CODE",
  "IRISF_CODE",
  "IRISrS_CODE",
  "IRISrS_LIB",
  "IRISrD_CODE",
  "IRISrD_LIB",
  # "COMF_CODE",
  "COMR_CODE",
  "COMR_LIB",
  "OM_CODE",
  "EPCI",
  "NATURE_EPCI",
  "ARR",
  "CV",
  "UU2020",
  "TUU2017",
  "TDUU2017",
  "BV2022",
  "ZE2020",
  "AAV2020",
  "TAAV2017",
  "TDAAV2017",
  "CATEAAV2020",
  "DEP",
  "REG"
)]

rm(list = setdiff(ls(), c("mar", "comf", "comr", "irisf", "irisr")))

####################################################################### EXPORTS
library(sf)

comf[] <- lapply(comf, as.character)
str(comf)
comr[] <- lapply(comr, as.character)
str(comr)
irisf[] <- lapply(irisf, as.character)
str(irisf)
irisr[] <- lapply(irisr, as.character)
str(irisr)

comf <- comf[order(comf$COM_CODE), ]
comr <- comr[order(comr$COM_CODE), ]
irisf <- irisf[order(irisf$IRIS_CODE), ]
irisr <- irisr[order(irisr$IRIS_CODE), ]

sf.irisf <- mar$ar01$sf.irisf
sf.irisf <- sf.irisf[, c(1,2)]
st_write(sf.irisf, "sf.irisf.gpkg")
write.csv(comf, "df.comf.csv")
write.csv(comr, "df.comr.csv")
write.csv(irisf, "df.irisf.csv")
write.csv(irisr, "df.irisr.csv")
