mar <- asf_mar()

sf.irisf <- mar$ar01$sf.irisf
sf.comf <- mar$ar01$sf.comf

d.comf.pass <- mar$ar01$d.comf.pass
d.irisf.pass <- mar$ar01$d.irisf.pass
d.comf.app <- mar$ar01$d.comf.app

tabl <- merge(d.irisf.pass, d.comf.pass, by = "COMF_CODE", all = TRUE)
tabl <- tabl[, c(3:5, 7, 1, 10)]

tmp <- tabl[apply(is.na(tabl), 1, any), ]


tabl <- merge(tabl, d.comf.app[, -c(1, 3)], by = "COMF_CODE", all = TRUE)
names(tabl)[7] <- "COM_TYPE"

tabl1 <- tabl[, c(
  
  "IRIS_CODE",
  "IRISF_CODE",
  "IRISF_LIB",
  
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


sf.irisf <- sf.irisf[, c(1, 2)]



rm(sf.comf,
   d.comf.pass,
   d.irisf.pass,
   d.comf.app,
   tabl,
   tmp)


###############################################################################

sf.irisr.d <- mar$ar02$sf.irisr.d
sf.irisr.s <- mar$ar02$sf.irisr.s

d.irisr.pass <- mar$ar02$d.irisr.pass
d.irisr.app <- mar$ar02$d.irisr.app

tabl <- merge(d.irisr.pass, d.irisr.app[, -c(3)], by = "IRISrD_CODE")
names(tabl)[13] <- "COMF_LIB_MULTI"

tabl2 <- tabl[, c(
  "IRISF_CODE",
  
  "IRISrS_CODE",
  "IRISrS_LIB",
  "IRISrD_CODE",
  "IRISrD_LIB",
  
  "COMF_CODE",
  "COMF_CODE_MULTI",
  "COMF_LIB_MULTI",
  
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

st_write(sf.irisf, "sf.irisf.gpkg")
write.csv(tabl1, "df.tabl_irisf.csv")
write.csv(tabl2, "df.tabl_irisr.csv")