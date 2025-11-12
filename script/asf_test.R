
#                                                           TEST DU PACKAGE ASF
#
#                                                                antoine beroud
#                                                                    avril 2025

# remove.packages("rmapshaper")
# remove.packages("mapinsetr")
# remove.packages("asf")
# 
# remotes::install_gitlab(repo = "atlas-social-de-la-france/asf",
#                         host = "gitlab.huma-num.fr",
#                         build_vignettes = TRUE,
#                         force = TRUE,
#                         upgrade = "never")

library(sf)
library(asf)
library(mapsf)


# Recuperation d'une table avec tous les iris qui ont un jour existe entre 1943 
# et 2023 et pour chacun leur iris_r5
mar <- asf_mar(md = "iris_xxxx", ma = "iris_r5", geom = TRUE)

fond_iris_f <- mar$geom
tabl <- mar$tabl

fond_iris_r5 <- asf_fond(fond_iris_f,
                         tabl, 
                         by = "IRISF_CODE", 
                         maille = "IRISr5_CODE")

mf_map(fond_iris_r5)




geom <- asf_drom(geom, id = "IRISF_CODE")
geom <- asf_fond(geom, tabl, 
                 by = "IRISF_CODE", 
                 maille = "IRISrD_CODE")

data <- read_csv("input/csp_2020.csv")
data_r2 <- asf_data(data, tabl, 
                    by.x = "IRIS", by.y = "IRIS_CODE", 
                    maille = "IRISrD_CODE", 
                    vars = c(4:13), funs = "sum")

sum(data$P20_POP)
sum(data_r2$P20_POP)











z <- asf_zoom(comr, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

comr_simply <- asf_simplify(comr)



data <- read.csv("input/csp_2020.csv")

comr_data <- asf_data(data, 
                      tabl, 
                      by.x = "IRIS", 
                      by.y = "IRIS_CODE", 
                      maille = "COMR2_CODE", 
                      vars = c(4:13), 
                      funs = c("sum"))



fondata <- asf_fondata(comr_simply, 
                       zoom, 
                       comr_data, 
                       by = "COMR2_CODE")


fondata$pct_var <- fondata$C20_POP15P_CS6 / fondata$C20_POP15P * 100

q6 <- quantile(fondata$pct_var, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

geom$DEP_CODE <- substr(geom$COMF_CODE, 1, 2)
dep <- asf_borders(geom, by = "DEP_CODE", keep = 0.01)

pal <- asf_palette("seq")


mf_map(fondata, 
       var = "pct_var", 
       type = "choro", 
       breaks = q6, 
       pal = pal, 
       border = NA)

mf_map(dep, 
       col = "#ffffff", 
       lwd = 0.5, 
       add = TRUE)

mf_map(point, 
       col = "red", 
       add = TRUE)

mf_label(label, 
         var = "label", 
         cex = 0.8)


data <- read.csv("input/csp_2020.csv")

tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r2", dir = "input/mar")

tmp <- merge(data, tabl, by.x = "IRIS", by.y = "IRIS_CODE", all.x = TRUE)

pal <- asf_palette(pal = "tulipe", nb = 8)
asf_plot_typo(tmp,
              vars = c(6:13),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7), 
              order.t = c("5", "1", "2", "3", "4", "0"),
              pal = pal
              )

asf_plot_typo(tmp,
              vars = c(6:13),
              typo = c("TAAV2017", "CATEAAV2020"), 
              order.t = c("30", "20", "13", "12", "11"),
              pal = pal, 
              eff = TRUE
              )






pal <- asf_palette(pal = "tulipe", nb = 6)
asf_plot_vars(tmp,
              vars = c(6:13),
              typo = "TAAV2017", 
              order.v = c(1:6, 8, 7),
              order.t = c("5", "1", "2", "3", "4", "0"),
              pal = pal
              )

pal <- asf_palette(pal = "tulipe", nb = 6)
asf_plot_vars(tmp,
              vars = c(6),
              typo = "TAAV2017",
              pal = pal
              )




#

data <- data.frame(
  com = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
  tav = c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3"),
  cav = c("1", "1", "2", "2", "1", "1", "2", "2", "1", "2"),
  pop = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
  m50 = c(20, 30, 50, 80, 130, 210, 220, 230, 450, 460),
  p50 = c(30, 70, 100, 120, 120, 90, 130, 170, 0, 40),
  cah = c("c1", "c2", "c2", "c1", "c2", "c2", "c1", "c2", "c1", "c1")
)

palette <- asf_palette(type = "qua")



asf_plot_typa(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_typa(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_typa(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_typa(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_typa(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_typa(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_typa(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_typa(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)


asf_plot_varo(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_varo(data, vars = "m50", typo = "tav", pal = palette)
asf_plot_varo(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_varo(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_varo(data, vars = "m50", typo = c("tav", "cav"), pal = palette)
asf_plot_varo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_varo(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_varo(data, vars = "m50", typo = "tav", pal = palette, eff = TRUE)
asf_plot_varo(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_varo(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_varo(data, vars = "m50", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_varo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)

