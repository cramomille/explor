
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

data <- read.csv("input/csp_2020.csv")
geom <- asf_mar(geom = TRUE)
tabl <- asf_mar(md = "iris_xxxx", ma = "iris_r2")

data_r2 <- asf_data(data, tabl, 
                    by.x = "IRIS", by.y = "IRIS_CODE", 
                    maille = "IRISrD_CODE", 
                    vars = c(4:13), funs = "sum")

sum(data$P20_POP)
sum(data_r2$P20_POP)

geom <- asf_drom(geom)

com_r2 <- asf_fond(geom, tabl, 
                   by = "IRISF_CODE", 
                   maille = "IRISrD_CODE")


mf_map(com_r2)


z <- asf_zoom(com_r2, 
              places = c("Paris", "Avignon", "Bergerac", "Annecy"))

zoom <- z$zooms
label <- z$labels
point <- z$points

com_r2_simply <- asf_simplify(com_r2)


fondata <- asf_fondata(com_r2_simply, 
                       zoom, 
                       data_r2, 
                       by = "IRISrD_CODE")


fondata$pct_var <- fondata$C20_POP15P_CS6 / fondata$C20_POP15P * 100

q6 <- quantile(fondata$pct_var, 
               probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), 
               na.rm = TRUE)

pal <- asf_palette("seq")


mf_map(fondata, 
       var = "pct_var", 
       type = "choro", 
       breaks = q6, 
       pal = pal, 
       border = NA)

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
  tav = c("1", "1", "1", "1", "2", "2", "2", "2", "3", NA),
  cav = c("1", "1", "2", "2", "1", "1", "2", "2", "1", NA),
  pop = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
  m50 = c(20, 30, 50, 80, 130, 210, 220, 230, 450, 460),
  p50 = c(30, 70, 100, 120, 120, 90, 130, 170, 0, 40),
  cah = c("c2", "c1", "c2", "c1", "c2", "c2", "c1", "c2", "c1", NA)
)

palette <- asf_palette(type = "qua")






asf_plot_typo(data, vars = "cah", typo = "tav", 
              order.v = c(2, 1),
              order.t = c(3, 1, 2)
              )

asf_plot_typo(data, vars = c("p50", "m50"), typo = "tav",
              order.v = c(2, 1),
              order.t = c(2, 3, 1)
              )

asf_plot_typo(data, vars = "cah", typo = c("tav", "cav"),
              order.v = c(2, 1),
              order.t = c(2, 3, 1)
              )

asf_plot_typo(data, vars = c("m50", "p50"), typo = c("tav", "cav"),
              order.v = c(2, 1),
              order.t = c(2, 3, 1)
              )





asf_plot_vars(data, vars = "cah", typo = "tav",
              # order.v = c(2, 1),
              order.t = c(2, 3, 1)
              )

asf_plot_vars(data, vars = "m50", typo = "tav",
              )

asf_plot_vars(data, vars = c("m50", "p50"), typo = "tav",
              )

asf_plot_vars(data, vars = "cah", typo = c("tav", "cav"),
              )

asf_plot_vars(data, vars = "m50", typo = c("tav", "cav"),
              )

asf_plot_vars(data, vars = c("m50", "p50"), typo = c("tav", "cav"),
              )



























asf_plot_typo(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_typo(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_typo(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_typo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_typo(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_typo(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_typo(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_typo(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)


asf_plot_vars(data, vars = "cah", typo = "tav", pal = palette)
asf_plot_vars(data, vars = "m50", typo = "tav", pal = palette)
asf_plot_vars(data, vars = c("m50", "p50"), typo = "tav", pal = palette)

asf_plot_vars(data, vars = "cah", typo = c("tav", "cav"), pal = palette)
asf_plot_vars(data, vars = "m50", typo = c("tav", "cav"), pal = palette)
asf_plot_vars(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette)


asf_plot_vars(data, vars = "cah", typo = "tav", pal = palette, eff = TRUE)
asf_plot_vars(data, vars = "m50", typo = "tav", pal = palette, eff = TRUE)
asf_plot_vars(data, vars = c("m50", "p50"), typo = "tav", pal = palette, eff = TRUE)

asf_plot_vars(data, vars = "cah", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_vars(data, vars = "m50", typo = c("tav", "cav"), pal = palette, eff = TRUE)
asf_plot_vars(data, vars = c("m50", "p50"), typo = c("tav", "cav"), pal = palette, eff = TRUE)












