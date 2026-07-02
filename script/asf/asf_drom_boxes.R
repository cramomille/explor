# Test asf_drom() -------------------------------------------------------------

geom <- asf_mar(geom = TRUE)

fond <- asf_fond(geom, maille = "COMF_CODE")

fond_wgs84 <- st_transform(fond, 4326)

drom_def <- data.frame(
  name     = c("Guadeloupe", "Martinique", "Guyane", "Reunion", "Mayotte"),
  dep      = c("971", "972", "973", "974", "976"),
  epsg_loc = c(5490, 5490, 2972, 2975, 4471),
  stringsAsFactors = FALSE
)

# bbox carre et centre autour d'une geometrie, avec marge
square_bbox_center <- function(geom, marge = 0.05) {
  bb <- st_bbox(geom)
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  cote <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) * (1 + marge)
  c(xmin = as.numeric(cx - cote / 2),
    ymin = as.numeric(cy - cote / 2),
    xmax = as.numeric(cx + cote / 2),
    ymax = as.numeric(cy + cote / 2))
}

targets <- vector("list", nrow(drom_def))
names(targets) <- drom_def$name

for (i in seq_len(nrow(drom_def))) {
  code <- drom_def$dep[i]
  sub <- iris_wgs84[substr(iris_wgs84$IRISF_CODE, 1, 3) == code, ]
  if (nrow(sub) == 0) stop("aucune entite trouvee pour le departement ", code)
  targets[[i]] <- square_bbox_center(sub, marge = 0.5)
}

# Valeurs pretes a copier dans asf_drom()
for (nm in names(targets)) {
  bb <- targets[[nm]]
  cat(sprintf("c(%.5f, %.5f, %.5f, %.5f), # %s\n",
              bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], nm))
}