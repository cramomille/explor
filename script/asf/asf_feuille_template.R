
library(sf)
library(asf)
library(mapsf)

# Tableau des lieux predefinis
p_data <- c(
  "Abbeville", 2, 1.832, 50.108,
  "Agde", 2, 3.484, 43.309,
  "Agen", 2, 0.626, 44.203,
  "Ajaccio", 2, 8.701, 41.935,
  "Albertville", 2, 6.404, 45.668,
  "Albi", 2, 2.147, 43.926,
  "Alencon", 2, 0.092, 48.432,
  "Ales", 2, 4.089, 44.125,
  "Amiens", 3, 2.290, 49.900,
  "Ancenis-Saint-Gereon", 2, -1.189, 47.380,
  "Angers", 3, -0.556, 47.477,
  "Angouleme", 2, 0.145, 45.647,
  "Annecy", 3, 6.118, 45.902,
  "Annonay", 2, 4.651, 45.246,
  "Arles", 2, 4.662, 43.547,
  "Arras", 2, 2.766, 50.290,
  "Aubenas", 2, 4.396, 44.610,
  "Auch", 2, 0.575, 43.653,
  "Auchel", 2, 2.469, 50.510,
  "Aurillac", 2, 2.442, 44.924,
  "Auxerre", 2, 3.582, 47.794,
  "Avignon", 3, 4.841, 43.935,
  "Bar-le-Duc", 2, 5.163, 48.764,
  "Basse-Terre", 2, -61.728, 16.000,
  "Bastia", 2, 9.425, 42.686,
  "Bayonne", 3, -1.466, 43.492,
  "Beaune", 2, 4.837, 47.026,
  "Beauvais", 2, 2.086, 49.436,
  "Belfort", 2, 6.845, 47.647,
  "Bergerac", 2, 0.487, 44.854,
  "Besancon", 3, 6.019, 47.255,
  "Bethune", 2, 2.672, 50.528,
  "Beziers", 2, 3.231, 43.347,
  "Blois", 2, 1.306, 47.582,
  "Bordeaux", 4, -0.573, 44.858,
  "Boulogne-sur-Mer", 2, 1.606, 50.727,
  "Bourg-en-Bresse", 2, 5.246, 46.205,
  "Bourges", 2, 2.405, 47.075,
  "Brest", 3, -4.502, 48.400,
  "Brive-la-Gaillarde", 2, 1.519, 45.143,
  "Caen", 3, -0.370, 49.185,
  "Cahors", 2, 1.441, 44.451,
  "Calais", 2, 1.874, 50.952,
  "Cambrai", 2, 3.242, 50.170,
  "Cannes", 3, 7.005, 43.552,
  "Carcassonne", 2, 2.343, 43.209,
  "Carpentras", 2, 5.061, 44.059,
  "Castres", 2, 2.238, 43.616,
  "Cayenne", 2, -52.317, 4.934,
  "Chalon-sur-Saone", 2, 4.852, 46.790,
  "Chalons-en-Champagne", 2, 4.378, 48.964,
  "Chambery", 3, 5.909, 45.584,
  "Charleville-Mezieres", 2, 4.717, 49.775,
  "Chartres", 2, 1.506, 48.447,
  "Chateauroux", 2, 1.694, 46.803,
  "Chatellerault", 2, 0.553, 46.816,
  "Chaumont", 2, 5.140, 48.098,
  "Cherbourg-en-Cotentin", 2, -1.621, 49.632,
  "Cholet", 2, -0.878, 47.046,
  "Clermont-Ferrand", 3, 3.115, 45.786,
  "Cluses", 2, 6.580, 46.063,
  "Cognac", 2, -0.335, 45.696,
  "Colmar", 2, 7.385, 48.110,
  "Compiegne", 2, 2.853, 49.399,
  "Dax", 2, -1.060, 43.701,
  "Dieppe", 2, 1.087, 49.922,
  "Dijon", 3, 5.037, 47.323,
  "Dole", 2, 5.501, 47.075,
  "Douai", 2, 3.091, 50.382,
  "Draguignan", 2, 6.455, 43.536,
  "Dunkerque", 3, 2.337, 51.030,
  "Epernay", 2, 3.932, 49.037,
  "Epinal", 2, 6.480, 48.163,
  "Evreux", 2, 1.142, 49.020,
  "Flers", 2, -0.563, 48.740,
  "Fort-de-France", 3, -61.069, 14.641,
  "Fougeres", 2, -1.194, 48.353,
  "Frejus", 2, 6.764, 43.472,
  "Gap", 2, 6.065, 44.580,
  "Grenoble", 4, 5.721, 45.182,
  "Haguenau", 2, 7.830, 48.842,
  "La Roche-sur-Yon", 2, -1.408, 46.668,
  "La Rochelle", 3, -1.173, 46.162,
  "Lannion", 2, -3.461, 48.744,
  "Laon", 2, 3.621, 49.568,
  "Laval", 2, -0.766, 48.061,
  "Le Havre", 3, 0.141, 49.498,
  "Le Mans", 3, 0.200, 47.989,
  "Le Puy-en-Velay", 2, 3.896, 45.028,
  "Lens", 3, 2.821, 50.438,
  "Les Abymes", 3, -61.502, 16.273,
  "Les Herbiers", 2, -1.022, 46.867,
  "Les Sables-d'Olonne", 2, -1.764, 46.520,
  "Libourne", 2, -0.235, 44.913,
  "Lille", 4, 3.047, 50.632,
  "Limoges", 3, 1.249, 45.854,
  "Lisieux", 2, 0.238, 49.147,
  "Lons-le-Saunier", 2, 5.558, 46.675,
  "Lorient", 3, -3.379, 47.751,
  "Louviers", 2, 1.153, 49.221,
  "Lyon", 4, 4.836, 45.755,
  "Macon", 2, 4.819, 46.321,
  "Mamoudzou", 3, 45.195, -12.790,
  "Manosque", 2, 5.784, 43.840,
  "Marmande", 2, 0.173, 44.520,
  "Marseille", 4, 5.420, 43.292,
  "Maubeuge", 2, 3.963, 50.284,
  "Metz", 3, 6.196, 49.108,
  "Mont-de-Marsan", 2, -0.490, 43.900,
  "Montargis", 2, 2.737, 47.999,
  "Montauban", 2, 1.364, 44.022,
  "Montbeliard", 2, 6.792, 47.516,
  "Montceau-les-Mines", 2, 4.354, 46.676,
  "Montelimar", 2, 4.748, 44.554,
  "Montlucon", 2, 2.604, 46.339,
  "Montpellier", 4, 3.869, 43.613,
  "Morlaix", 2, -3.820, 48.600,
  "Moulins", 2, 3.327, 46.563,
  "Mulhouse", 3, 7.325, 47.749,
  "Nancy", 3, 6.175, 48.690,
  "Nantes", 4, -1.548, 47.232,
  "Narbonne", 2, 3.020, 43.165,
  "Nevers", 2, 3.157, 46.988,
  "Nice", 3, 7.238, 43.712,
  "Nimes", 3, 4.348, 43.845,
  "Niort", 2, -0.465, 46.328,
  "Orange", 2, 4.810, 44.129,
  "Orleans", 3, 1.917, 47.882,
  "Oyonnax", 2, 5.653, 46.261,
  "Paris", 5, 2.343, 48.857,
  "Pau", 3, -0.350, 43.320,
  "Perigueux", 2, 0.712, 45.192,
  "Perpignan", 3, 2.899, 42.696,
  "Poitiers", 3, 0.360, 46.584,
  "Quimper", 3, -4.091, 47.997,
  "Reims", 3, 4.040, 49.252,
  "Rennes", 4, -1.682, 48.112,
  "Roanne", 2, 4.080, 46.045,
  "Rochefort", 2, -0.972, 45.943,
  "Rodez", 2, 2.567, 44.359,
  "Romans-sur-Isere", 2, 5.038, 45.055,
  "Rouen", 3, 1.094, 49.441,
  "Roussillon", 2, 4.819, 45.379,
  "Royan", 2, -1.018, 45.634,
  "Saint-Benoit", 2, 55.649, -21.092,
  "Saint-Brieuc", 3, -2.763, 48.515,
  "Saint-Denis", 3, 55.447, -20.933,
  "Saint-Die-des-Vosges", 2, 6.938, 48.297,
  "Saint-Dizier", 2, 4.949, 48.628,
  "Saint-Etienne", 3, 4.379, 45.430,
  "Saint-Lo", 2, -1.077, 49.110,
  "Saint-Louis", 2, 55.439, -21.192,
  "Saint-Malo", 2, -1.981, 48.640,
  "Saint-Nazaire", 3, -2.251, 47.280,
  "Saint-Omer", 2, 2.264, 50.768,
  "Saint-Paul", 2, 55.322, -21.045,
  "Saint-Pierre", 3, 55.494, -21.312,
  "Saint-Quentin", 2, 3.278, 49.847,
  "Saintes", 2, -0.650, 45.742,
  "Salon-de-Provence", 2, 5.068, 43.646,
  "Sarrebourg", 2, 7.047, 48.738,
  "Sarreguemines", 2, 7.070, 49.109,
  "Saumur", 2, -0.083, 47.267,
  "Selestat", 2, 7.462, 48.248,
  "Sens", 2, 3.301, 48.186,
  "Soissons", 2, 3.325, 49.379,
  "Strasbourg", 4, 7.768, 48.571,
  "Tarbes", 2, 0.066, 43.235,
  "Thonon-les-Bains", 2, 6.481, 46.369,
  "Toulon", 3, 5.933, 43.137,
  "Toulouse", 4, 1.432, 43.596,
  "Tours", 3, 0.696, 47.398,
  "Troyes", 3, 4.078, 48.297,
  "Valence", 3, 4.914, 44.923,
  "Valenciennes", 3, 3.516, 50.359,
  "Vannes", 2, -2.757, 47.660,
  "Vesoul", 2, 6.155, 47.632,
  "Vichy", 2, 3.424, 46.130,
  "Villeneuve-sur-Lot", 2, 0.742, 44.425,
  "Vitre", 2, -1.193, 48.114
)

# Conversion en data.frame
p_coords <- data.frame(
  lab = p_data[seq(1, length(p_data), by = 4)],
  aav = as.numeric(p_data[seq(2, length(p_data), by = 4)]),
  lon = as.numeric(p_data[seq(3, length(p_data), by = 4)]),
  lat = as.numeric(p_data[seq(4, length(p_data), by = 4)])
)

pts <- st_as_sf(
  p_coords,
  coords = c("lon", "lat"),
  crs = 4326
)


geom <- asf_mar(geom = TRUE, dir = "input/mar/")
geom <- asf_drom(geom)
geom$DEP <- substr(geom$IRISF_CODE, 1, 2)

mf_map(geom)

pts <- asf_drom(pts, f_ref = geom)

pts_5 <- pts[pts$aav == "5", ]
pts_4 <- pts[pts$aav == "4", ]
pts_3 <- pts[pts$aav == "3", ]
pts_2 <- pts[pts$aav == "2", ]

mf_map(pts)

geom_simply <- asf_simplify(geom, keep = 0.8)
geom_simply <- asf_fond(geom_simply, maille = "COMF_CODE")

dep <- asf_borders(geom, by = "DEP", keep = 0.05)

monde <- st_read("input/monde_1M.gpkg")
monde <- monde[monde$CNTR_ID %in% c("FR", "UK", "BE", "DE", "CH", "IT", "ES", "LU", "AD", "MC", "JE", "GG"), ]
monde <- st_transform(monde, 2154)
monde <- asf_simplify(monde, keep = 0.2)

mf_map(geom_simply, border = NA)
mf_map(dep, col = "#fff", add = TRUE)
mf_map(monde, col = NA, add = TRUE)

mf_map(pts_5, var = "lab", col = "black", add = TRUE)
mf_map(pts_4, var = "lab", col = "red", add = TRUE)
mf_map(pts_3, var = "lab", col = "orange", add = TRUE)
mf_map(pts_2, var = "lab", col = "pink", add = TRUE)

mf_label(pts_5, var = "lab", col = "black", pos = 4)
mf_label(pts_4, var = "lab", col = "red", pos = 4)
mf_label(pts_3, var = "lab", col = "orange", pos = 4)
mf_label(pts_2, var = "lab", col = "pink", pos = 4)








# POPSU ----
mar <- asf_mar(md = "iris_xxxx", ma = "iris_f", geom = TRUE, dir = "input/mar")

tabl <- mar$tabl
geom <- mar$geom

geom <- asf_drom(geom)
geom <- asf_simplify(geom, keep = 0.05)

epci <- asf_fond(geom, tabl, by = "IRISF_CODE", maille = "EPCI")


p <- c("Lille", "Nancy", "Epinal", "Bourges", "Chateauroux", 
       "Le Mans", "Rennes", "Nantes", "Brest", "Caen", 
       "Rouen", "Paris", "Dijon", "Bordeaux", "Clermont-Ferrand", 
       "Grenoble", "Toulouse", "Pau"
       )

c <- c(2.337, 50.908,  # Dunkerque
       6.086, 49.220,  # Thionville-Metz
       7.698, 48.592,  # Strasbourg
       0.579, 47.380,  # Tours
       4.887, 45.797,  # Lyon
       4.314, 45.451,  # Saint-Etienne
       7.126, 43.822,  # Nice
       5.471, 43.385,  # Marseille
       -1.328, 43.411  # Bayonne
       )

l <- c("Dunkerque", 
       "Thionville-Metz", 
       "Strasbourg", 
       "Tours", 
       "Lyon", 
       "Saint-Etienne",
       "Nice", 
       "Marseille", 
       "Bayonne")


z <- asf_zoom(epci, 
              places = p,
              coords = c,
              labels = l, 
              r = 30000)


mf_map(epci)
mf_map(z[[1]])
mf_label(z[[2]], var = "label")

