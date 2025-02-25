# METHODE 1 --------------------------------------------------------------------
# Creation de la connexion a DuckDB pour executer des requetes SQL
con <- dbConnect(duckdb())

# Direction du dossier qui contient les fichiers .parquet
parquet_dir <- "test/parquet/export/data1/"

# Recuperation des noms de tous les fichiers .parquet
parq <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

# Creation d'une vue SQL pour agreger les fichiers .parquet
query <- paste0(
  "CREATE OR REPLACE VIEW all_data AS ",
  paste0("SELECT * FROM read_parquet('", parq, "')", collapse = " UNION ALL ")
)
dbExecute(con, query)

# Chargement de la vue comme table DuckDB
tbl_duckdb <- tbl(con, "all_data")

# Inspections des colonnes disponibles
tbl_duckdb$lazy_query$vars

# Selection et collecte des donnees d'interet avec dplyr
data <- tbl_duckdb %>%
  # filter(dep == "55") %>%
  select(c(1:2, 45:53)) %>%
  collect()

# Deconnexion de DuckDB
dbDisconnect(con, shutdown = TRUE)