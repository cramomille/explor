#' @title Transformation de fichiers .sas7bdat en fichiers .parquet
#' @description
#' Cette fonction permet de creer des fichiers .parquet a partir de fichiers 
#' .sas7bdat
#' 
#' @param sas le vecteur avec les chemins vers les fichiers .sas7bdat
#' @param parquet le chemin vers le dossier ou sera cree les dossiers qui 
#' contiendront les fichiers .parquet
#' @param chunk le nombre de lignes des chunks qui constitueront le fichier 
#' .parquet
#' 
#' @return 
#' La fonction cree un dossier du meme nom que le fichier .sas7bdat qui 
#' contiendra les fichiers .parquet crees (si le fichier .sas traite est compose
#' de 100 lignes et que la taille des chunks est de 10, alors il y aura 10 
#' fichiers .parquet)
#' 
#' @examples
#' parquet_convert(sas = c("test/parquet/data1.sas7bdat", 
#'                         "test/parquet/data2.sas7bdat"), 
#'                 parquet = "test/parquet/export/",
#'                 chunk = 100000)


library(haven)
library(arrow)

parquet_convert <- function(sas,
                            parquet,
                            chunk = 1000000) {
  
  for (x in sas) {
    sas_name <- sub("\\.sas7bdat$", "", basename(x))
    
    # Creation d'un dossier specifique pour chaque fichier .sas7bdat
    output_dir <- file.path(parquet, sas_name)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Definition des parametres des chunks
    count <- 1
    row <- 0
    total_time <- 0
    
    # Boucle principale
    repeat {
      
      start_time <- Sys.time()
      
      # Lecture d'un chunk du fichier .sas7bdat
      c <- tryCatch(
        read_sas(x, skip = row, n_max = chunk),
        error = function(e) NULL
      )
      
      if (is.null(c) || nrow(c) == 0) break
      
      # Ecriture du fichier .parquet dans le dossier specifique au fichier .sas7bdat
      output <- file.path(output_dir, paste0(sas_name, "_chunk", sprintf("%02d", count), ".parquet"))
      write_parquet(c, output, compression = "snappy")
      
      end_time <- Sys.time()
      
      # Calcul du temps de calcul et d'ecriture du chunk
      chunk_time <- as.numeric(end_time - start_time, units = "secs")
      total_time <- total_time + chunk_time
      
      h <- floor(chunk_time / 3600)
      m <- floor((chunk_time %% 3600) / 60)
      s <- round(chunk_time %% 60)
      
      # Mise a jour de la premiere ligne du prochain chunk
      row <- row + nrow(c)
      row_text <- formatC(row, format = "d", big.mark = " ")
      
      # Message d'avancement du traitement du fichier .sas7bdat
      cat(sprintf("chunk %02d [time %02d:%02d:%02d]    %10s lignes\n",
                  count, h, m, s, row_text))
      
      # Incrementation du compteur de chunks
      count <- count + 1
    }
    
    # Message final pour chaque fichier .sas7bdat entierement traite
    total_h <- floor(total_time / 3600)
    total_m <- floor((total_time %% 3600) / 60)
    total_s <- round(total_time %% 60)
    
    cat(sprintf("\nfichier '%s' [time %02d:%02d:%02d]\n\n",
                sas_name, total_h, total_m, total_s))
  }
}