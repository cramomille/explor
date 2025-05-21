#' @title Telechargement du maillage d'Aliette Roux
#' @description 
#' Import des fichiers crees par Aliette ROUX pour la manipulation de son 
#' maillage composite
#' 
#' @param ar01 le boleen indiquant si l'on souhaite telecharger cet ensemble de donnees
#' @param ar02 le boleen insiquant si l'on souhaite telecharger cet ensemble de donnees
#' @param sf le boleen indiquant si l'on souhaite telecharger les objets sf
#' 
#' @return 
#' La fonction renvoie une liste composee de trois listes contenant des objets 
#' sf ou des data.frames
#'
#' @examples
#' \dontrun{
#' mar <- asf_mar()
#' }
#' @export

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