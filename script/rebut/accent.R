accent <- list(
  "[ag]" = "\u00e0",
  "[ug]" = "\u00f9",
  "[cc]" = "\u00e7",
  "[ea]" = "\u00e9",
  "[eg]" = "\u00e8",
  "[ec]" = "\u00ea"
)

. <- function(text) {
  for (key in names(accent)) {
    text <- gsub(key, accent[[key]], text, fixed = TRUE)
  }
  return(text)
}