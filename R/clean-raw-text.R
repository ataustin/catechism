clean_raw_text <- function(raw_text) {
  no_blank     <- raw_text[nchar(raw_text) > 0]
  no_newline   <- gsub("\\s", " ", no_blank)
  single_space <- gsub("  ", " ", no_newline)
  single_space
}
