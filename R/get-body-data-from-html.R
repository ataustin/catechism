get_body_data_from_html <- function(html) {
  text_raw   <- get_raw_text(html, xpath = get_xpath("text_body"))
  text_quote <- get_raw_text(html, xpath = get_xpath("text_quotation"))
  
  raw_clean   <- clean_raw_text(text_raw)
  quote_clean <- clean_raw_text(text_quote)

  body_data <- convert_body_to_data(raw_clean)
  body_data$is_quotation <- body_data$text %in% quote_clean
  
  data_clean  <- clean_body_data(body_data)
  data_clean
}


convert_body_to_data <- function(text) {
  is_superscript    <- grepl("^[0-9]{1,4}$", text)
  superscript_index <- which(is_superscript)
  has_marker        <- superscript_index - 1
  group_index       <- cumsum(!is_superscript)

  marker <- character(max(group_index))
  marker[group_index[has_marker]] <- text[superscript_index]

  col_text <- text[!is_superscript]
  number   <- as.integer(stringr::str_extract(col_text, "^[0-9]{0,4}"))

  text_data <- tibble::tibble(text   = col_text,
                              marker = as.integer(marker),
                              number = number)

  text_data
}


clean_body_data <- function(text_data) {
  text_data      <- tidyr::fill(text_data, number)
  text_data$text <- gsub("^[0-9]{1,4} ", "", text_data$text)
  text_data      <- select(text_data, number, text, is_quotation, marker)
  text_data
}