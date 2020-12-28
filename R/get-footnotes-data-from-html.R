get_footnotes_data_from_html <- function(html) {
  footnotes_raw   <- get_raw_text(html, get_xpath("footnotes"))
  page_divisions  <- get_raw_text(html, get_xpath("page_divisions"))
  footnotes_clean <- clean_raw_footnotes(footnotes_raw, page_divisions)
  footnotes_data  <- convert_footnotes_to_data(footnotes_clean)
  footnotes_data
}


clean_raw_footnotes <- function(raw_footnotes, page_divisions) {
  no_blank     <- raw_footnotes[nchar(raw_footnotes) > 0]
  no_divisions <- no_blank[!(no_blank %in% page_divisions)]
  no_arrow     <- no_divisions[!grepl(get_arrow_unicode(), no_divisions)]
  no_newline   <- gsub("\\s", " ", no_arrow)
  single_space <- gsub("  ", " ", no_newline)
  single_space
}


convert_footnotes_to_data <- function(footnotes) {
  is_marker    <- grepl("^[0-9]{1,3}$", footnotes)
  is_footnote  <- !is_marker
  marker_index <- which(is_marker)
  group_index  <- cumsum(is_marker)

  footnote_data <- tibble::tibble(footnotes,
                                  is_footnote,
                                  group_index)
  footnote_data %>%
    dplyr::filter(is_footnote) %>%
    dplyr::group_by(group_index) %>%
    dplyr::summarize(footnote = paste(footnotes, collapse = " ")) %>%
    dplyr::mutate(marker = as.integer(footnotes[is_marker])) %>%
    dplyr::select(marker, footnote)
}

