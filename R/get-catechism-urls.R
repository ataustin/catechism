get_all_urls <- function(index = "http://www.vatican.va/archive/ENG0015/_INDEX.HTM") {
  html     <- xml2::read_html(index)
  nodes    <- rvest::html_nodes(html, xpath = "//a/@href")
  contents <- xml2::xml_contents(nodes)
  suffixes <- as.character(contents)
  filtered <- suffixes[grepl("__P", suffixes)]
  urls     <- vapply(filtered, make_url, character(1))

  urls
}


make_url <- function(suffix, base = "http://www.vatican.va/archive/ENG0015") {
  URLencode(paste0(base, "/", suffix))
}
