get_raw_text <- function(html, xpath) {
  nodes <- rvest::html_nodes(html, xpath = xpath)
  text  <- rvest::html_text(nodes, trim = TRUE)
  text
}