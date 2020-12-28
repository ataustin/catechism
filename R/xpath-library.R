get_xpath <- function(component) {
  switch(component,
         "page_divisions" = "//ul/li/text()",
         "text_body"      = "//p/descendant::text()[not(parent::b)]",
         "text_quotation" = "//p[@style='margin-left:35.4pt']/descendant::text()",
         "footnotes"      = "/html/body/font/descendant::text()")
}