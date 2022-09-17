library(dplyr)
library(rvest)
library(httr)
webpage <- rvest::read_html("https://www.thelancet.com/journals/lancet/issues")


session <- rvest::session("https://www.thelancet.com/journals/lancet/issues")

session %>%
  session_follow_link(i = "")
  


scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

test <- scraplinks("https://www.thelancet.com/journals/lancet/issues")
