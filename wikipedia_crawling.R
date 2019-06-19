library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/Squirrel"

page <- read_html(url)
name <- html_text(html_node(page, css = "#firstHeading"))


links_body <- page%>%html_node(css = "#bodyContent")%>%
  html_nodes("a")%>%
  html_attr("href")%>%
  str_match(pattern = "/wiki/[[:alnum:][:punct:]]+")%>%
  subset(str_detect(string = ., pattern = ":", negate = TRUE))%>%
  as.character()%>%
  unique()%>%
  paste0("https://en.wikipedia.org", .)

to_be_checked_urls <- links_body
checked_urls <- url
error_urls <- character()

url_dictionary <- data.frame(name = name, url = url)
edges <- data.frame(From = name, from_url = url, to_url = links_body, To = NA)


# PUT THIS INTO WHILE: length(to_be_checked_urls) > 0
# for now, only try with small set

# KEEP TRACK OF HOW MUCH WE CRAWLED
#  10
# 140

i <- 0
while (i < 140) {
  i <- i + 1
  url <- to_be_checked_urls[1]
  if(sum(checked_urls == url) > 0){
    to_be_checked_urls <- to_be_checked_urls[-1]
    print(paste("ALREADY CHECKED", url))
  }else{
    
    # only process actual http requests
    if(str_detect(url, pattern = "http")){
      tryCatch({page <- read_html(url)}, 
               error = function(e) {print(paste("URL ERROR FOR", url)); 
                 error_urls <- c(error_urls, url);
                 to_be_checked_urls <- to_be_checked_urls[-1]})
      print(paste("i =", i, ","))
      
      # actual scraping
      name <- html_text(html_node(page, css = "#firstHeading"))
      url_dictionary <- rbind(url_dictionary,
                              data.frame(name = name,
                                         url = url))
      
      links_body <- page%>%html_node(css = "#bodyContent")%>%
        html_nodes("a")%>%
        html_attr("href")%>%
        str_match(pattern = "/wiki/[[:alnum:][:punct:]]+")%>%
        subset(str_detect(string = ., pattern = ":", negate = TRUE))%>%
        as.character()%>%
        unique()%>%
        paste0("https://en.wikipedia.org", .)
      
      # add new links to edge list
      if(length(links_body) > 0){
        edges <- rbind(edges,
                       data.frame(From = name,
                                  from_url = url, 
                                  to_url = links_body,
                                  To = NA))
      }
      #setdiff also removes duplicates
      new_urls <- setdiff(links_body, checked_urls)
      to_be_checked_urls <- c(to_be_checked_urls[-1], new_urls)
      checked_urls <- c(checked_urls, url)
      Sys.sleep(2)
    }else{
      wrong_urls <- c(wrong_urls, url)
      checked_urls <- c(checked_urls, url)
      to_be_checked_urls <- to_be_checked_urls[-1]
      print(paste("WRONG URL", url))
    }
    
  }
}




