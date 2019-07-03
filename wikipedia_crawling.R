library(rvest)
library(tidyverse)


# Actual web crawling -----------------------------------------------------


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
# This might take forever and eventually search the whole wikipedia. Hence, try with small number of urls first.
# Squirrel has 509 neighbours. Hence, crawl those first, by putting i < 509 in the while condition.
# This left us with a to_be_checked_urls vector of length 66517. To crawl the two-step neighbourhood of 
# Squirrel, now put i < 66517 into the while condition.
# This leaves us with an url_dictionary of 26655 rows, i.e. this is the number of articles we actually crawled,
# and an edges dataframe with 7 518 388 rows. This is the number of edges in the THREE-step neighbourhood
# of Squirrel. Three-step, because it also includes edges from the nodes at distance two further out. However,
# we have not yet crawled those websites at distance three, i.e. these urls are not yet in the url_dictionary and
# we do not yet know the topic of those pages.


i <- 0
while (i < 66517) {
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
      # this allows us to keep track of progress without spamming the console
      cat(paste("i =", i, ",", url), file = "checked_urls.txt", append = TRUE)
      
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
        # we are extracting relative urls, to make them useable for read_html we need to add the following
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
      Sys.sleep(1)
    }else{
      wrong_urls <- c(wrong_urls, url)
      checked_urls <- c(checked_urls, url)
      to_be_checked_urls <- to_be_checked_urls[-1]
      print(paste("WRONG URL", url))
    }
    
  }
}
