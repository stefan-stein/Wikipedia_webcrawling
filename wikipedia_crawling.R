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
# 359
# This concluded the first step from Squirrel. This left us with a to_be_checked_urls vector of length 66517

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
      #print(paste("i =", i, ",", url))
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



# Post-processing ---------------------------------------------------------

url_dictionary$name <- as.character(url_dictionary$name)
url_dictionary$url <- as.character(url_dictionary$url)
edges$From <- as.character(edges$From)
edges$from_url <- as.character(edges$from_url)
edges$to_url <- as.character(edges$to_url)

# full_edges <- edges%>%left_join(url_dictionary, by = c("to_url" = "url"))%>%
#   select(-To)%>%
#   setNames(c(names(edges)[1:3], "To"))
# two_step_edges <- full_edges%>%filter(!is.na(To))
# weighted_two_step_edges <- two_step_edges%>%group_by(From, To)%>%
#   summarise(Weight = n())

weighted_two_step_edges <- edges%>%left_join(url_dictionary, by = c("to_url" = "url"))%>%
  select(-To)%>%
  setNames(c(names(edges)[1:3], "To"))%>%
  filter(!is.na(To))%>%
  group_by(From, To)%>%
  summarise(Weight = n())%>%
  ungroup()

node_list <- weighted_two_step_edges%>%select(From)%>%distinct(From)%>%
  bind_rows(weighted_two_step_edges%>%select(To)%>%distinct(To)%>%setNames("From"))%>%
  distinct()%>%
  setNames("Node")%>%
  arrange(Node)

# Useful functions --------------------------------------------------------

library(igraph)
library(diagram)

# given a term, find the path from Squirrel to that term
find_path <- function(target){
  # if(nrow(weighted_two_step_edges%>%filter(From == "Squirrel", To == target)) > 0){
  #   weighted_two_step_edges%>%filter(From == "Squirrel", To == target)
  # }else{
    # back_1 <- weighted_two_step_edges%>%filter(To == target)%>%
    #   select(From)%>%distinct(From)
    # weighted_two_step_edges%>%filter(From == "Squirrel", To %in% back_1$From)%>%
    #   select(-Weight)%>%
    #   mutate(End = target)
    
    back_0 <- weighted_two_step_edges%>%filter(From == "Squirrel", To == target)
    if(nrow(back_0) > 0){
      return(back_0)
    }else{
    back_1 <- weighted_two_step_edges%>%filter(To == target)
    back_2 <- weighted_two_step_edges%>%filter(From == "Squirrel", To %in% back_1$From)
    rbind(
          back_2,
          back_1%>%filter(From %in% back_2$To)
          )
    }
  
}


graph_path <- function(target){
  path <- find_path(target)
  if(nrow(path) == 0){print(paste("No valid connection for", target))}
  else{
    g <- graph_from_data_frame(path[,1:2])
    
    if(nrow(path)==1){
      coords = coordinates(pos = c(1,1), hor = F)
      plot(g, layout=coords, vertex.shape = "none",
           vertex.label.font=2, vertex.label.color="gray40",
           vertex.label.cex=.7, edge.color="gray85")
    }else{
      coords <- coordinates(pos = c(1, nrow(path%>%filter(From != "Squirrel")), 1),
                            hor = F)
        plot(g, layout=coords, vertex.shape = "none",
             vertex.label.font=2, vertex.label.color="gray40",
             vertex.label.cex=.7, edge.color="gray85")
      
    }
  }
}
