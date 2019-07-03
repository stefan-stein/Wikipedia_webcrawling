library(tidyverse)
library(igraph)
library(diagram)

# Post-processing ---------------------------------------------------------

# load crawled data
load("two-step.RData")

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

# fill in entries of the To column and count how often these edges appeared
weighted_two_step_edges <- edges%>%left_join(url_dictionary, by = c("to_url" = "url"))%>%
  select(-To)%>%
  setNames(c(names(edges)[1:3], "To"))%>%
  filter(!is.na(To))%>%
  group_by(From, To)%>%
  summarise(Weight = n())%>%
  ungroup()

# List of unique topics in the two step neighbourhood of Squirrel: 22 524 in total
node_list <- weighted_two_step_edges%>%select(From)%>%distinct(From)%>%
  bind_rows(weighted_two_step_edges%>%select(To)%>%distinct(To)%>%setNames("From"))%>%
  distinct()%>%
  setNames("Node")%>%
  arrange(Node)

# Useful functions --------------------------------------------------------

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
  
  # This works, because we know maximum distance is two
  back_0 <- weighted_two_step_edges%>%filter(From == "Squirrel", To == target)
  # check if there is direct connection from Squirrel to target
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


# graphical version of find_path
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
