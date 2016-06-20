library(dplyr)
library(igraph)
library(stringi)
library(jsonlite)

set.seed(14)

strata <- fromJSON("strata.json", simplifyDataFrame = FALSE)

#svg("allYears.svg")
jpeg("allYears.jpg", width = 2048, height = 3064, res = 388)

op <- par(mar = c(0,0,1,0), mfrow=c(3,2))

for(year in names(strata)) {
#  svg(paste0("apache", year, ".svg"))
  
  titles <- vapply(strata[[year]], `[[`, "a", "title")
  
  descriptions <- vapply(strata[[year]], `[[`, "a", "description")
  
  allApache <- na.omit(stri_match(c(titles, descriptions), regex = "[Aa]pache (\\w+)"))[,2]
  apache <- unique(allApache)
  
  table(allApache)
  
  # Default no edges
  el <- data.frame(from=character(0), to = character(0))
  
  # When edges/nodes are few things can break
  
  if(length(apache) > 1) {
    occur <- sapply(apache, grepl, c(titles, descriptions), ignore.case = TRUE)
    
    occur <- occur[rowSums(occur) > 1, ]
    
    if(nrow(occur) > 0) {
      if (all(rowSums(occur)==2)) {
        el <- as.data.frame(t(apply(occur, 1, function(x) t(combn(apache[x], 2)))),
                            stringsAsFactors = FALSE)
      } else {
        el <- as.data.frame(do.call("rbind", apply(occur, 1, function(x) t(combn(apache[x], 2)))),
                            stringsAsFactors = FALSE)
      }
      names(el) <- c("from", "to")
    }
  }
  
  # I prefre to count edges before going to igraph
  el <- el %>% group_by(from, to) %>% summarise(weight = n())
  # Data frame of vertices
  vl <- data.frame(name = apache, mentions = as.numeric(table(allApache)[apache]))
  
  
  # Build the graph ---------------------------------------------------------
  
  g <- graph_from_data_frame(el, vertices = vl, directed = FALSE)
  
  #library(svglite)
  #svglite(paste0("ApacheNet", year, ".svg"))
  #pdf(paste0("ApacheNet", year, ".pdf"))
  
  plot(g, edge.width=E(g)$weight, vertex.size = 5*V(g)$mentions, main = year)
 
#  dev.off()
  
}
par(op)

dev.off()
