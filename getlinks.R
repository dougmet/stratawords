library(httr)

ga <- secure::decrypt("googleapi")

makeSearch <- function(x) {
  searchtext <- paste0("Apache%20", x)
  query=paste0("https://www.googleapis.com/customsearch/v1?key=", ga$apikey, "&cx=",
               ga$engineid,"&q=", searchtext)
  resp <- GET(query)
  resp
}
resps <- lapply(apache, makeSearch)
names(resps) <- apache

rm(ga)

save(resps, file = "responses.rda")

links <- vapply(resps, function(x) content(x)$items[[1]]$link, c(a="a"))
projectinfo <- data.frame(Project = names(links), Link = links, row.names = NULL, stringsAsFactors = FALSE)
write.csv(projectinfo, file = "projectinfo.csv", row.names = FALSE)
