library(rvest)
setwd("~/Documents/Blogs/Strata/")


# Functions ---------------------------------------------------------------


getAnchor <- function(x, div) {
  x %>% html_nodes(paste0("div.", div)) %>%
    html_nodes("a") %>% html_text()
}

getDiv <- function(x, div) {
  x %>% html_nodes(paste0("div.", div)) %>% html_text()
}


getEarlyRecord <- function(sdiv, fixed = NULL) {
  
  dat <- list(title = getAnchor(sdiv, "en_session_title"),
              speakers = getAnchor(sdiv, "en_session_speakers"),
              description = getDiv(sdiv, "en_session_description"))

  c(dat, fixed)
}


getSessions <- function(x, year) {
  lapply(html_nodes(x, "div.en_session"), getEarlyRecord, list(year=year))
}


getLateRecord <- function(sdiv, fixed = NULL) {
  
  dat <- list(title = getAnchor(sdiv, "session_name"),
              topic = getAnchor(sdiv, "topics"),
              speakers = getAnchor(sdiv, "en_speakers"),
              description = getDiv(sdiv, "en_description"))
  
  c(dat, fixed)
}


# Years 2012 - 2014 -------------------------------------------------------

earlyStrata <- list("2012" = read_html("html/strata2012-10-01.html"),
                    "2013" = read_html("html/strata2013-11-11.html"),
                    "2014" = read_html("html/strata2014-11-02.html"))

early <- list()
for (year in c("2012", "2013", "2014")) {
  early[[year]] <- getSessions(earlyStrata[[year]], year)
  early[[year]] <- early[[year]][sapply(early[[year]], function(x) length(x$title) > 0)]
}

# Years 2015 - 2016 -------------------------------------------------------

late <- list()
for (year in c("2015", "2016")) {
  strata <- lapply(list.files(path = "html", pattern = paste0(year, "-*"), full.names = TRUE),
                   read_html)
  sessions <- lapply(strata, function(x) html_nodes(x, "div.en_session"))
  late[[year]] <- do.call("c", lapply(sessions, function(x) lapply(x, getLateRecord, fixed = list(year = year))))
}

# Clean up
# Must have a title
for (year in names(late)) {
  late[[year]] <- late[[year]][sapply(late[[year]], function(x) length(x$title) > 0)]
}


# Combine -----------------------------------------------------------------

strata <- c(early, late)

writeLines(toJSON(strata, pretty = TRUE, auto_unbox = TRUE), "strata.json")