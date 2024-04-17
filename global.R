library(shiny)
library(shinydashboard)
library(idiogramFISH)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(ggnetwork)

rdsFiles <- list.files("rds", full.names = T, pattern = ".*rds$")
rdsObject <- gsub("rds\\/|.rds", "", rdsFiles)
objectList <- lapply(rdsFiles, readRDS)
names(objectList) <- rdsObject

plotwidth <- 11.5
plotheight <- 11.5
divisor <- 70
width <- plotwidth * divisor
height <- plotheight * divisor

inputTitle <- "name in this study / molecular study"

library(bib2df)
bibList <- list.files("bib", full.names = T)
mybibdf <- list()
invisible(tryCatch(lapply(seq_along(bibList), function(x) mybibdf[[x]] <<- bib2df(bibList[x])), error = function(e) {
  cat("not found")
}))
mybibdf <- dplyr::bind_rows(mybibdf)

if (exists("mybibdf")) {
  colnames(mybibdf) <- tolower(colnames(mybibdf))
  mybibdf <- as.data.frame(mybibdf, stringsAsFactors = F)
  for (i in 1:nrow(mybibdf)) {
    for (j in 1:ncol(mybibdf)) {
      mybibdf[[i, j]] <- paste0(unlist(mybibdf[[i, j]]), collapse = "; ")
      mybibdf[[i, j]] <- (gsub("(\\{|\\})", "", mybibdf[[i, j]]))
      mybibdf[[i, j]] <- gsub("'", "", mybibdf[[i, j]])
      mybibdf[[i, j]] <- gsub("(\\^)", "", mybibdf[[i, j]])
      mybibdf[[i, j]] <- gsub("\\\\", "", mybibdf[[i, j]])
      mybibdf[[i, j]] <- gsub("\\-\\-", " - ", mybibdf[[i, j]])
    }
  }
  mybibdf[mybibdf == "NA"] <- NA
}

source("functions.R")

mybibdf$url <- ifelse(is.na(mybibdf$url),
  ifelse(!is.na(mybibdf$doi), paste0("htttps://doi.org", mybibdf$doi), mybibdf$url),
  mybibdf$url
)

listOfRefs <- list()
for (i in 1:nrow(mybibdf)) {
  listOfRefs <- AnyItemIfTitle(c(
    "author", "year", "title", "journal",
    "editor",
    "booktitle",
    "volume", "pages"
  ), mybibdf, 1, booltitle = F)
}

refStrings <- paste(
  unlist(lapply(listOfRefs, function(x) paste(x, collapse = ""))),
  ifelse(is.na(mybibdf$url), "", '\n<a href="'),
  ifelse(is.na(mybibdf$url), "", mybibdf$url),
  ifelse(is.na(mybibdf$url), "", '">'),
  ifelse(is.na(mybibdf$url), "", mybibdf$url),
  ifelse(is.na(mybibdf$url), "", "</a>")
)
names(refStrings) <- mybibdf$bibtexkey
