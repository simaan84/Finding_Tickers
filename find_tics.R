library(rvest)

rm(list = ls())

get_tics <- function(theurl) {
  file <- read_html(theurl)
  tables <- html_nodes(file, "table")

  # try extract each into a data.frame
  tables.list <- numeric()
  for (i in 1:length(tables)) { 
    tables.list[i] <- list(try(html_table(tables[i], fill = TRUE),silent = T))
  }

  # the table we are looking should have columns for volume and price: 
  find.volume <- lapply(tables.list, function(x) grep("Volume",x,ignore.case = T)   )
  find.price <- lapply(tables.list, function(x) grep("Price",x,ignore.case = T)   )
  locate.table <- which.max(sapply(find.volume, length) + sapply(find.price, length) )
  
  ds <- tables.list[[locate.table]][[1]]
  names(ds) <- ds[1,]
  ds <- ds[-1,]

  return(ds)
}

# scrap the data from online
theurl1 <- "http://www.wsj.com/mdc/public/page/2_3021-activnyse-actives.html"
ds1 <- get_tics(theurl1)
tics <- sapply(strsplit(ds1[,2]," "),function(x) x[length(x)]) 
tics <- gsub("\\(","",tics)
tics <- gsub("\\)","",tics)
cat("The most traded tickers are\n", tics[1:10],"\n")

# the same function can be applied to the top gainers table
theurl2 <- "http://www.wsj.com/mdc/public/page/2_3021-gainnyse-gainer.html?mod=mdc_uss_pglnk"
ds2 <- get_tics(theurl2)
head(ds2)

