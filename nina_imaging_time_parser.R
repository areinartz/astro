



#### This is a script to parse NINA captures folder to obtain time spent imaging

library(ggplot2)



## Input folders with exposures

NINA_captures.dirs <- c("~/../Desktop/NINA_captures/","~/../Desktop/NINA_captures/")


#NINA_captures.dir <- "~/../Desktop/NINA_captures/"

## Read and parse

all_files <- NULL

for (NINA_captures.dir in NINA_captures.dirs) {
  dirfiles <- list.files(NINA_captures.dir, include.dirs = F, recursive = T, full.names = F)
  all_files <- c(all_files, gsub(".+/","",dirfiles))
}

unique_files <- unique(all_files)
patternmatch_files <- unique_files[grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}.+\\.fits",unique_files)]


length(all_files)
length(unique_files)
length(patternmatch_files)


## split filenames into columns of data frame
nina_table <- data.frame(do.call(rbind, strsplit(patternmatch_files, "_")), stringsAsFactors = F)[,-3]
colnames(nina_table) <- c("Date", "Time", "Temp", "Exp", "Number")


## Clean columns
nina_table$Date <- as.Date(nina_table$Date)
nina_table$Time <- gsub("-",":",nina_table$Time)
nina_table$Exp <- as.numeric(gsub("s","",nina_table$Exp))



## Per date aggregate

perdate <- aggregate(nina_table, Exp ~ Date, FUN = "sum")



ggplot(perdate, aes(x = Date, y = Exp)) +
  geom_bar(stat = "identity", fill = "blue")


