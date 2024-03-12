



#### This is a script to parse NINA captures folder to obtain time spent imaging

library(ggplot2)
library(reshape2)


## Input folders with exposures

NINA_captures.dirs <- c("~/../Desktop/NINA_captures/","c:/Astrophotography/Data/M9ASTRO/", "D:/Astrophotography/Data/NINA_captures/")


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
  geom_bar(stat = "identity", fill = "#283ead", col = "#283ead")


perdate$Exp_h <- perdate$Exp / 3600
perdate$cumulative_h <- cumsum(perdate$Exp)/3600


ggplot(perdate, aes(x = Date, y = Exp_h)) +
  geom_area(aes(y = cumulative_h), alpha = 0.1, col = "black") +
  geom_bar(stat = "identity", fill = "#283ead", col = "#283ead") +
  ylab("Hours of exposure") +
  scale_x_date(date_breaks = "week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



molten <- melt(perdate[,c("Date","Exp_h","cumulative_h")], id.vars = "Date")
molten$value <- as.numeric(as.character(molten$value))
molten$variable <- as.character(molten$variable)
molten$variable[molten$variable == "Exp_h"] <- "per_day"
molten$variable[molten$variable == "cumulative_h"] <- "cumulative"

ggplot(molten, aes(x = Date, y = value)) +
  geom_area(data = molten[molten$variable == "cumulative",],alpha = 0.1, col = "black", fill = "darkblue") +
  geom_bar(data = molten[molten$variable == "per_day",], stat = "identity", fill = "#283ead", col = "#283ead") +
  facet_grid(rows = vars(variable), scales = "free") +
  ylab("Hours of exposure") +
  scale_x_date(date_breaks = "month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


