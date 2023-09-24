


library(reshape2)
library(ggplot2)


#### script to parse and plot NINA HFR history file


hfrhist_file <- "~/N.I.N.A/2023-09-23_history.csv"

hfrhist <- read.csv(hfrhist_file)

colnames(hfrhist)

QC_metrics <- hfrhist[,c("Id", "Filename", "dateTime", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.Ra", 
                "Rms.Dec", "Rms.Total")]


molten_QC <- melt(QC_metrics, id.vars = c("Id", "Filename","dateTime"))
molten_QC$value <- as.numeric(as.character(molten_QC$value))


ggplot(molten_QC, aes(x=Filename, y=value, group=variable)) +
  geom_line() +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  


ggplot(molten_QC[molten_QC$variable %in% c("HFR","Stars"),], aes(x=Filename, y=value, group=variable)) +
  geom_line() +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))




ordr <- QC_metrics[order(QC_metrics$Stars),]
