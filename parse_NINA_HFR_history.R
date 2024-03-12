


library(reshape2)
library(ggplot2)


#### script to parse and plot NINA HFR history file


hfrhist_file <- "~/N.I.N.A/2023-12-17_history.csv"

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


##### move



ggplot(molten_QC[molten_QC$variable %in% c("Stars"),], aes(x=Filename, y=value, group=variable)) +
  geom_line() +
  geom_hline(yintercept = 230) +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

table(QC_metrics$Stars < 230)

a <- QC_metrics[QC_metrics$Stars < 230,1:3]
a <- a[order(a$Filename),]
a <- a[33:nrow(a),]





