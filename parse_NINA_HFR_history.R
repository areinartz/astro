


library(reshape2)
library(ggplot2)


#### script to parse and plot NINA HFR history file


hfrhist_file <- "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-04-09_history.csv"

hfrhist <- read.csv(hfrhist_file)

colnames(hfrhist)

QC_metrics <- hfrhist[,c("Id", "Filename", "dateTime", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.Ra", 
                "Rms.Dec", "Rms.Total", "Temperature", "Focuser.Position")]


molten_QC <- melt(QC_metrics, id.vars = c("Id", "Filename","dateTime"))
molten_QC$value <- as.numeric(as.character(molten_QC$value))


ggplot(molten_QC, aes(x=Filename, y=value, group=variable)) +
  geom_line() +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank())
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  


ggplot(molten_QC[molten_QC$variable %in% c("HFR","Stars","Median","Rms.Total", "Temperature", "Focuser.Position"),], aes(x=Filename, y=value, group=variable)) +
  geom_line() +
  facet_grid(rows = vars(variable), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) + xlab("")
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





############# Temp compensation correlation


## Make linear regression model

model <- lm(Focuser.Position~HFR, data=QC_metrics)
summary(model)

ggplot(QC_metrics, aes(x=Temperature, y=Focuser.Position)) +
  geom_point(aes(size=HFR, col=HFR), alpha = 0.8) +
  geom_smooth(method='lm') +
  xlab("Temperature (°C)") +
  ylab("Focuser position") +
  scale_color_gradient(low = "green", high = "red") +
  theme_minimal()




#### best quantile subset

best_pct = 0.05

quan <- quantile(QC_metrics$HFR, probs = best_pct)

bestsubset <- QC_metrics[QC_metrics$HFR <= quan,]


ggplot(QC_metrics, aes(x=Temperature, y=Focuser.Position)) +
  geom_smooth(method='lm') +
  geom_smooth(data = bestsubset, method = "lm", col = "red") +
  geom_point(aes(size=HFR, col=HFR), alpha = 0.8) +
  xlab("Temperature (°C)") +
  ylab("Focuser position") +
  scale_color_gradient(low = "green", high = "red") +
  theme_minimal()


