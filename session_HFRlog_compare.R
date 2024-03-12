
library(reshape2)
library(ggplot2)
library(ggbeeswarm)


loglist <- c("C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-02-12_history.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-04_history.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-04_history_leo.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-06_history_flamstar.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-06_historyLEO.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-07_historyflamstar.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-07_historyleo.csv")




loglist <- c("C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-02-12_history.csv",
              "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-04_history.csv",
              "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-06_history_flamstar.csv",
              "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-03-07_historyflamstar.csv")

frames_dir <- "C:/Astrophotography/Data/M9ASTRO/2024-03-FLAMSTAR/"



####=======================================================####

names(loglist) <- paste0("SESSION_",regmatches(loglist,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",loglist)))
frames_files <- gsub(".+/","",list.files(frames_dir, recursive = T, pattern = "\\.fits"))



####=======================================================####


longtable <- NULL

for (session_id in names(loglist)) {
  
  logdata <- read.csv(loglist[session_id])
  
  ### filter with framedir files
  logdata$used <- logdata$Filename %in% frames_files
  
  ### arcsec RMS
  logdata$Rms.ArcSec <- logdata$Rms.Total * logdata$Rms.Scale
  
  ### Keep only interesting columns
  logdata <- logdata[,c("Temperature", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.ArcSec", "used")]
  
  ### Melt and add
  longtable <- rbind(longtable, cbind(session = session_id,melt(logdata, id.vars = "used")))
  
}


filter_out_vars <- c("Mean", "StDev", "MAD")

ggplot(longtable[(longtable$used & !(longtable$variable %in% filter_out_vars)),], 
       aes(x=session, y=value, col = variable)) +
  #geom_boxplot() +
  #geom_beeswarm(method = "swarm") +
  #geom_jitter() +
  geom_quasirandom(alpha = 0.6) +
  #stat_summary(fun.y=median, geom="point", shape=18, size=3, color="black", fill="black") +
  geom_boxplot(col = "black", alpha = 0) +
  facet_grid(rows = vars(variable), scales = "free") +
  xlab("") +
  ylab("") +
  theme_minimal() + 
  theme(strip.background = element_rect(fill="grey"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid', 
                                        colour = "grey"),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 1),
        legend.position = "none")





median_agg <- dcast(longtable[longtable$used,], variable ~ session, fun.aggregate = median)











