
####### This is a script to cull subframes based on NINA HFR export

library(reshape2)
library(ggplot2)

######################################
######################################
###### INPUT

project_name <- "eveil"

loglist <- c("C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-07-13_history.csv",
             "C:/Astrophotography/Data/M9ASTRO/NINA_HFR_logs/2024-07-14_history.csv")

frames_dir <- "C:/Astrophotography/Data/M9ASTRO/"
crap_dir <- paste0("C:/Astrophotography/crap_frames/",project_name)
plot_dir <- paste0("./",project_name)

######################################
######################################
###### DIRS

if(!dir.exists(crap_dir)){dir.create(crap_dir)}
if(!dir.exists(plot_dir)){dir.create(plot_dir)}

######################################
######################################
###### FUNCTIONS

get_wide_table <- function(loglist, frames_dir){
  
  names(loglist) <- paste0("SESSION_",regmatches(loglist,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",loglist)))
  frames_files <- gsub(".+/","",list.files(frames_dir, recursive = T, pattern = "\\.fits"))
  
  longtable <- NULL
  widetable <- NULL
  
  for (session_id in names(loglist)) {
    
    logdata_raw <- read.csv(loglist[session_id])
    ### filter with framedir files
    logdata_raw$used <- logdata_raw$Filename %in% frames_files
    ### arcsec RMS
    logdata_raw$Rms.ArcSec <- logdata_raw$Rms.Total * logdata_raw$Rms.Scale
    ### Keep only interesting columns
    logdata <- logdata_raw[,c("Filename","Temperature", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.ArcSec", "used")]
    
    ### Add session
    logdata$session <- session_id
    
    ### Add to wide
    widetable <- rbind(widetable, logdata)
    

    
  }
  
  return(widetable)
}
plot_boxes <- function(long_table){
  session_boxplots <- ggplot(long_table,aes(x=session, y=value, col = variable)) +
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
  
  return(session_boxplots)
  
}
plot_lines <- function(wide_table){
  
  chron_long <- melt(wide_table[wide_table$used == T,c("Filename","Temperature", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.ArcSec")], id.vars = "Filename")
  chron_long$value <- as.numeric(as.character(chron_long$value))
  chron_plot_filtered <- ggplot(chron_long, aes(x=Filename, y=value, group=variable)) +
    geom_line() +
    facet_grid(rows = vars(variable), scales = "free") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  
  return(chron_plot_filtered)
}

######################################
######################################
###### SESSION COMPARISON

wide_table <- get_wide_table(loglist, frames_dir)
long_table <- melt(wide_table[wide_table$used == T,c("session","Temperature", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.ArcSec", "used")], id.vars = "session")
session_boxplots <- plot_boxes(long_table[(!(long_table$variable %in% c("Mean", "StDev", "MAD"))),])
ggsave(session_boxplots, device = "png", width = 6, height = 10, path = plot_dir, filename = "session_raw_boxplots.png", bg = "white")

session_boxplots



##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

###### FILTERING WORKSPACE

filtered_table <- wide_table

##
filtered_table$used[filtered_table$HFR > 2.6] <- FALSE
filtered_table$used[filtered_table$Median > 3800] <- FALSE
filtered_table$used[filtered_table$Stars <= 1053] <- FALSE
filtered_table$used[filtered_table$Rms.ArcSec > 0.8] <- FALSE


sort(filtered_table[filtered_table$used == T,]$Stars)[19]


##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

long_filtered_table <- melt(filtered_table[filtered_table$used == T,c("session","Temperature", "HFR", "Stars", "Median", "Mean", "StDev", "MAD", "Rms.ArcSec")], id.vars = "session")
filtered_boxplots <- plot_boxes(long_filtered_table[(!(long_filtered_table$variable %in% c("Mean", "StDev", "MAD"))),])
ggsave(filtered_boxplots, device = "png", width = 6, height = 10, path = plot_dir, filename = "session_filtered_boxplots.png", bg = "white")

filtered_boxplots

table(filtered_table$used)

######################################
######################################



plot_lines(filtered_table)
plot_lines(wide_table)


####=========================================================================================================#####
####=========================================================================================================#####
####=========================================================================================================#####
####=========================================================================================================#####


crapframes <- filtered_table[filtered_table$used == F,"Filename"]

movecrapframes <- function(frames_dir, crapframes){
  
  for (crapframe in crapframes) {
    
    ## find it
    crapfile <- list.files(path = frames_dir, recursive = T, pattern = paste0("^",crapframe,"$"), full.names = T)
    crapfile_short <- gsub("^.+/","",crapfile)
    
    ## move it
    file.rename(from=crapfile,
                to=file.path(crap_dir, crapfile_short))
  }
  
  
}

#movecrapframes(frames_dir, crapframes)


