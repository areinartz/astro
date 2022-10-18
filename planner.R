



library(ggplot2)
##### 


######## https://github.com/mattiaverga/OpenNGC


###### INPUT

observer_latitude <- "51:01:19"
observer_longitude <- "03:42:50"


#######



NGC_cat <- read.csv("./NGC_catalog/NGC.csv", header = T, sep = ";")
Messier_cat <- NGC_cat[!is.na(NGC_cat$M),]


####### FUNCTIONS


base60_to_decimal <- function(dec_val){
  dec_split <- do.call(rbind.data.frame, strsplit(dec_val, split = ":"))
  colnames(dec_split) <- c("deg","min","sec")
  dec_split$deg <- as.numeric(as.character(dec_split$deg))
  dec_split$min <- as.numeric(as.character(dec_split$min))
  dec_split$sec <- as.numeric(as.character(dec_split$sec))
  decimal_degrees <- dec_split[,1] + dec_split[,2]/60 + dec_split[,3]/3600
  return(decimal_degrees)
}



plot_session_visibility <- function(catalog_extract, 
                                    south_RA_at_start, 
                                    south_RA_at_end,
                                    observer_latitude,
                                    object_label_col = "M"){
  
  
  RA_south_dusk <- south_RA_at_start
  RA_south_dawn <- south_RA_at_end
  
  if(RA_south_dawn < RA_south_dusk){RA_south_dawn <- RA_south_dawn+24}
  
  ### clean catalog extract
  catalog_extract <- catalog_extract[!is.na(catalog_extract$Dec),]
  catalog_extract <- catalog_extract[catalog_extract$Dec != "",]
  
  ### convert degs and hours to decimals
  catalog_extract$decimal_dec <- base60_to_decimal(catalog_extract$Dec)
  catalog_extract$RA_dec <- base60_to_decimal(catalog_extract$RA)
  obs_lat <- base60_to_decimal(observer_latitude)
  
  #### repeat the RA + 1 period, so we always have a nice window
  RAreps <- catalog_extract
  RAreps$RA_dec <- RAreps$RA_dec + 24
  RAreps2 <- catalog_extract
  RAreps2$RA_dec <- RAreps2$RA_dec - 24
  plottable <- rbind(RAreps2, catalog_extract, RAreps)
  
  #### set the label column
  plottable$labs <- plottable[,object_label_col]
  

  #### functions for visibility lines: sine wave approximations of sphere cross section
  #### note that this is only accurate in mercator projection and not in equirectangular as is the case here
  #### It is a sufficient approximation though
  term_dusk <- function(x){(90-obs_lat)*sin(2*pi*(x+18-RA_south_dusk)/24)}
  term_dawn <- function(x){(90-obs_lat)*sin(2*pi*(x+18-RA_south_dawn)/24)}
  
  ### 
  visibility_lines_color <- "#1E374A"
  
  #### GGPLOT OBJECT
  sessionplot <- ggplot(plottable, aes(x=RA_dec, y=decimal_dec, col=Type)) +
    
    ##### Minimal declination from observer
    geom_hline(yintercept = -90+obs_lat, col="red") +
    
    # repeat indicators
    geom_vline(xintercept = 0, size = 1, linetype = "dashed") +
    geom_vline(xintercept = 24, size = 1, linetype = "dashed") +
    geom_vline(xintercept = -24, size = 1, linetype = "dashed") +
    geom_vline(xintercept = 48, size = 1, linetype = "dashed") +
    
    ##### Objects
    geom_point() +
    geom_text(aes(label=labs),nudge_y = 2, size=3) +
    
    ##### Draw visibility margins for session
    # before session and session start
    stat_function(fun=function(x){90-obs_lat}, size = 1.5, xlim = c(-24, RA_south_dusk-12), col=visibility_lines_color) +
    stat_function(fun=term_dusk, xlim = c(RA_south_dusk-12,RA_south_dusk), size = 1.5, col=visibility_lines_color) +
    stat_function(fun=term_dusk, xlim = c(RA_south_dusk, RA_south_dusk+12), size = 1, linetype = "longdash", col=visibility_lines_color) +
    stat_function(fun=function(x){90-obs_lat}, xlim = c(RA_south_dusk+12, 48), size = 1, linetype = "longdash", col=visibility_lines_color) +
    # in between
    stat_function(fun=function(x){-90+obs_lat}, size = 1.5, xlim = c(RA_south_dusk, RA_south_dawn), col=visibility_lines_color) +
    # session end and after
    stat_function(fun=function(x){90-obs_lat}, xlim = c(-24,RA_south_dawn-12), size = 1, linetype = "longdash", col=visibility_lines_color) +
    stat_function(fun=term_dawn, xlim = c(RA_south_dawn-12,RA_south_dawn), size = 1, linetype = "longdash", col=visibility_lines_color) +
    stat_function(fun=term_dawn, xlim = c(RA_south_dawn,RA_south_dawn+12), size = 1.5, col=visibility_lines_color) +
    stat_function(fun=function(x){90-obs_lat}, size = 1.5, xlim = c(RA_south_dawn + 12, 48), col=visibility_lines_color) +

    # y axis stuff
    scale_y_continuous(limits = c(-95,95), breaks = seq(-90,90,by=10)) +
    ylab("Declination (degrees)") +
    # x axis stuff
    scale_x_continuous(limits = c(-24,48), breaks = -24:47, labels = rep(0:23,3)) +
    xlab("Right ascension (h)") +
    
    #### Theme etc
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  
  #geom_hline(yintercept = 90-obs_lat) +
  #geom_vline(xintercept = RA_south_dawn) +
  #geom_vline(xintercept = RA_south_dusk) +
  #geom_point(x=RA_south_dawn,y=-90+obs_lat, col="red", size=3) +
  #geom_point(x=RA_south_dusk,y=-90+obs_lat, col="red", size=3) +
  
    return(sessionplot)
}


plot_session <- function(catalog_extract,
                         current_date = Sys.Date(),
                         night_length_h = 8,
                         observer_latitude = "51:01:19",
                         object_label_col = "M"){
  
  #### Convert date to south RA start and end
  decimal_month <- as.numeric(gsub("^[0-9]{4}-|-[0-9]{2}$","",current_date)) + as.numeric(gsub(".+-","",current_date))/30.5
  midnight_south_RA <- 4+(2*decimal_month)
  if(midnight_south_RA >= 24){midnight_south_RA <- midnight_south_RA - 24}
  south_RA_at_start <- midnight_south_RA - night_length_h/2
  south_RA_at_end <- midnight_south_RA + night_length_h/2
  
  #### Call the general function
  sessionplot <- plot_session_visibility(catalog_extract = catalog_extract,
                                         south_RA_at_start = south_RA_at_start, 
                                         south_RA_at_end = south_RA_at_end, 
                                         observer_latitude = observer_latitude,
                                         object_label_col = object_label_col)
  
  return(sessionplot)
  
}



######## EXAMPLES

plot_session_visibility(catalog_extract = NGC_cat[NGC_cat$Type %in% c("G","GGroup","GPair","PN") & NGC_cat$Common.names != "",],
                        south_RA_at_start = 19, 
                        south_RA_at_end = 21, 
                        observer_latitude = observer_latitude,
                        object_label_col = "Common.names")


#ggsave("sessionplot_commonname_G_PN.png", height = 12, width = 24, device = "png", bg = "white")


plot_session_visibility(catalog_extract = NGC_cat[NGC_cat$Type %in% c("GCl") & NGC_cat$V.Mag < 7,],
                        south_RA_at_start = 19, 
                        south_RA_at_end = 21, 
                        observer_latitude = observer_latitude,
                        object_label_col = "M")



plot_session(catalog_extract = NGC_cat[!is.na(NGC_cat$M),])




