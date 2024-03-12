

library(ggplot2)

#### This is a script to parse and plot registration info


###################################################
#################### FUNCTIONS ####################
###################################################

regdir_to_list <- function(reg.dir, verbose = FALSE){
  
  info.files <- list.files(reg.dir, pattern = "\\.Info\\.txt$")

  info.list <- list()
  
  for(info.file in info.files){
    
    if(verbose == TRUE){cat(info.file,"\n")}
    
    info.lines <- readLines(file.path(reg.dir, info.file))
    
    ### parse general info
    general.df <- data.frame(do.call('rbind', strsplit(info.lines[1:7], split = " = ")), stringsAsFactors = FALSE)
    colnames(general.df) <- c("property","value")
    row.names(general.df) <- general.df[,1]
    general.df$value <- as.numeric(as.character(general.df$value))
    
    ### parse star list
    
    nrstars <- general.df["NrStars","value"]
    
    if(nrstars > 0){
      
      stars.list <- list()
      starhead.idx <- grep("^Star# = ",info.lines)
      
      if(length(starhead.idx) != nrstars){stop("Different number of stars found than stated in header\n")}
      
      for(starhead.i in starhead.idx){
        star.df <- data.frame(do.call('rbind', strsplit(info.lines[starhead.i:(starhead.i + 6)], split = " = ")), stringsAsFactors = FALSE)
        colnames(star.df) <- c("property","value")
        row.names(star.df) <- star.df[,1]
        starname <- paste0("Star_",star.df["Star#","value"])
        stars.list[[starname]] <- star.df
      }
      
    }else{stars.list <- list(NA)}
    
    info.name <- gsub("\\.Info\\.txt","",info.file)
    info.list[[info.name]] <- list(general = general.df, star = stars.list)
    
  }
  return(info.list)
}



##### plotting general parameters


plot_general_registration_parameters <- function(registration.list, 
                                                 plot_RB_shifts = TRUE, 
                                                 sorting = "filename",
                                                 filename_to_num = FALSE,
                                                 cutprop = NA,
                                                 cutval = NA){
  
  if(!sorting %in% c("filename","stars","score")){stop("sorting parameter must be filename, stars or score\n")}
  
  general.long <- NULL
  for(i in 1:length(registration.list)){
    general.df <- registration.list[[i]]["general"]$general
    general.df <- cbind(img = names(registration.list)[i], general.df)
    general.long <- rbind(general.long, general.df)
  }
  
  if(plot_RB_shifts == FALSE){
    rm.idx <- which(general.long$property %in% c("RedXShift", "RedYShift", "BlueXShift", "BlueYShift"))
    general.long <- general.long[-rm.idx,]
  }
  
  general.long$property <- factor(general.long$property, levels = 
                                    c("OverallQuality", "NrStars", "SkyBackground", "RedXShift", "RedYShift", "BlueXShift", "BlueYShift"))
  
  
  if(sorting == "filename"){
    general.long$img <- factor(general.long$img, levels = sort(unique(general.long$img)))
    if(filename_to_num == TRUE){
      general.long$img <- as.numeric(gsub(".+_","",general.long$img))
    }
  }
  
  if(sorting == "score"){
    score.df <- general.long[general.long$property == "OverallQuality",]
    general.long$img <- factor(general.long$img, levels = unique(score.df$img[order(score.df$value)]))
  }
  
  if(sorting == "stars"){
    score.df <- general.long[general.long$property == "NrStars",]
    general.long$img <- factor(general.long$img, levels = unique(score.df$img[order(score.df$value)]))
  }
  
  general.plot <- ggplot(general.long, aes(x=img,y=value,group=property,col=property)) +
    geom_line() +
    #geom_area(alpha = 0.2, fill="blue") +
    facet_grid(rows = vars(property), scales = "free") +
    scale_color_manual(values = c("black","black","black","red","red","blue","blue")) +
    #theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
  
  
  if(!(is.na(cutprop)|is.na(cutval))){

    
    cutoff <- data.frame(cbind(property=cutprop, value=as.numeric(cutval)), stringsAsFactors = FALSE)
    cutoff$value <- as.numeric(cutoff$value)
    
    numpass <- sum(general.long$property == cutprop & general.long$value > cutval)
    passphrase <- paste0(numpass, " out of ", sum(general.long$property == cutprop))
    
    general.plot <- general.plot +
      geom_hline(data = cutoff, aes(yintercept = value, group = property)) +
      ggtitle(passphrase)
    
    
  }
  
  
  return(general.plot)
  
}



###################################################
#################### EXECUTE ######################
###################################################

# test edit for test commit

###
output.dir <- "./test1"

if(!dir.exists(output.dir)){dir.create(output.dir)}
####################################

indir <- "~/../Desktop/NINA_captures/2023-09-05/LIGHT/"
indir <- "C:/Astrophotography/Data/NX_studio/MWPOR/"

registration.list <- regdir_to_list(indir, verbose = T)

####################################

genplot <- plot_general_registration_parameters(registration.list, 
                                                plot_RB_shifts = F, 
                                                sorting = "filename", 
                                                filename_to_num = F,
                                                cutprop = "OverallQuality",
                                                cutval = 1150)
genplot
ggsave(genplot, filename = file.path(output.dir,"QC.png"), device = "png", width = 32, height = 16)

genplot +
  geom_vline(xintercept = 133) +
  geom_vline(xintercept = 140)

names(registration.list)[133:140]
