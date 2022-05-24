



#### this is a script to split DSS file lists into chunks




#######

DSS_full_list.file <- "~/../Desktop/SharpCap Captures/2022-05-19/allsky/DSSfilelists/full.txt"
output.dir <- "~/../Desktop/SharpCap Captures/2022-05-19/allsky/DSSsplitlists/"

drop_lights <- c("allsky_00219")

stacksize <- 20


#######

DSS_full_list <- readLines(DSS_full_list.file)
if(!dir.exists(output.dir)){dir.create(output.dir)}


### Count the number of light frames

light.idx <- grep("\tlight\t", DSS_full_list)
cat("Detected",length(light.idx),"light frames\n")


### Filter the drop lights

DSS_filtered <- DSS_full_list

for(droplight in drop_lights){
  rm.idx <- grep(droplight, DSS_filtered)
  DSS_filtered <- DSS_filtered[-rm.idx]
}

light.idx <- grep("\tlight\t", DSS_filtered)
cat("Detected",length(light.idx),"light frames after applying drop\n")



###

number_of_stacks.dec <- length(light.idx)/stacksize
cat(number_of_stacks.dec,"stacks will be made of",stacksize,"subs each\n")
number_of_stacks <- ceiling(number_of_stacks.dec)

###

DSS_batches <- list()

for(i in 1:(number_of_stacks-1)){
  
  start <- (i-1)*stacksize + 1
  stop <- start + 19
  
  cat(start,"\t",stop,"\n")
  
  batch.idx <- light.idx[start:stop]
  
  ### remove all non batch lights from the full
  rm.idx <- light.idx[(!light.idx %in% batch.idx)]
  DSS_batches[[i]] <- DSS_filtered[-rm.idx]
  
}

### last stack, might be smaller than stacksize so needs another stop

for(i in number_of_stacks){
  
  start <- (i-1)*stacksize + 1
  stop <- length(light.idx)
  
  batch.idx <- light.idx[start:stop]
  
  ### remove all non batch lights from the full
  rm.idx <- light.idx[(!light.idx %in% batch.idx)]
  DSS_batches[[i]] <- DSS_full_list[-rm.idx]
  
}


##################

for(i in 1:length(DSS_batches)){
  
  fileConn<-file(file.path(output.dir,paste0("stack_",sprintf("%05d", i),".txt")))
  writeLines(DSS_batches[[i]], fileConn)
  close(fileConn)
  
  
}



