rsdg <- function(){
  
  ## Clean Memory
  rm(list=ls())

  .GlobalEnv$filename <- file.choose()


  
  # load packages
  library(utils)
  library(stats)
  library(dplyr)
  library(reshape2)
  library(tidyr)
  library(tools)
  
  tmpfolder <- tempfile()
  workfolder <- getwd()
  
  ## extract just the child
  unzip(file.path(filename), exdir=tmpfolder)
  ff <- file.path(filename)
  index <- unzip(ff,list=TRUE)
  ## Read the file
  temp = list.files(path = tmpfolder, pattern="*.txt")
  
  
  for(i in 1:length(temp)){
    assign(paste0("raw_", sub("\\..*", "", temp[i]) ), read.csv(paste0(tmpfolder, '/',temp[i]), skip = 0, header=T, sep="\t", stringsAsFactors=F), envir = .GlobalEnv)
  }
  
  # first check sampling protocols for special characters
  raw_event$samplingProtocol <- gsub("[[:punct:]]", "_", raw_event$samplingProtocol)
  
  
  # find events with occurrences
  eventswithoccurr <- subset(raw_event, (id %in% raw_occurrence$eventID))
  
  # find events without occurrences
  eventswithoutoccurr <- subset(raw_event, !(id %in% raw_occurrence$eventID))
  
  
  
  # join in sample protocol information to event measurements or facts table
  extendedmeasurementorfact2 <-  merge(x = raw_extendedmeasurementorfact, y = raw_event[,c("id","samplingProtocol")], by.x = "id", by.y= "id")
  
  # join in sample protocol information to occurrence table
  occurrence2 <-  merge(x = raw_occurrence, y = raw_event[,c("id","samplingProtocol")], by.x = "id", by.y= "id")
  
  
  
  # then check sampling protocols, basis for iteration
  raw_event$samplingProtocol <- as.factor(as.character(raw_event$samplingProtocol))
  dfsamplingsprotocols <- data.frame(samplingsprotocols = levels(raw_event$samplingProtocol), stringsAsFactors=T)
  
  
  
  
  
  # OUTPUT TABLES
  
  
  # dcast for mof on occurrence
  # then check sampling protocols, basis for iteration
  eventswithoccurr$samplingProtocol <- as.factor(as.character(eventswithoccurr$samplingProtocol))
  dfoccurrsamplingsprotocols <- data.frame(samplingsprotocols = levels(eventswithoccurr$samplingProtocol), stringsAsFactors=T)
  for(i in seq_along(levels(dfoccurrsamplingsprotocols$samplingsprotocols))){
    #
    assign(paste0("df_", dfoccurrsamplingsprotocols$samplingsprotocols[i]), dcast(subset(extendedmeasurementorfact2, samplingProtocol == dfoccurrsamplingsprotocols$samplingsprotocols[i]),occurrenceID~measurementType,value.var = "measurementValue"), envir = .GlobalEnv )
    # variable description
    assign(paste0("df_", dfoccurrsamplingsprotocols$samplingsprotocols[i],"_desc"), data.frame(subset(extendedmeasurementorfact2, samplingProtocol == dfoccurrsamplingsprotocols$samplingsprotocols[i]) %>% select(measurementType,measurementAccuracy,measurementUnit,measurementMethod) %>% unique()), envir = .GlobalEnv )
  }
  
  
  # dcast for mof on event
  # then check sampling protocols, basis for iteration
  eventswithoutoccurr$samplingProtocol <- as.factor(as.character(eventswithoutoccurr$samplingProtocol))
  dfeventsamplingsprotocols <- data.frame(samplingsprotocols = levels(eventswithoutoccurr$samplingProtocol), stringsAsFactors=T)
  for(i in seq_along(levels(dfeventsamplingsprotocols$samplingsprotocols))){
    #
    assign(paste0("df_", dfeventsamplingsprotocols$samplingsprotocols[i]), dcast(subset(extendedmeasurementorfact2, samplingProtocol == dfeventsamplingsprotocols$samplingsprotocols[i]),id~measurementType,value.var = "measurementValue"), envir = .GlobalEnv )
    # variable description
    assign(paste0("df_", dfeventsamplingsprotocols$samplingsprotocols[i],"_desc"), data.frame(subset(extendedmeasurementorfact2, samplingProtocol == dfeventsamplingsprotocols$samplingsprotocols[i]) %>% select(measurementType,measurementAccuracy,measurementUnit,measurementMethod) %>% unique()), envir = .GlobalEnv )
  }
    
  # remove supporting data frames
  rm(eventswithoccurr)
  rm(eventswithoutoccurr)
  rm(extendedmeasurementorfact2)
  rm(occurrence2)
  rm(dfoccurrsamplingsprotocols)
  rm(dfeventsamplingsprotocols)
  rm(index)
  
  # remove supporting vectors
  rm(i)
  rm(ff)
  rm(temp)
}
