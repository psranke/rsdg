enterFileName <- function(){
  options(myproject.filename = readline(prompt="Enter file name: "))
  return(filename)
}

rsdg <- function(filename){

  ## Clean Memory
  rm(list=ls())

  # load packages
  library(utils)
  library(stats)
  library(dplyr)
  library(reshape2)
  library(tidyr)
  library(tools)

  tmpd <- getwd()
  #filename <-'dwca-great_reed_warbler-v1.5.zip'
  #filename <- getOption("myproject.filename")
  filename <- enterFileName()
  #paste0(tmpd,'/',filename)

  tmpfolder <- tempfile()

  ## extract just the child
  unzip(paste0(tmpd,'/',filename), exdir=tmpfolder)
  ff <- file.path(paste0(tmpd,'/',filename))
  index <- unzip(ff,list=TRUE)
  ## Read the file
  temp = list.files(path = tmpfolder, pattern="*.txt")
  #temp



  for(i in 1:length(temp)){
    assign(paste0("raw_", sub("\\..*", "", temp[i]) ), read.csv(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[i]), skip = 0, header=T, sep="\t", stringsAsFactors=F), envir = .GlobalEnv)
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


  # for(i in seq_along(levels(dfsamplingsprotocols$samplingsprotocols))){
  #   # split events and mof by sampling protocols
  #   assign(paste0("event_", dfsamplingsprotocols$samplingsprotocols[i]), data.frame(subset(raw_event, samplingProtocol == dfsamplingsprotocols$samplingsprotocols[i])))
  #   assign(paste0("mof_", dfsamplingsprotocols$samplingsprotocols[i]), data.frame(subset(extendedmeasurementorfact2, samplingProtocol == dfsamplingsprotocols$samplingsprotocols[i])))
  #
  #
  #   # finding occurrences based on sample protocols
  #   assign(paste0("occurr_", dfsamplingsprotocols$samplingsprotocols[i]), data.frame(subset(occurrence2, (id %in% occurrence2$eventID) & (occurrence2$samplingProtocol.y %in% dfsamplingsprotocols$samplingsprotocols[i]))))
  #
  # }


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












  # remove subsets
  # for(i in seq_along(levels(dfsamplingsprotocols$samplingsprotocols))){
  #   rm(list = listtmp <- paste0("event_", dfsamplingsprotocols$samplingsprotocols[i]) )
  #   rm(list = listtmp <- paste0("mof_", dfsamplingsprotocols$samplingsprotocols[i]) )
  #   rm(list = listtmp <- paste0("occurr_", dfsamplingsprotocols$samplingsprotocols[i]) )
  # }

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
  #rm(listtmp)
  rm(temp)

}

