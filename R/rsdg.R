## Clean Memory
rm(list=ls())

# load packages
library(utils)
library(stats)
library(dplyr)
library(reshape2)
library(tidyr)
library(tools)


# read from zip-folder
#data <- read.table(unz("/home/peterxps/Downloads", "Sales.dat"), nrows=10, header=T, quote="\"", sep=",")

# load functions
myfun <- function(x){x + 7}
myfun(2)

#zipdf <- unzip("/home/peterxps/Downloads/dwca-great_reed_warbler-v1.4.zip", list = TRUE)

# the following line assuming the archive has only a single file


#x <- unzip("/home/peterxps/Downloads/dwca-great_reed_warbler-v1.4.zip")




## Set path for your working location
#setwd("D:/blah")

## unzipped it the file
#y <- unzip("/home/peterxps/Downloads/dwca-great_reed_warbler-v1.4.zip")

## Check file in the zipped file
#list.files("/home/peterxps/Downloads/dwca-great_reed_warbler-v1.4.zip")




tmpd <- '/home/peterxps/Dropbox/papers/Ranke_msc_GRW_habitat_preference'
filename <-'dwca-great_reed_warbler-v1.5.zip'
paste0(tmpd,'/',filename)

tmpfolder <- tempfile()

## extract just the child
unzip(paste0(tmpd,'/',filename), exdir=tmpfolder)
ff <- file.path(paste0(tmpd,'/',filename))
index <- unzip(ff,list=TRUE)
## Read the file
temp = list.files(path = tmpfolder, pattern="*.txt")
temp
#unlink(ff) # deleting???

# files is deleted...

# temp files   CRAN requirement
#file <- tempfile()


#need for loop here
#for(i in 1:length(temp)){
event <- read.table(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[1]), skip = 0, header=T, sep="\t")
extendedmeasurementorfact <- read.table(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[2]), skip = 0, header=T, sep="\t")
materialsample <- read.table(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[3]), skip = 0, header=T, sep="\t")
occurrence <- read.table(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[4]), skip = 0, header=T, sep="\t")
resourcerelationship <- read.table(paste0(tmpd,'/',file_path_sans_ext(filename), '/',temp[5]), skip = 0, header=T, sep="\t")
#}

# find events with occurrences
eventswithoccurr <- subset(event, (id %in% occurrence$eventID))

# find events without occurrences
eventswithoutoccurr <- subset(event, !(id %in% occurrence$eventID))

# join in sample protocol information to event measurements or facts table
extendedmeasurementorfact2 <-  merge(x = extendedmeasurementorfact, y = event[,c("id","samplingProtocol")], by.x = "id", by.y= "id")

# join in sample protocol information to occurrence table
occurrence2 <-  merge(x = occurrence, y = event[,c("id","samplingProtocol")], by.x = "id", by.y= "id")


# first check sampling protocols, basis for iteration
df <- data.frame(samplingsprotocols = levels(event$samplingProtocol))

for(i in seq_along(levels(df$samplingsprotocols))){
  # split events and mof by sampling protocols
  assign(paste0("event_", df$samplingsprotocols[i]), data.frame(subset(event, samplingProtocol == df$samplingsprotocols[i])))
  assign(paste0("mof_", df$samplingsprotocols[i]), data.frame(subset(extendedmeasurementorfact2, samplingProtocol == df$samplingsprotocols[i])))

  
  # finding occurrences based on sample protocols
  tab <- paste0("event_", df$samplingsprotocols[i])
  assign(paste0("occurr_", df$samplingsprotocols[i]), data.frame(subset(occurrence2, (id %in% occurrence2$eventID) & (occurrence2$samplingProtocol.y %in% df$samplingsprotocols[i]))))

}


# dcast for mof on occurrence

mof_capture_recapture_adults_wide <- dcast(mof_capture_recapture_adults,occurrenceID~measurementType,value.var = "measurementValue")
write.table(mof_capture_recapture_adults_wide,"mof_capture_recapture_adults_wide.txt", quote = F, sep = ";",row.names = F, col.names = T)

mof_capture_recapture_nestlings_wide <- dcast(mof_capture_recapture_nestlings,occurrenceID~measurementType,value.var = "measurementValue")
write.table(mof_capture_recapture_nestlings_wide,"mof_capture_recapture_nestlings_wide.txt", quote = F, sep = ";",row.names = F, col.names = T)



# dcast for mof on event

# issues with special characters in samplingProtocol
mof_habitat_measurement <- `mof_habitat-measurement`
mof_habitat_measurement_wide <- dcast(mof_habitat_measurement,id~measurementType,value.var = "measurementValue")
write.table(mof_habitat_measurement_wide,"mof_habitat_measurement_wide.txt", quote = F, sep = ";",row.names = F, col.names = T)

# issues with special characters in samplingProtocol
mof_tree_measurement <- `mof_tree-measurement`
mof_tree_measurement_wide <- dcast(mof_tree_measurement,id~measurementType,value.var = "measurementValue")
write.table(mof_tree_measurement_wide,"mof_tree_measurement_wide.txt", quote = F, sep = ";",row.names = F, col.names = T)




