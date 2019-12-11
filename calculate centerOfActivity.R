###########################################################################################################
###########################################################################################################
###########################################################################################################
# Zy Biesinger. 20 March 2019 
#
#
# This file defines a function that accepts a data.frame (?data.table) of glatos detections created 
# by the file 'import GLATOS detection data *.r' This file typically contains detections for 
# many individual fish, possibly of different species, potentially of different projects. 
#
# With that detection file...When a single transmission is heard on 
# multiple receivers, a single position, called the center of activity, is calculated. This center of
# activity is calculated at regular, specified time intervals.
#
#
# Notes
# Tags with temperature and depth sensors have two 'transmitter_id's corresponding to 
# transmissions for different sensor data. Because of this, transmitter_id doesn't uniquely
# identify all the transmissions for one fish: one fish give out two different transmitter_ids.
# Instead, use 'tag_serial_number" as the unique fish ID.
#
###########################################################################################################


# ZYZYZYZYZY when tag serial numbers get sent and received from glatos, this code needs to be updated
# to replace transmisison_id with animal_id as the unique identifier. See below.




###########################################################################################################
# define 'findCOA()': calculates centers of activity, one tag at a time
###########################################################################################################
findCOA = function(
  det,      # this should be the entire det data.table
  whichTag, # integer. a string containing an 'animal_id'
  showPlot = FALSE  # show the plot?
  # evaluate the following line for code development
  # det=det; whichTag=unique(det$animal_id)[1]; showPlot=FALSE;
  #
){   
try({ # error handling code which wraps around the entire body of this function
# check to see if whichTag is a valid animal_id
  if( !is.character(whichTag) ){ 
    return(paste0(whichTag, " must be a character string specifying an animal_id.")) 
  }
  
  if( !(whichTag %in% det$animal_id) ){ 
    return(paste0(whichTag, " is not found in the detection file.")) 
  }
  
# Only proceed if there are detections for whichTag, otherwise return(NA) and quit
  
if( det[animal_id == whichTag, .N] == 0 ){ return(NA) } else { 

  # det = det[ tag_serial_number == whichTag ]
  det = det[ animal_id == whichTag ]   
  
  ### Calculate 'Center of Activity'
  # ksmooth() requires a list of times at which to make a position estimate
  startTime = trunc( min(det$detection_timestamp_utc), "min" )
  stopTime = trunc( max(det$detection_timestamp_utc), "min" ) + 60
  breaks = seq(startTime, stopTime, 180)

  # detection_timestamp_utc is POSIXct.  This means the values are stored as seconds.
  # For a 15 minute window you need 60 seconds X 7.5 minutes = 450 seconds.
  # find ksmooth() points separately for lat and lon
  numSecs = 450
  coaX=ksmooth(det$detection_timestamp_utc, det$deploy_long,"normal", bandwidth=numSecs, x.points=breaks)
  coaY=ksmooth(det$detection_timestamp_utc, det$deploy_lat,"normal", bandwidth=numSecs, x.points=breaks)
  # I'm not sure calcualting depth coa works because now some of our tags give pressure and temperature
  #coaZ=ksmooth(det$detection_timestamp_utc, det$depth,"normal", bandwidth=numSecs, x.points=breaks)
  
  # combine results. rounding to 5 digits gives ~1m accuracy
  coa = data.table(
    "tag_serial_number" = det$tag_serial_number[1],
    "animal_id" = det$animal_id[1],
    "timestamp_utc"= coaX$x,
    "release_location" = det$release_location[1],
    #"transmitter_codespace" = det$transmitter_codespace[1],
    "common_name_e" =  det$common_name_e[1],
    "latitude" = round(coaY$y,5),
    "longitude" = round(coaX$y,5)
  )
  # drop any times (breaks) when there wasn't a detection
  coa = coa[!is.na(coa$latitude)]

  # diagnostic plot
  if(showPlot){
    plot(coa$timestamp_utc, coa$latitude, pch=19, main=paste0(coa$common_name_e[1], " ", whichTag))
  }
  
  # results
  return(coa)
} # end of if-else if there were tag detections 
}) # end of try() error handling
} # end calculateCOA
# END findCOA
###########################################################################################################


# ###########################################################################################################
# # find COA for one or all fish and gather all fish into one data.table
# ###########################################################################################################
# # one fish at a time
# tagIDs = unique(det$animal_id)
# oneTag = findCOA(det, tagIDs[1], showPlot = T)
# 
# # all/some fish
# wFish = head(tagIDs)
# startTime = Sys.time()
# allFish = lapply(wFish, findCOA, det=det, showPlot=F)
# Sys.time() - startTime
# 
# # bind all fish together
# centerOfActivity = rbindlist(allFish, use.names=T)
# 
# # how many positions for each species
# centerOfActivity[, .N, by = "common_name_e"]
# # END find COA for one or all fish...
# ###########################################################################################################
# 
# 
# ###########################################################################################################
# # diagnostic plots
# ###########################################################################################################
# # look at the original detection data with the center of activity over laid. They coa should
# # always fall within the cloud of the original datection data.
# 
# showPlot = TRUE
# if(showPlot){
#   i = 2
#   oneFishDet = det[ transmitter_id == tagIDs[i] ]             ##################################################### DELETE THIS LINE WHEN SUBMISSION FORMS ARE FIXED
#   oneFishCoa = centerOfActivity[transmitter_id == tagIDs[i]]         ##################################################### DELETE THIS LINE WHEN SUBMISSION FORMS ARE FIXED
# 
#   plot(oneFishDet$detection_timestamp_utc, oneFishDet$deploy_lat, pch=19, main = tagIDs[i])
#   points(oneFishCoa$dati, oneFishCoa$lat, type="l", col="red")
# } # end showPlot
# 
# 
# # END diagnostic plots
###########################################################################################################








